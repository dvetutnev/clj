(ns clj.core
  (:require [clojure.math :as math]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import (java.nio ByteBuffer ByteOrder))
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def SectorSize 512)

(def strm1 (byte-array (+ 1 (* 1 SectorSize)) (byte \A)))
(def strm2 (byte-array (* 2 SectorSize) (byte \B)))

(def ENDOFCHAIN 0xFFFFFFFE)
(def FREESEC 0xFFFFFFFF)
(def FATSEC 0xFFFFFFFD)

(defn calc-num-sector
  ([length] (calc-num-sector length 1))
  ([length entry-size]
   (let [total-size (* length entry-size)
         num-full-sector (math/floor-div total-size SectorSize)]
     (if (= (mod total-size SectorSize) 0)
       num-full-sector
       (inc num-full-sector)))))

(defn make-fat-chain [start length]
  (let [start (inc start)
        end (+ start (dec length))]
    (conj (vec (range start end)) ENDOFCHAIN)))

(defn make-proto-fat [sizes]
  (reduce (fn [[starts fat] size]
            (let [starts (conj starts (count fat))
                  chain (make-fat-chain (count fat) (calc-num-sector size))
                  fat (concat fat chain)]
              [starts fat]))
          [[] []] sizes))

(def u32size 4)
(def fat-entry-peer-sector (/ SectorSize u32size))

(defn make-fat [proto-fat]
  (loop [num-fat-sector (calc-num-sector (* (count proto-fat) u32size))]
    (if (> (+ num-fat-sector (count proto-fat))
           (* num-fat-sector fat-entry-peer-sector))
      (recur (inc num-fat-sector))
      (let [num-pad-entry (- (* num-fat-sector fat-entry-peer-sector)
                             (+ num-fat-sector (count proto-fat)))
            start (+ (count proto-fat) num-pad-entry)]
        [(concat proto-fat
                 (long-array num-pad-entry FREESEC)
                 (long-array num-fat-sector FATSEC))
         start num-fat-sector]))))

(def num-difat-entry-in-header 109)
(defn make-difat [start length]
  {:pre [(<= length num-difat-entry-in-header)]}
  (concat (range start (+ start length))
          (long-array (- num-difat-entry-in-header length) FREESEC)))

(defn serialize-header [header]
  (let [^ByteBuffer buffer (ByteBuffer/allocate SectorSize)]
    (-> buffer
        (.order ByteOrder/LITTLE_ENDIAN)
        (.put (byte-array [0xD0 0xCF 0x11 0xE0 0xA1 0xB1 0x1A 0xE1])) ; Signature
        (.put (byte-array 16 (byte 0))) ; CLSID
        (.putShort 0x003E) ; Minor version
        (.putShort 0x0003) ; Major version
        (.putShort (unchecked-short 0xFFFE)) ; Byte order
        (.putShort 0x0009) ; Sector size
        (.putShort 0x0006) ; Mini stream sector size
        (.putShort 0) ; Reserved
        (.putInt 0)   ; Reserved
        (.putInt 0)   ; Number of directory sector (not used for version 3)
        (.putInt (:num-fat-sector header))  ; Number of FAT sector
        (.putInt (:start-directory header)) ; Directory starting sector location
        (.putInt 0) ; Transaction signature
        (.putInt 0) ; Mini stream cutoff
        (.putInt (unchecked-int ENDOFCHAIN)) ; Mini FAT start sector location
        (.putInt 0) ; Number of mini FAT sector
        (.putInt (unchecked-int ENDOFCHAIN)) ; DIFAT start sector location
        (.putInt 0)) ; Number of DIFAT sector
    (doseq [entry (:difat header)]
      (.putInt buffer (unchecked-int entry)))
    (.array buffer)))

(defn calc-padding [size]
  (let [m (mod size SectorSize)]
    (if (= m 0)
      0
      (- SectorSize m))))

(declare make-directory)

(defn make-cfb [streams]
  (let [[starts strm-proto-fat] (make-proto-fat (map (comp count last) streams))
        directory (make-directory (map (fn [[path stream] start]
                                         [path (count stream) start]) streams starts))
        start-directory (count strm-proto-fat)
        num-directory-sector (calc-num-sector (count directory) 128)
        proto-fat (concat strm-proto-fat
                          (make-fat-chain start-directory num-directory-sector))
        [fat start-fat num-fat-sector] (make-fat proto-fat)
        difat (make-difat start-fat num-fat-sector)
        header {:num-fat-sector num-fat-sector
                :start-directory start-directory
                :difat difat}]
    (with-open [out (io/output-stream "test.bin")]
      (.write out (serialize-header header))
      (doseq [[_ content] streams]
        (.write out content)
        (.write out (byte-array (calc-padding (count content)) (byte 0)))))))

(defrecord Node [name child left right type size start])

(defn add-node [directory parent-id direction node]
  (let [new-id (count directory)
        parent-node (nth directory parent-id)
        upd-parent-node (assoc parent-node direction new-id)]
    [(conj (assoc directory parent-id upd-parent-node) node)
     new-id]))

(defn insert-in-tree [directory root-id node]
  (loop [root-id root-id
         parent-id nil
         direction nil]
    (if (nil? root-id)
      (add-node directory parent-id direction node)
      (let [{:keys [name left right]} (directory root-id)
            cmp-res (compare (:name node) name)]
        (cond
          (< cmp-res 0) (recur left root-id :left)
          (> cmp-res 0) (recur right root-id :right)
          :else [directory root-id])))))

(defn insert-in-storage [directory storage-id node]
  (let [storage (nth directory storage-id)
        root-id (:child storage)]
    (if (nil? root-id)
      (add-node directory storage-id :child node)
      (insert-in-tree directory root-id node))))

(defn add-nodes-path [directory path]
  (let [[directory _] (reduce (fn [[directory storage-id] node]
                                (insert-in-storage directory storage-id node))
                              [directory 0] path)]
    directory))

(defn make-nodes-path [path size start]
  (let [path* (string/split path #"/")
        head (map #(map->Node {:name % :type :storage}) (drop-last path*))
        tail (map->Node {:name (last path*) :type :stream :size size :start start})]
    (concat head (list tail))))

(defn make-directory [items]
  (reduce (fn [directory [path size start]]
            (let [path* (make-nodes-path path size start)]
              (add-nodes-path directory path*)))
          [(map->Node {:name "Root Entry" :type :storage})] items))
