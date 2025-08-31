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

(def strm1 (byte-array (+ 1 (* 7 SectorSize)) (byte \A)))
(def strm2 (byte-array (* 8 SectorSize) (byte \B)))

(def ENDOFCHAIN 0xFFFFFFFE)
(def FREESEC 0xFFFFFFFF)
(def FATSEC 0xFFFFFFFD)

(defn calc-padding [size]
  (let [m (mod size SectorSize)]
    (if (= m 0)
      0
      (- SectorSize m))))

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

(defrecord Node [name child left right type size start])

(defn make-nodes-path [path size start]
  (let [path* (string/split path #"/")
        head (map #(map->Node {:name % :type :storage}) (drop-last path*))
        tail (map->Node {:name (last path*) :type :stream :size size :start start})]
    (concat head (list tail))))
(defn add-nodes-path [directory path])

(defn make-directory [items]
  (reduce (fn [directory [path size start]]
            (let [path* (make-nodes-path path size start)]
              (add-nodes-path directory path*)))
          [(map->Node {:name "Root Entry" :type :storage})] items))

(defn make-cfb [streams]
  (let [[starts proto-fat] (make-proto-fat (map (comp count last) streams))
        directory (make-directory (map (fn [[path stream] start]
                                         [path (count stream) start]) streams starts))
        start-directory (count proto-fat)]
    [directory]))

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

(defn int-to-bytes [^long n]
  (let [^ByteBuffer buffer (ByteBuffer/allocate 4)]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (.putInt buffer n)
    (.array buffer)))

(defn write-to-file [path arr]
  (with-open [out (io/output-stream path)]
    (.write out arr)))

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

(def num-difat-sector-in-header 109)
(defn make-difat [start length]
  {:pre [(<= length num-difat-sector-in-header)]}
  nil)
