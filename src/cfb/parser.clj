(ns cfb.parser
  (:import (java.nio.file Paths StandardOpenOption OpenOption)
           (java.nio.channels FileChannel)
           (java.nio ByteBuffer ByteOrder)))

(def SectorSize 512)
(def HeaderSize 512)
(def u32size 4)

(defn read-u8! [^ByteBuffer buffer]
  (-> buffer
      .get
      (bit-and 0xFF)))

(defn read-u16! [^ByteBuffer buffer]
  (-> buffer
      .getShort
      (bit-and 0xFFFF)))

(defn read-u32! [^ByteBuffer buffer]
  (-> buffer
      .getInt
      (bit-and 0xFFFFFFFF)))

(defn shift-position! [^ByteBuffer buffer n]
  (.position buffer (+ (.position buffer) n)))

(defn read-header [f]
  (let [buffer (ByteBuffer/allocate HeaderSize)
        signature (byte-array 8)]
    (.read f buffer)
    (doto buffer
      (.order ByteOrder/LITTLE_ENDIAN)
      (.rewind)
      (.get signature))
    (assert (java.util.Arrays/equals signature
                                     (byte-array [0xD0 0xCF 0x11 0xE0 0xA1 0xB1 0x1A 0xE1])))
    (shift-position! buffer 16) ; Skip CLSID
    (let [header (apply hash-map [:minor-version (read-u16! buffer)
                                  :major-version (read-u16! buffer)
                                  :byte-order (read-u16! buffer)
                                  :sector-shift (read-u16! buffer)
                                  :mini-sector-shift (read-u16! buffer)
                                  :num-fat-sector (do ; skip reserved and numdirectory sector
                                                    (shift-position! buffer 10)
                                                    (read-u32! buffer))
                                  :start-directory-sector (read-u32! buffer)
                                  :mini-stream-cutoff (do ; skip transaction signature
                                                        (shift-position! buffer 4)
                                                        (read-u32! buffer))
                                  :start-minifat (read-u32! buffer)
                                  :num-minifat-sector (read-u32! buffer)
                                  :start-difat-sector (read-u32! buffer)
                                  :num-difat-sector (read-u32! buffer)])

          difat (let [difat (transient [])]
                  (doseq [_ (range (min (:num-fat-sector header) 109))]
                    (conj! difat (read-u32! buffer)))
                  (persistent! difat))]

      (assert (= (:minor-version header) 0x003E))
      (assert (= (:major-version header) 0x0003))
      (assert (= (:byte-order header) 0xFFFE))
      (assert (= (:sector-shift header) 0x0009))
      (assoc header :difat difat))))

(defn sector->offset [n]
  (* (+ n 1) SectorSize))

(defn read-fat [f difat]
  (let [buffer (ByteBuffer/allocate SectorSize)
        fat (transient [])]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (doseq [n difat]
      (.position f (sector->offset n))
      (.clear buffer)
      (.read f buffer)
      (.rewind buffer)
      (doseq [_ (range (/ SectorSize u32size))]
        (conj! fat (read-u32! buffer))))
    (persistent! fat)))

(defn read-directory-stream [fat start]
  (loop [sector-id start]))

(defn read-directory-entry-name! [^ByteBuffer buffer]
  (let [name (byte-array 64 (byte 0x00))]
    (.position buffer 0)
    (.get buffer name)
    (let [len (read-u16! buffer)]
      (String. name 0 (- len 2) "UTF-16LE"))))

(defn read-directory-entry-type! [^ByteBuffer buffer]
  (let [type (read-u8! buffer)]
    (case type
      1 :storage
      2 :stream
      5 :root)))

(defn read-directory-sector [f sector]
  (let [buffer (ByteBuffer/allocate 128)
        entries (transient [])]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (.position f (sector->offset sector))
    (doseq [_ (range 4)]
      (.clear buffer)
      (.read f buffer)
      (.rewind buffer)
      (let [entry (apply hash-map [:name (read-directory-entry-name! buffer)
                                   :type (read-directory-entry-type! buffer)])]
        (conj! entries entry)))
    (persistent! entries)))

(defn open-cfb [^String path]
  (let [p (Paths/get path (into-array String []))
        f (FileChannel/open p (into-array OpenOption [StandardOpenOption/READ]))
        header (read-header f)
        fat (read-fat f (:difat header))
        directory-stream (read-directory-stream fat (:start-directory-sector header))
        dir-sector (read-directory-sector f (:start-directory-sector header))]
    (assoc header
           :fat fat
           ;:directory directory-stream
           :dir-sector dir-sector)))
