(ns cfb.parser
  (:import (java.nio.file Paths StandardOpenOption OpenOption)
           (java.nio.channels FileChannel)
           (java.nio ByteBuffer ByteOrder)))

(def SectorSize 512)
(def HeaderSize 512)

(defn read-u16 [^ByteBuffer buffer]
  (-> buffer
      .getShort
      (bit-and 0xFFFF)))

(defn read-u32 [^ByteBuffer buffer]
  (-> buffer
      .getInt
      (bit-and 0xFFFFFFFF)))

(defn shift-position [^ByteBuffer buffer n]
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
    (shift-position buffer 16) ; Skip CLSID
    (let [header (apply hash-map [:minor-version (read-u16 buffer)
                                  :major-version (read-u16 buffer)
                                  :byte-order (read-u16 buffer)
                                  :sector-shift (read-u16 buffer)
                                  :mini-sector-shift (read-u16 buffer)
                                  :num-fat-sector (do ; skip reserved and numdirectory sector
                                                    (shift-position buffer 10)
                                                    (read-u32 buffer))
                                  :start-directory-sector (read-u32 buffer)
                                  :mini-stream-cutoff (do ; skip transaction signature
                                                        (shift-position buffer 4)
                                                        (read-u32 buffer))
                                  :start-minifat (read-u32 buffer)
                                  :num-minifat-sector (read-u32 buffer)
                                  :start-difat-sector (read-u32 buffer)
                                  :num-difat-sector (read-u32 buffer)])]

      (assert (= (:minor-version header) 0x003E))
      (assert (= (:major-version header) 0x0003))
      (assert (= (:byte-order header) 0xFFFE))
      (assert (= (:sector-shift header) 0x0009))
      header)))

(defn open-cfb [^String path]
  (let [p (Paths/get path (into-array String []))
        f (FileChannel/open p (into-array OpenOption [StandardOpenOption/READ]))]
    (read-header f)))
