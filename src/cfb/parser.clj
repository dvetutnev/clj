(ns cfb.parser
  (:import (java.nio.file Paths StandardOpenOption OpenOption)
           (java.nio.channels FileChannel)
           (java.nio ByteBuffer ByteOrder)))

(def SectorSize 512)

(defn read-header [f]
  (let [buffer (ByteBuffer/allocate SectorSize)
        signature (byte-array 8)]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (.read f buffer)
    (.rewind buffer)
    (.get buffer signature)
    (assert (java.util.Arrays/equals signature
                                     (byte-array [0xD0 0xCF 0x11 0xE0 0xA1 0xB1 0x1A 0xE1])))
    (.position buffer (+ (.position buffer) 16)) ; Skip CLSID
    (let [header {:minor-version (.getShort buffer)
                  :major-version (.getShort buffer)
                  :byte-order (.getShort buffer)}]
      (assert (= (:byte-order header) (unchecked-short 0xFFFE)))
      header)))

(defn open-cfb [^String path]
  (let [p (Paths/get path (into-array String []))
        f (FileChannel/open p (into-array OpenOption [StandardOpenOption/READ]))]
    (read-header f)))
