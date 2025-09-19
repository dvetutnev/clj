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
    (let [header (apply hash-map [:minor-version (.getShort buffer)
                                  :major-version (.getShort buffer)
                                  :byte-order (.getShort buffer)
                                  :sector-shift (.getShort buffer)
                                  :mini-sector-shift (.getShort buffer)
                                  :num-fat-sector (do ; skip reserved and numdirectory sector
                                                    (.position buffer (+ (.position buffer) 10))
                                                    (.getInt buffer))
                                  :start-directory (.getInt buffer)
                                  :mini-stream-cutoff (do ; skip transaction signature
                                                        (.position buffer (+ (.position buffer) 4))
                                                        (.getInt buffer))
                                  :start-minifat (.getInt buffer)])]

      (assert (= (:byte-order header) (unchecked-short 0xFFFE)))
      (assert (= (:minor-version header) 0x003E))
      (assert (= (:major-version header) 0x0003))
      (assert (= (:sector-shift header) 0x0009))
      #_(assert (= (:mini-sector-shift header) 0x0006))
      header)))

(defn open-cfb [^String path]
  (let [p (Paths/get path (into-array String []))
        f (FileChannel/open p (into-array OpenOption [StandardOpenOption/READ]))]
    (read-header f)))
