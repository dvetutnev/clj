(ns cfb.parser
  (:import (java.nio.file Paths StandardOpenOption OpenOption)
           (java.nio.channels FileChannel)
           (java.nio ByteBuffer ByteOrder)))

(def SectorSize 512)

(defn read-header [f]
  (let [buffer (ByteBuffer/allocate SectorSize)
        header (byte-array 8)]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (.read f buffer)
    (.rewind buffer)
    {:minor-version (do
                      (.get buffer header)
                      (.get buffer (byte-array 16)) ; CLSID
                      (.getShort buffer))}))

(defn open-cfb [^String path]
  (let [p (Paths/get path (into-array String []))
        f (FileChannel/open p (into-array OpenOption [StandardOpenOption/READ]))]
    (read-header f)))
