(ns cfb.parser
  (:import (java.nio.file Paths StandardOpenOption OpenOption)
           (java.nio.channels FileChannel)
           (java.nio ByteBuffer ByteOrder)))

(def SectorSize 512)
6
(defn open-cfb [^String path]
  (let [p (Paths/get path (into-array String []))
        f (FileChannel/open p (into-array OpenOption [StandardOpenOption/READ]))] f))
