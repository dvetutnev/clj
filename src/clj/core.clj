(ns clj.core
  (:import (java.nio ByteBuffer ByteOrder))
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defrecord Node [name child left right type])

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
  (reduce (fn [[directory storage-id] node]
            (insert-in-storage directory storage-id node))
          [directory 0] path))

(defn int-to-bytes [^long n]
  (let [^ByteBuffer buffer (ByteBuffer/allocate 4)]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (.putInt buffer n)
    (.array buffer)))

(defn gen [n]
  (cons n (lazy-seq (gen (inc n)))))

(def ENDOFCHAIN 0xFFFFFFFE)

(defn make-fat-chain [start length]
  (let [head (take (dec length) (gen (inc start)))]
    (concat head [ENDOFCHAIN])))

(defn make-proto-fat [lengths]
  (reduce (fn [acc length]
            (let [chain (make-fat-chain (count acc) length)]
              (concat acc chain)))
          () lengths))
