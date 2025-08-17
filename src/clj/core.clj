(ns clj.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defrecord Node [value child left right])

(defn add-node [directory parent-id direction node]
  (let [new-id (count directory)
        parent-node (nth directory parent-id)
        upd-parent-node (assoc parent-node direction new-id)]
    [(conj (assoc directory parent-id upd-parent-node) node)
     new-id]))

(defn insert-in-tree [directory root-id nv]
  (loop [root-id root-id
         parent-id nil
         direction nil]
    (if (nil? root-id)
      (let [new-node (map->Node {:value nv})]
        (add-node directory parent-id direction new-node))
      (let [{:keys [value left right]} (directory root-id)
            cmp-res (compare nv value)]
        (cond
          (< cmp-res 0) (recur left root-id :left)
          (> cmp-res 0) (recur right root-id :right)
          :else nil)))))

(defn insert-in-storage [directory storage-id nv]
  (let [storage (nth directory storage-id)
        root-id (:child storage)]
    (if (nil? root-id)
      (let [new-node (map->Node {:value nv})]
        (add-node directory storage-id :child new-node))
      (insert-in-tree directory root-id nv))))

(def directory [(->Node "Root" 1 nil nil) ;0
                (->Node "B" nil 2 3)      ;1
                (->Node "A" nil nil nil)  ;2
                (->Node "C" nil nil nil)]);3
(def nv "D")

(def edir [(->Node "Root" nil nil nil)])
(def _edirc (insert-in-storage edir 0 nv))
(def edirc (nth _edirc 0))

(def nv2 "B")

(defn fred [dir storage-id nv]
  [(str dir nv)
   (inc storage-id)])

(defn add-path [path]
  (reduce (fn [[directory storage-id] path-comp]
            (fred directory storage-id path-comp))
          ["Root" 42] path))
