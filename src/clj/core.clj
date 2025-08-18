(ns clj.core
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
          :else nil)))))

(defn insert-in-storage [directory storage-id node]
  (let [storage (nth directory storage-id)
        root-id (:child storage)]
    (if (nil? root-id)
      (add-node directory storage-id :child node)
      (insert-in-tree directory root-id node))))

(def directory [(map->Node {:name "Root" :type :storage :child 1})      ;0
                (map->Node {:name "B" :type :storage :left 2 :right 3}) ;1
                (map->Node {:name "A" :type :storage})                  ;2
                (map->Node {:name "C" :type :storage})])                ;3
(def nn (map->Node {:name "D" :type :storage}))

(def edir [(map->Node {:name "Root" :type :storage})])
(def _edirc (insert-in-storage edir 0 nn))
(def edirc (nth _edirc 0))

(def nn2 (map->Node {:name "B" :type :storage}))

(defn add-path [directory path]
  (reduce (fn [[directory storage-id] path-comp]
            (let [node (map->Node {:name path-comp :type :storage})]
              (insert-in-storage directory storage-id node)))
          [directory 0] path))
