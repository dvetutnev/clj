(ns clj.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn add-node [directory parent-id direction node]
  (let [new-id (count directory)
        parent-node (nth directory parent-id)
        upd-parent-node (assoc parent-node direction new-id)]
    (conj (assoc directory parent-id upd-parent-node) node)))

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

(defrecord Node [value child left right])
(def directory [(->Node "Root" 1 nil nil) ;0
                (->Node "B" nil 2 3)      ;1
                (->Node "A" nil nil nil)  ;2
                (->Node "C" nil nil nil)]);3
(def nv "D")

(def edir [(->Node "Root" nil nil nil)])
(def edirc (insert-in-storage edir 0 nv))
(def nv2 "B")
