(ns clj.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defrecord Node [value child left right])
(def directory [(->Node "Root" 1 nil nil) ;0
                (->Node "B" nil 2 3)      ;1
                (->Node "A" nil nil nil)  ;2
                (->Node "C" nil nil nil)]);3
(def nv "D")

(defn insert-in-tree [directory root-id nv]
  (loop [root-id root-id
         parent-id nil
         direction nil]
    (if (nil? root-id)
      (let [new-key (count directory)
            new-node (map->Node {:value nv})
            upd-parent-node (assoc (nth directory parent-id) direction new-key)]
        (assoc (assoc directory parent-id upd-parent-node) new-key new-node))
      (let [{:keys [value left right]} (directory root-id)
            cmp-res (compare nv value)]
        (cond
          (< cmp-res 0) (recur left root-id :left)
          (> cmp-res 0) (recur right root-id :right)
          :else nil)))))

(def edir [(->Node "Root" nil nil nil)])

(defn insert-in-storage [directory storage-id nv]
  (let [storage (nth directory storage-id)
        root-id (:child storage)]
    (if (nil? root-id)
      (let [new-key (count directory)
            new-node (map->Node {:value nv})
            upd-storage-node (assoc (nth directory storage-id) :child new-key)]
        (assoc (assoc directory storage-id upd-storage-node) new-key new-node))
      (insert-in-tree directory root-id nv))))
