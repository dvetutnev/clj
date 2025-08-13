(ns clj.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defrecord Node [value child left right])
(def tree {0 (Node. "Root" 1 nil nil)
           1 (Node. "B" nil 2 3)
           4 (Node. "A" nil nil nil)
           3 (Node. "C" nil nil nil)})
(def nv "D")
(def new-node (map->Node {:value "D"}))

(loop [root 1]
  (if (nil? root)
    (let [new-key (inc (last (sort (keys tree))))]
      (assoc tree  new-key new-node))
    (let [{:keys [value left right]} (tree root)
          cmp-res (compare (:value new-node) value)]
      (cond
        (< cmp-res 0) (recur left)
        (> cmp-res 0) (recur right)
        :else nil))))
