(ns clj.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defrecord Node [value child left right])
(def tree {0 (Node. "Root" 1 nil nil)
           1 (Node. "B" nil 2 3)
           2 (Node. "A" nil nil nil)
           3 (Node. "C" nil nil nil)})
(def nv "D")
;(defn insert [tree root-id value])

(loop [root 1]
  (if (nil? root)
    (inc  42)
    (let [{:keys [value left right]} (tree root)
          cmp-res (compare nv value)]
      (cond
        (< cmp-res 0) (recur left)
        (> cmp-res 0) (recur right)
        :else nil))))
