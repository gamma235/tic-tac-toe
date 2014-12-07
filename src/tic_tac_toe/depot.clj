;; this is a file for stuff that has not yet been but might get used in core.clj
(def side-corner-pairs (concat opposite-corners same-side-corners))

(defn has-corners? [player]
  (< 1 (count (remove #(= % nil) (for [corner corners]
                                   (@player corner))))))
