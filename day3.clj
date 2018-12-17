(require '[clojure.string :as str])

(defn claiming-points [claim]
  (let [left (:left claim)
        top (:top claim)]
    (flatten
      (for [dx (range 0 (:width claim))]
        (for [dy (range 0 (:height claim))]
          {:id (:id claim) :point [(+ left dx) (+ top dy)]})))))

(defn claim-points [claimed {:keys [id point]}]
  (assoc claimed point
         (if-let [old-ids (get claimed point)]
           (conj old-ids id)
           (vector id))))

(defn count-overlapping-points [claims]
  (count
    (filter (fn [[point ids]] (> (count ids) 1))
      (reduce claim-points {} (flatten (map claiming-points claims))))))

(defn non-overlapping-claim-ids [claims]
  ; (map second
    (filter (fn [[point ids]] (> (count ids) 1))
      (reduce claim-points {} (flatten (map claiming-points claims)))));)

(defn parse-claim [s]
  (let [[id left top width height]
        (map #(Integer/parseInt %)
             (rest (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s)))]
    {:id id :left left :top top :width width :height height}))

; (prn (claiming-points {:id 1 :left 1 :top 3 :width 4 :height 4 }))
; (prn (reduce claim-points {} (claiming-points {:id 1 :left 1 :top 3 :width 4 :height 4 })))
(prn (= (count-overlapping-points [{:id 1 :left 1 :top 3 :width 4 :height 4 }
                                   {:id 2 :left 3 :top 1 :width 4 :height 4 }
                                   {:id 3 :left 5 :top 5 :width 2 :height 2 }]) 4))
(prn (= (parse-claim "#1 @ 258,327: 19x22")
        {:id 1 :left 258 :top 327 :width 19 :height 22}))
(prn (non-overlapping-claim-ids [{:id 1 :left 1 :top 3 :width 4 :height 4 }
                                    {:id 2 :left 3 :top 1 :width 4 :height 4 }
                                    {:id 3 :left 5 :top 5 :width 2 :height 2 }]))
(prn (= (non-overlapping-claim-ids [{:id 1 :left 1 :top 3 :width 4 :height 4 }
                                    {:id 2 :left 3 :top 1 :width 4 :height 4 }
                                    {:id 3 :left 5 :top 5 :width 2 :height 2 }]) [3]))

; (with-open [rdr (clojure.java.io/reader "day3.txt")]
;   (println (count-overlapping-points (map parse-claim (line-seq rdr)))))
