(ns p-p-p-pokerface)

(def rank-mapping {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank-char [card]
  (let [[r _] card]
    r))

(defn rank [card]
  (let [rank (rank-char card)
        mapping (rank-mapping rank)]
    (if (nil? mapping)
      (Integer/valueOf (str rank))
      mapping)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn max-rank-freq [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        times (vals freq)
        maxt (apply max times)]
    maxt))

(defn pair? [hand]
  (>= (max-rank-freq hand) 2))

(defn three-of-a-kind? [hand]
  (>= (max-rank-freq hand) 3))

(defn four-of-a-kind? [hand]
  (>= (max-rank-freq hand) 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn sort-freqs [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    freqs))

(defn full-house? [hand]
  (let [freqs (sort-freqs hand)
        full? (= freqs '(2 3))]
    full?))

(defn two-pairs? [hand]
  (let [freqs (sort-freqs hand)]
    (or (= freqs '(1 4)) (= freqs '(1 2 2)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        min-rank (apply min ranks)
        trans-ranks (if (= min-rank 2)
                      (replace {14 1} ranks)
                      ranks)
        sorted-trans-ranks (sort trans-ranks)
        new-min-rank (first sorted-trans-ranks)
        max-straight-rank (+ new-min-rank 5)]
    (= sorted-trans-ranks (range new-min-rank max-straight-rank))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4]  [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        applicables (filter (fn [[x _]] (x hand)) checkers)]
    (apply max (map second applicables))))
