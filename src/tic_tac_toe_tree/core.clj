(ns tic-tac-toe-tree.core
  (:gen-class))


(def board {})  ;sparse game board
(def plays #{[0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]})
(def node-count (atom 0))

;(defn or1 [a b] (or a b))

(def lines [[[0 0] [0 1] [0 2]]
            [[1 0] [1 1] [1 2]]
            [[2 0] [2 1] [2 2]]
            [[0 0] [1 0] [2 0]]
            [[0 1] [1 1] [2 1]]
            [[0 2] [1 2] [2 2]]
            [[0 0] [1 1] [2 2]]
            [[2 0] [1 1] [0 2]]
            ])

;Our old friend for depth first digging in clojure. (might be a better way. tell me if you find it)
(defn doseqfor [combineby deck digwith]
  (do	(doseq [val deck] (digwith val))
	   (let [vallist (vec (for [val deck] (digwith val)))]
      (combineby vallist))))

;assume sparse board!!
;_any_ win (since we know it'll be the current play... IF we build from the first play...)
(defn win? [b]
  (reduce #(or % %2)
          (for [e lines] ;checks each line
            (let [a (mapv b e)
                  ret (apply = a) ]
              (and ret (not= [nil nil nil] a)) ))	))  ;returns true if all 3 entries in a line is = AND not all = nil



;**node cnt? try it w/o memoize?
(def ttt (memoize (fn [board play p1?]
                    (swap! node-count inc)
                    (let [	new-board (assoc board play (if p1? 1 2))
                          plays1 (apply (partial disj plays) (keys new-board)) ] ;**board must be sparse for this to work, no nil values filled in
                      
                      (if (win? new-board)
                        (if p1? 1 -1)
                        (if (empty? plays1)
                          0
                          (doseqfor (partial apply (if p1? min max)) ;collection fn for _after_ next level(?)
                                    plays1
                                    #(ttt new-board % (not p1?))		) ) ) ))))


;init call?
;(ttt {} [0 0] true)
;(map #(ttt {} % true) plays)


