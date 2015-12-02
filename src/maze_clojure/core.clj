(ns maze-clojure.core
  (:gen-class))

(def size 10)

(defn create-rooms []
  (vec (for [row (range 0 size)]
         (vec (for [col (range 0 size)]
                {:row     row, :col col, :visited? false
                 :bottom? true, :right? true,
                 :start?  (and (= 0 row) (= 0 col))})))))

(defn possible-neighbors [rooms row col]
  [(get-in rooms [(- row 1) col])
   (get-in rooms [(+ row 1) col])
   (get-in rooms [row (- col 1)])
   (get-in rooms [row (+ col 1)])])

(defn random-neighbor [rooms row col]
  (let [neighbors (possible-neighbors rooms row col)
        neighbors (filter (fn [room]
                            (and room
                                 (not (:visited? room))))
                          neighbors)]
    (if (pos? (count neighbors))
      (rand-nth neighbors)
      nil)))

(defn tear-down-wall [rooms old-row old-col new-row new-col]
  (cond
    ; going up
    (< new-row old-row)
    (assoc-in rooms [new-row new-col :bottom?] false)
    ; going down
    (> new-row old-row)
    (assoc-in rooms [old-row old-col :bottom?] false)
    ; going left
    (< new-col old-col)
    (assoc-in rooms [new-row new-col :right?] false)
    ; going right
    (> new-col old-col)
    (assoc-in rooms [old-row old-col :right?] false)))

(defn set-end-if-necessary [rooms row col]
  (let [filtered-rooms (filter :end? (flatten rooms))]
    (if (pos? (count filtered-rooms))
      rooms
      (assoc-in rooms [row col :end?] true))))

(defn create-maze [rooms row col]
  (let [rooms (assoc-in rooms [row col :visited?] true)
        next-room (random-neighbor rooms row col)]
    (if next-room
      (let [rooms (tear-down-wall rooms row col (:row next-room) (:col next-room))]
        (loop [old-rooms rooms]
          (let [new-rooms (create-maze old-rooms (:row next-room) (:col next-room))]
            (if (= old-rooms new-rooms)
              old-rooms
              (recur new-rooms)))))
      (set-end-if-necessary rooms row col))))



(defn -main [& args]
  (let [rooms (create-rooms)
        rooms (create-maze rooms 0 0)]
    (doseq [row rooms]
      (print " _"))
    (println)
    (doseq [row rooms]
      (print "|")
      (doseq [room row]
        (print (str (cond
                      (:start? room) "o"
                      (:end? room) "x"
                      (:bottom? room) "_"
                      :else " ")
                    (if (:right? room) "|" " "))))
      (println))))
