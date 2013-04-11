(ns tictactoe.core
  (:gen-class))


(defn make-board ([] (vec (replicate 3 (vec (replicate 3 0))))))

(defn place ([player x y board] (assoc board x (assoc (nth board x) y player))))

(defn count-tokens ([player list] (count (filter (fn[x] (== x player)) list))))

(defn board-cols ([board] board))

(defn board-rows ([board] (map (fn[y] (map (fn[x] (nth x y)) board)) (range (count board)))))

(defn board-diags
  ([board] (list (map (fn[a] (nth (nth board a) a)) (range (count board)))
                 (map (fn[a] (nth (nth board (- (count board) a 1)) a)) (range (count board))))))

(defn board-lists ([board] (concat (board-cols board) (board-rows board) (board-diags board))))

(defn player-win
  ([player board] (== (count board)
                      (reduce max (map (fn[line] (count-tokens player line)) (board-lists board))))))

(defn get-int [] (try (Integer/parseInt (read-line)) (catch Exception e nil)))

(defn valid-input
  ([x y board] (and (>= (min x y)  0) (< (max x y) (count board)) (== (nth (nth board x) y) 0))))

(defn win ([player] (println "Player:" player "wins!")))

(defn can-place
  ([x y board] (if (and (>= (min x y) 0) (< (max x y) (count board)))
                 (if (nth (nth board x) y) false true) false)))

(defn dump-board ([board] (map println (board-rows board)) board))

(defn inquire-and-place
  ([p board] (let [x (get-int) y (get-int)]
               (if (and x y (valid-input x y board))
                 (place p x y board)
                 (do (println "Invalid Input")
                     (inquire-and-place p board))))))

(defn game-step [p1 p2 board]
  (let [afterp1 (inquire-and-place p1 board)]
    (if (player-win p1 (dump-board afterp1)) (win p1)
          (let [afterp2 (inquire-and-place p2 afterp1)]
            (if (player-win p2 (dump-board afterp2)) (win p2)
                (game-step p1 p2 afterp2))))))
(defn -main
  [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (game-step 1 2 (make-board)))
