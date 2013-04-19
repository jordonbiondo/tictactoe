;; jordon biondo
;; ben summers
;; clojure connect 4

(ns tictactoe.core (:gen-class))

(defn make-board ([] (vec (replicate 3 (vec (replicate 3 0))))))

(defn place ([player x y board] (assoc board x (assoc (nth board x) y player))))

(defn count-tokens ([player list] (count (filter (fn[x] (== x player)) list))))

(defn board-rows ([board] (map (fn[y] (map (fn[x] (nth x y)) board)) (range (count board)))))

(defn board-diags
  ([board] (list (map (fn[a] (nth (nth board a) a)) (range (count board)))
                 (map (fn[a] (nth (nth board (- (count board) a 1)) a)) (range (count board))))))

(defn board-lists ([board] (concat board (board-rows board) (board-diags board))))

(defn player-win
  ([player board] (== (count board)
                      (reduce max (map (fn[l] (count-tokens player l)) (board-lists board))))))

(defn get-int [name] (print name ": ") (flush)
  (let [num (try (Integer/parseInt (read-line)) (catch Exception e nil))]
    (if (and num (> num 0) (< num 10))
      (let [x (mod (dec num) 3) y (int (/ (dec num) 3))]
        (vector x y))
      (vector nil nil))))

(defn valid-input
  ([x y board] (and (>= (min x y)  0) (< (max x y) (count board)) (== (nth (nth board x) y) 0))))

(defn win ([p] (println "--- Player" (nth '("Ｘ" "◌") (dec p)) "wins! ---")))

(defn can-place
  ([x y board] (if (and (>= (min x y) 0) (< (max x y) (count board)))
                 (if (nth (nth board x) y) false true) false)))

(defn dump-board ([board] (let [visboard board chars (vector "１" "２" "３" "４" "５" "６" "７" "８" "９")]
                            (doseq [y (range 3)]
                              (apply println (concat "|" (map (fn[x] (nth (vector (nth chars (- (+(* (inc y) 3) x) 3)) "Ｘ" "◌")
                                                        (nth (nth (board-rows board) y) x))) (range 3)) "|")))) board))

(defn is-cat-game ([board] (not= (reduce min (reduce concat  board)) 0)))

(defn inquire-and-place
  ([p board] (println "It is " (nth '("X" "O") (dec p)) "'s turn") (flush)
     (let [values (get-int "your move?")]
       (let [x (first values) y (second values)]
         (if (and x y (valid-input x y board)) (place p x y board)
             (do (println "--- Invalid Input ---") (inquire-and-place p board)))))))

(defn game-step [p1 p2 board]
  (let [afterp1 (inquire-and-place p1 board)]
    (if (player-win p1 (dump-board afterp1)) (win p1)
        (if (is-cat-game afterp1) (println "--- Cat's game --- ") (game-step p2 p1 afterp1)))))

(defn -main [& args]
  (println "Welcome to tic tac toe!\nEnter the number you where you want to place a token.")
  (let [board (make-board)]
    (dump-board board)
    (game-step 1 2 (make-board))))
