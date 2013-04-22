;; jordon biondo
;; ben summers
;; clojure connect 4

(ns tictactoe.core
  (:gen-class))

(defn make-board
  "make the board, it is a list of 3 lists, each with 3 elements set to 0"
  ([] (vec (replicate 3 (vec (replicate 3 0))))))


(defn place
  "in the xth column, change the value at position y, to player"
  ([player x y board] (assoc board x (assoc (nth board x) y player))))


(defn count-tokens
  "In a given list (row or column) filter out values that don't equal player, then count the remaining"
  ([player list] (count (filter (fn[x] (== x player)) list))))


(defn board-rows
  "get the board as rows, instead columns"
  ([board] (map (fn[y] (map (fn[x] (nth x y)) board)) (range (count board)))))


(defn board-diags
  "Get the two diagonal lists of the board"
  ([board] (list (map (fn[a] (nth (nth board a) a)) (range (count board)))
                 (map (fn[a] (nth (nth board (- (count board) a 1)) a)) (range (count board))))))


(defn board-lists
  "Return a list containing the boards columns, rows and both diags"
  ([board] (concat board (board-rows board) (board-diags board))))


(defn player-win
  "Count the number of player tokens in each board list, if one of them is 3, return true"
  ([player board] (== (count board)
                      (reduce max (map (fn[l] (count-tokens player l)) (board-lists board))))))


(defn get-int
  "Get an int"
  [name] (print name ": ") (flush)
  (let [num (try (Integer/parseInt (read-line)) (catch Exception e nil))]
    (if (and num (> num 0) (< num 10))
      (let [x (mod (dec num) 3) y (int (/ (dec num) 3))]
        (vector x y))
      (vector nil nil))))


(defn valid-input
  "Validate that a given x y postion is on the board  0 <= (x & y) <= 2"
  ([x y board] (and (>= (min x y)  0) (< (max x y) (count board)) (== (nth (nth board x) y) 0))))


(defn win
  "Tell the world that P won"
  ([p] (println "--- Player" (nth '("Ｘ" "◌") (dec p)) "wins! ---")))


(defn dump-board
  "Print the board, note that println needs to be given to apply in order for stdout to be forced"
  ([board] (let [visboard board chars (vector "１" "２" "３" "４" "５" "６" "７" "８" "９")]
             (doseq [y (range 3)]
               (apply println (concat "|" (map (fn[x] (nth (vector (nth chars (- (+(* (inc y) 3) x) 3)) "Ｘ" "◌")
                                                           (nth (nth (board-rows board) y) x))) (range 3)) "|")))) board))


(defn is-cat-game
  "True if there is no empty spaces on the board, the gets the minium element in all the columns, if it is 0,
there is an empty space"
  ([board] (not= (reduce min (reduce concat  board)) 0)))


(defn inquire-and-place
  "Get user input, and place a token for p"
  ([p board] (println "It is " (nth '("X" "O") (dec p)) "'s turn") (flush)
     (let [values (get-int "your move?")]
       (let [x (first values) y (second values)]
         (if (and x y (valid-input x y board)) (place p x y board)
             (do (println "--- Invalid Input ---") (recur p board)))))))


(defn game-step [p1 p2 board]
  "Places a p1 token on the board and check for win or cat, then recurse"
  (let [afterp1 (inquire-and-place p1 board)]
    (if (player-win p1 (dump-board afterp1)) (win p1)
        (if (is-cat-game afterp1) (println "--- Cat's game --- ") (recur p2 p1 afterp1)))))


(defn -main [& args]
  "Run the game"
  (println "Welcome to tic tac toe!\nEnter the number you where you want to place a token.")
  (let [board (make-board)]
    (dump-board board)
    (game-step 1 2 (make-board))))
