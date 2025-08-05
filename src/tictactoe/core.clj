(ns tictactoe.core
  (:gen-class))

(def initial-board (vec (repeat 9 nil)))

(def winning-lines
  [[0 1 2] [3 4 5] [6 7 8]
   [0 3 6] [1 4 7] [2 5 8]
   [0 4 8] [2 4 6]])

(defn winner [board]
  (some (fn [[a b c]]
          (let [va (board a) vb (board b) vc (board c)]
            (when (and va (= va vb vc)) va)))
        winning-lines))

(defn board-full? [board]
  (every? some? board))

(defn game-over? [board]
  (or (winner board) (board-full? board)))

(defn score [board]
  (case (winner board)
    :o 1
    :x -1
    0))

(defn available-moves [board]
  (keep-indexed (fn [i v] (when (nil? v) i)) board))

(defn make-move [board idx player]
  (assoc board idx player))

(defn minimax [board player]
  (if (game-over? board)
    {:score (score board)}
    (let [next-player (if (= player :x) :o :x)
          moves (available-moves board)
          results (map (fn [m]
                         (let [new-board (make-move board m player)
                               {:keys [score]} (minimax new-board next-player)]
                           {:move m :score score}))
                       moves)]
      (if (= player :o)
        (apply max-key :score results)
        (apply min-key :score results)))))

(defn best-move [board]
  (:move (minimax board :o)))

(defn render-cell [i v]
  (str (case v
         :x "X"
         :o "O"
         (inc i))))

(defn print-board [board]
  (println)
  (doseq [row (partition 3 (map-indexed render-cell board))]
    (println (clojure.string/join " | " row)))
  (println))

(defn read-move [board]
  (println "Your move (1-9):")
  (let [input (try (Integer/parseInt (read-line)) (catch Exception _ 0))
        idx (dec input)]
    (if (and (<= 0 idx 8) (nil? (board idx)))
      idx
      (do (println "Invalid move. Try again.")
          (recur board)))))

(defn human-turn [board]
  (let [idx (read-move board)]
    (make-move board idx :x)))

(defn computer-turn [board]
  (println "Computer's move:")
  (let [idx (best-move board)]
    (println (inc idx))
    (make-move board idx :o)))

(defn game-loop [board player]
  (print-board board)
  (if-let [w (winner board)]
    (println (str (clojure.string/upper-case (name w)) " wins!"))
    (if (board-full? board)
      (println "Draw!")
      (recur
       (case player
         :x (computer-turn board)
         :o (human-turn board))
       (if (= player :x) :o :x)))))

(defn -main [& _]
  (println "Welcome to Tic Tac Toe!")
  (game-loop initial-board :o))
