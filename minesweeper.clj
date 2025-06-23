(ns minesweeper.core
  (:require [clojure.string :as str]))

;;; Глобальное состояние игры
(def game-state (atom nil))

;;; Структура клетки
(defrecord Cell [mine? count state])

;;; Инициализация новой игры
(defn new-game [rows cols mines-count]
  (let [total-cells (* rows cols)
        mine-positions (set (take mines-count (shuffle (range total-cells))))]
    {:board (vec (for [i (range total-cells)]
                   (let [mine? (contains? mine-positions i)]
                     (->Cell mine? 0 :closed))))
     :rows rows
     :cols cols
     :mines-count mines-count
     :game-state :playing}))

;;; Подсчет мин вокруг клетки (исправленный синтаксис for)
(defn count-mines-around [board idx rows cols]
  (let [row (quot idx cols)
        col (mod idx cols)
        neighbor-offsets (for [dr [-1 0 1]
                           dc [-1 0 1]
                           :when (not (and (zero? dr) (zero? dc)))]
                           [dr dc])]
    (count
      (for [[dr dc] neighbor-offsets
            :let [r (+ row dr)
                  c (+ col dc)]
            :when (and (>= r 0) (< r rows) (>= c 0) (< c cols))
            :let [pos (+ c (* r cols))]
            :when (:mine? (nth board pos))]
        true))))

;;; Обновление счетчиков мин на доске
(defn update-counts [board rows cols]
  (vec (map-indexed (fn [idx cell]
                      (if (:mine? cell)
                        cell
                        (assoc cell :count (count-mines-around board idx rows cols))))
                    board)))

;;; Открытие клетки (исправленный синтаксис for и расчет индекса)
(defn open-cell [game idx]
  (let [board (:board game)
        cell (nth board idx)
        cols-count (:cols game)]
    (cond
      (not= :closed (:state cell)) game
      (:mine? cell) (assoc game :game-state :lost)
      :else (let [new-board (assoc board idx (assoc cell :state :open))
                  new-game (assoc game :board new-board)]
              (if (zero? (:count cell))
                (reduce (fn [g i] (open-cell g i)) 
                        new-game 
                        (filter #(= :closed (:state (nth board %))) 
                                (set (for [dr [-1 0 1] 
                                        dc [-1 0 1]
                                        :when (not (and (zero? dr) (zero? dc)))
                                        :let [new-row (+ (quot idx cols-count) dr)
                                              new-col (+ (mod idx cols-count) dc)
                                              new-idx (+ new-col (* new-row cols-count))]
                                        :when (and (>= new-row 0) (< new-row (:rows game))
                                                 (>= new-col 0) (< new-col cols-count))]
                                        new-idx))))
                new-game)))))

;;; Переключение флага
(defn toggle-flag [game idx]
  (update-in game [:board idx] 
             #(case (:state %)
                :closed (assoc % :state :flagged)
                :flagged (assoc % :state :closed)
                %)))

;;; Проверка победы
(defn check-win [game]
  (every? (fn [cell]
            (if (:mine? cell)
              (not= :open (:state cell))
              (= :open (:state cell))))
          (:board game)))

;;; Отображение клетки
(defn cell-display [cell game-ended?]
  (cond
    (and game-ended? (:mine? cell)) "*"
    (= :open (:state cell)) (if (zero? (:count cell)) " " (str (:count cell)))
    (= :flagged (:state cell)) "F"
    :else "-"))

;;; Печать игрового поля
(defn print-board [game]
  (let [board (:board game)
        rows (:rows game)
        cols (:cols game)
        game-ended? (not= :playing (:game-state game))]
    (println "\n " (str/join " " (map #(format "%2d" %) (range cols))))
    (doseq [r (range rows)]
      (print (format "%2d " r))
      (doseq [c (range cols)]
        (let [idx (+ c (* r cols))
              cell (nth board idx)]
          (print (cell-display cell game-ended?) " ")))
      (println)))
  (flush))

;;; Обработка хода игрока
(defn process-input [game input]
  (let [parts (str/split input #" ")]
    (try
      (let [command (first parts)
            row (Integer/parseInt (nth parts 1))
            col (Integer/parseInt (nth parts 2))
            idx (+ col (* row (:cols game)))]
        (cond
          (and (= command "o") (= :playing (:game-state game))) (open-cell game idx)
          (and (= command "f") (= :playing (:game-state game))) (toggle-flag game idx)
          :else game))
      (catch Exception _ game))))

;;; Главный игровой цикл
(defn game-loop []
  (reset! game-state (new-game 8 8 10)) ; Размер 8x8, 10 мин
  (swap! game-state update :board update-counts (:rows @game-state) (:cols @game-state))
  (loop []
    (print-board @game-state)
    (cond
      (= :lost (:game-state @game-state))
        (println "BOOM HA HA! Game Over! You lose.")
      (= :win (:game-state @game-state))
        (println "YUIPII! You won!"))
    
    (if (#{:win :lost} (:game-state @game-state))
      (do
        (println "Play again? (y/n)")
        (if (= "y" (str/lower-case (read-line)))
          (do (reset! game-state (new-game 8 8 10)) 
              (swap! game-state update :board update-counts 8 8)
              (recur))
          (println "Thanks for playing!")))
      (do
        (print "How play - commands: (o <row> <col> - open, f <row> <col> - flag): ")
        (flush)
        (let [input (read-line)]
          (swap! game-state process-input input)
          (when (and (= :playing (:game-state @game-state))
                    (check-win @game-state))
            (swap! game-state assoc :game-state :win))
          (recur))))))

;;; Запуск игры
(defn -main []
  (println "Hello little minesweeper!")
  (println "Commands:")
  (println "  o <row> <col> - open square")
  (println "  f <row> <col> - put/remove flag")
  (game-loop))

;; Явный вызов главной функции
(-main)