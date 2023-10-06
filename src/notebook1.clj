;; # Hello 👋
;; You can use **Markdown** in *Clerk* notebooks,
;; with formulas: $$ \varepsilon = mc^2 $$.
;; Below is some notes how to use Clojure and Clerk.

;; - Install VSCode
;; - Install *Calva* (`Extensions` at the left panel or
;;   `File/Preferences/Extensions`)
;; - Open the folder `external-beam-lwfa` in VSCode
;; - Run REPL - see button at left at the bottom panel
;;   (jack-in, `src` as root, `deps.edn`)
;; - Have fun with `Ctrl+Enter` (send minimal closed piece of code
;;   to the REPL) or `Alt+Enter` (send all the expression to the REPL),
;;   or `Ctrl+Alt+C` then `Enter` (execute all file)

;; Open `src/user.clj`, put the cursor inside and press `Ctrl+Alt+C` then
;; `Enter`, then choose `src/notebook.clj` in the browser to see this file rendered.
;; Or return to this file (notebook.clj) and save it pressing `Ctrl+S`, it will
;; be updated in the browser.

;; Try it:

(+ 1 2) ;; Alt+Enter

(+ 1 2 3 4)

(+ (+ 2 3) ;; Ctrl+Enter
   (* 4 5)
   (/ 20 5 2)) ;; Alt+Enter

;; Vector
[1 2 (+ 1 2)]

;; Constant
(def some-vec [1 2 3])
(get some-vec 0)

;; Lists. To get element by index, first convert list to vector.
'(1 2 3)
(first '(1 2 3))
(rest '(1 2 3))
(get (vec '(1 2 3)) 2)

;; Hash map (dictionary)
;; { key1 value1
;;   key2 value2 }
(def some-map {:a 42 "key" "value" 57 58})
(get some-map :a)
(:a some-map)
(get some-map "key")
(get some-map 57)

;; function definition
(defn some-fun [x] (+ x 1))

;; `map` and lambda functions
(map some-fun [1 2 3])
(map #(+ % 1) [1 2 3])
(map (fn [x] (+ x 1)) [1 2 3])
(map + [1 2 3] [10 20 30])

;; `concat` and `mapcat` (map then concat)
;; To see help, press `Ctrl+Shift+P` (palette), then write `doc` and hit
;; `Require REPL utilities like doc...`
;; Try copy-and-paste this to REPL:
;; (doc concat)
;; (doc mapcat)
(concat [1 2 3] [4 5 6])
(str "Hello, " 42 "!")
(map (fn [x] [(- x) x]) [1 2 3])
(mapcat (fn [x] [(- x) x]) [1 2 3])

;; `apply` applies a function to a list of its arguments
(apply + [1 2 3])
(apply concat (map (fn [x] [(- x) x]) [1 2 3]))
(mapcat (fn [x] (range x)) [1 2 3 4])

;; `let` is a local definition
(let [x 42
      y 35]
  (- x y))

;; `if` returns
(if (= 1 2) 42 35)
(str "smth = " (if (not= 1 2) (+ 1 4) (* 2 2)))

;; --------------------------------------------------------
;; ## The main namespace is below
;; --------------------------------------------------------

(ns notebook1
  (:require [nextjournal.clerk.viewer :as v]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [scicloj.ml.dataset :as ds]))

;; try it by parts with Ctrl+Enter
(map #(string/split % #" ")
      (string/split-lines (slurp (io/file (str "resources/" "res.txt"))) ;; try
                          ) ;; try
     ) ;; try

(defn read-data-vv
  "Reads data from file `./resources/filename`, returns vector of vectors of numbers."
  [filename]
  (let [str2double (fn [s] (try (Double/parseDouble s)
                                (catch Exception _ nil)))
        lines (string/split-lines (slurp (io/file (str "resources/" filename))))
        subLines (map (fn [s] (string/split s #" ")) lines)
        numbers (map (fn [line] (map str2double line)) subLines)]
    (vec (map vec numbers))))

(read-data-vv "example.txt") ;; try Alt+Enter
(assert (= (first (first (read-data-vv "example.txt"))) 1.0))
(assert (= (last (last (read-data-vv "example.txt"))) 6.0))

(defn read-data
  "Reads data of special format to dataset with :p0 etc columns"
  [filename]
  (let [vector-of-vectors (read-data-vv filename)
        column (fn [i] (map (fn [list] (get list i)) vector-of-vectors))
        m {:angle (column 0) ;; initial e-beam angle, in radians
           :p0 (column 1) ;; initial electron momentum, in mc
           :ne (map #(* % 1e18) (column 2)) ;; ne in cm^{-3}
           :trapped (column 3) ;; percent of beam electrons trapped
           :mean-p (column 4) ;; resulting mean electron momentum, in mc 
           :sigma (column 5) ;; standart deviation of resulting momentum, in mc
           }]
    (ds/dataset m))) ;; dataset in Scicloj is very similar to that in Plotly

;; ### Time to plot something

(def res-raw (read-data "res1.txt"))

(defn get-column-data
  "Get data from column `key` of dataset `dset`"
  [dset key]
  (vec (ds/column dset key)))

(defn add-rel-sigma
  "Let's add a column with relative sigma, sigma / mean-p."
  [dset]
  (let [safe-divide (fn [x y] (if (= y 0.0) 0.0 (/ x y)))
        sigma (get-column-data dset :sigma)
        mean-p (get-column-data dset :mean-p)]
    (ds/add-column dset :rel-sigma (map safe-divide sigma mean-p))))

(defn add-p-gain
  "Let's add gained momentum, mean-p - p0."
  [dset]
  (ds/add-column dset :p-gain (map -
                                   (get-column-data dset :mean-p)
                                   (get-column-data dset :p0))))

(def res (add-p-gain (add-rel-sigma res-raw)))

(defn simple-plot
  "Makes a dictionary which resembles a json of Plotly plot,
   with `keyY` vs `keyX` markers, coloured with `keyZ`"
  [dset keyX keyY keyZ]
  {:layout {:title {:text (str keyZ)}
            :xaxis {:title (str keyX)}
            :yaxis {:title (str keyY)}}
   :data [{:x (get-column-data dset keyX)
           :y (get-column-data dset keyY)
           :marker {:color (get-column-data dset keyZ)}
           :type "scatter"
           :mode "markers"}]})

 ;; plotly viewer renders it
(v/plotly (simple-plot res :mean-p :rel-sigma :trapped))

;; ### Анализ

;; Построим зависимость разброса импульса от набранного импульса, а не от
;; среднего импульса (который содержит в себе p0):

(v/plotly (simple-plot res :p-gain :rel-sigma :trapped))

;; Из этого графика видно, что при набранном импульсе выше 20 mc не получается
;; добиться малого разброса по энергиям ни в одном счёте. Разброс по энергии
;; в счётах с импульсом больше 100 mc примерно одинаковый, то есть он слабо зависит
;; от угла, начального импульса и плотности плазмы.

;; Процент захваченных частиц странным образом коррелирует с плотностью плазмы
;; и может быть очень высок:

(v/plotly (simple-plot res :ne :trapped :p-gain))

;; Зависимость набранного импульса от начального импульса, угла влёта
;; и плотности плазмы:
(v/plotly (simple-plot res :p0 :p-gain :trapped))
(v/plotly (simple-plot res :angle :p-gain :trapped))
(v/plotly (simple-plot res :ne :p-gain :trapped))

;; Таким образом, оптимальными можно назвать счёты с набранным импульсом
;; больше 200 mc:

;; (drop all rows with gained p less than 200)
(ds/drop-rows res (map #(< % 200) (get-column-data res :p-gain)))
