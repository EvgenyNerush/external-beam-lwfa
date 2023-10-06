;; ### ne is fixed, L is variable

(ns notebook2
  (:require [nextjournal.clerk.viewer :as v]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [scicloj.ml.dataset :as ds]))

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
           :length (column 2) ;; mm
           :trapped (column 3) ;; percent of beam electrons trapped
           :mean-p (column 4) ;; resulting mean electron momentum, in mc 
           :sigma (column 5) ;; standart deviation of resulting momentum, in mc
           }]
    (ds/dataset m))) ;; dataset in Scicloj is very similar to that in Plotly

;; ### res2

(def res-raw (read-data "res2.txt"))

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

;; ### Анализ

;; Построим зависимость набранного импульса от всех переменных начальных
;; параметров

;; plotly viewer renders the given 'json'
(v/plotly (simple-plot res :angle :p-gain :trapped))
(v/plotly (simple-plot res :p0 :p-gain :trapped))
(v/plotly (simple-plot res :length :p-gain :trapped))

;; => есть оптимальный угол, при этом зависимость от p0 и длины плазмы
;; не такая ясная. Процент захваченных частиц, как правило, велик.

;; Разброс по энергии также имеет оптимум при определённом угле:

(v/plotly (simple-plot res :angle :rel-sigma :p-gain))

;; Отфильтруем случаи с высокой набранной энергией

(v/plotly (simple-plot res :p-gain :rel-sigma :trapped))

(def res-filtered (ds/drop-rows res (map #(< % 300) (get-column-data res :p-gain))))

res-filtered
