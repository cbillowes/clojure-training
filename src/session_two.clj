(ns session-two
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.instant :as inst]
            [camel-snake-kebab.core :as csk]))

(defonce orders (atom []))


(defn read-lines-from-file!
  "Returns a vector of strings"
  ([]
   (read-lines-from-file! "Orders.csv"))
  ([filename]
   (with-open [rdr (io/reader filename)]
     (into [] (doall (line-seq rdr))))))



(defn- get-header-row
  []
  (->> (read-lines-from-file!)
       (take 1)
       (map #(str/split % #","))
       (first)
       (map csk/->kebab-case-keyword)))


(defn- parse-values
  [{:keys [category sub-category] :as order}]
  (let [category (-> (str category "/" sub-category)
                     (str/replace #" " "_")
                     ;; My reasoning about (keyword):
                     ;; I keep the parenthesis because it's beautifully consistent
                     ;; and is 100% clear it's a function call for novice Clojurians.
                     (keyword))]
    (-> order
        (dissoc :sub-category)
        (assoc :category category)
        (update :order-date inst/read-instant-date)
        (update :ship-date inst/read-instant-date)
        (update :sales parse-double) ; What am I doing wrong with Float/parseFloat ??? Unable to find static field: parseFloat in class java.lang.Float
        (update :profit parse-double)
        (update :row-id parse-long)
        (update :discount parse-long)
        (update :quantity parse-long))))


(defn- get-orders
  []
  (if (empty? @orders)
    (->> (read-lines-from-file!)
         (rest)
         (map #(str/split % #","))
         (map #(zipmap (get-header-row) %))
         (map parse-values)
         (swap! orders concat))
    @orders))


(defn get-unique-values-for
  [orders column-name]
  (->> orders
       (map #(get % column-name))
       (distinct)))


(defn- calculate-profit-by-category
  [orders set-of-categories]
  (let [filter-by-categories (fn [order]
                               (if (empty? set-of-categories)
                                 true
                                 (contains? set-of-categories (:category order))))]
    (->> orders
         (filter filter-by-categories)
         (map #(if (nil? (:profit %)) 0.0 (:profit %)))
         (reduce +))))


(defn- calculate-by-field
  [orders field-to-calculate field-to-filter-by field-values ]
  (let [filter-by-field (fn [order]
                          (if (empty? field-values)
                            true
                            (contains? field-values (get order field-to-filter-by))))]
    (->> orders
         (filter filter-by-field)
         (map #(if (nil? (get % field-to-calculate)) 0.0 (get % field-to-calculate)))
         (reduce +))))


(comment

  ;; PART 1
  (->> (get-orders)
       (clojure.pprint/pprint))


  ;; PART 2
  (-> (get-orders)
      (get-unique-values-for :category)
      (clojure.pprint/pprint))


  ;; PART 3
  (-> (get-orders)
      (calculate-profit-by-category #{})
      (clojure.pprint/pprint))

  (-> (get-orders)
      (calculate-profit-by-category #{:Furniture/Bookcases})
      (clojure.pprint/pprint))

  (-> (get-orders)
      (calculate-profit-by-category #{:Furniture/Bookcases :Furniture/Chairs :Furniture/Tables})
      (clojure.pprint/pprint))


  ;; PART 4
  (-> (get-orders)
      (calculate-by-field :sales :state #{"Kentucky" "Florida"}))

  )
