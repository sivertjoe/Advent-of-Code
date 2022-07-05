(require ['clojure.string :as 'str])

(def lines 
  (->>
    (str/split-lines (slurp "input"))
    (filter (fn [elem] (not (str/includes? elem "no other"))))))
    
(defn get-inner [line]
  (str/split
    (->>
      (str/split line #"\s?bags?,?.?")
      (map str/trim)
      (first))
    #"\s"
    2))
  
;; Use 'first' since this list only has two elements (since
;; the string "contains" is only contained once)
(defn get-map-val [list]
  (->>
    (str/split (first list) #",")
    (map get-inner)))

(defn get-map-key [s]
  (first (str/split s #" bags")))

(defn get-map [lines]
  (reduce 
    (fn [m line]
      (let [spl (str/split line #"contain")
            k (get-map-key (first spl))
            v (get-map-val (rest spl))]
          (assoc m k v)))
    {}
    lines)
)

(def my-map (get-map lines))

; Use 'or' here to short circut if we found a shiny gold
(defn count-shiny-bags' [map bag]
  (or 
    (= bag "shiny gold")
    (some
      (fn [bag]
        (count-shiny-bags' map (second bag)))
      (get map bag))))

(defn count-shiny-bags [map]
  (reduce-kv
    (fn [acc k v] 
      (+ acc 
        (if (count-shiny-bags' map k) 1 0)))
    0
    map))

(defn count-bags' [map bags n]
  (->>
    (reduce 
      (fn [acc bag]
        (+
          acc
          (count-bags' map (get map (second bag)) (Integer. (first bag)))))
      1
      bags)
    (* n)))

(defn count-bags [map]
  (count-bags' map (get map "shiny gold") 1))

; Minus one since the listing contains a shiny gold
(defn part-one []
  (- (count-shiny-bags my-map) 1))
    
(defn part-two []
  (- (count-bags my-map) 1))
    
(defn main []
  (println (part-one))
  (println (part-two)))
    
(main)