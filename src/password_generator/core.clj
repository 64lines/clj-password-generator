(ns password-generator.core
  (:gen-class))

(def default-password-size 15)
(def alphabeth-uppercase "ABCDEFGHIJKLMOPQRSTUVWXYZ")
(def alphabeth-lowercase "abcdefghijklmopqrstuvwxyz")
(def special-chars "!#$%&/()=?-_<>|.")

(defn get-random-letter [collection]
  (get collection (rand-int (count collection))))

(defn rand-range [start end]
  (+ (rand-int (- end start)) start))

(defn contains [coll element]
  (some #(= element %) coll))

(defn get-password-char [positions letters password-text index]
  (if (contains positions index) (get letters (mod index (count letters))) (get password-text index)))

(defn get-random-positions [char-collection char-number]
  (map (fn [_] (rand-int (count char-collection))) (range char-number)))

(defn add-chars-to-password [char-collection max-chars password-text]
  (let [char-number (rand-range 1 (count char-collection))
        letters (apply str (take char-number char-collection))
        random-positions (get-random-positions char-collection char-number)]
        (apply str (map #(get-password-char random-positions letters password-text %) (range (count password-text))))))

(defn create-password-letter [index] 
  (cond
    (= (rand-int 2) 0) (get-random-letter alphabeth-uppercase)
    :else (get-random-letter alphabeth-lowercase)))

(defn generate-password [size]
  "Generates the password using a specified size"
  (apply str (map #(create-password-letter %) (range size))))

(defn -main
  [& args]
  (let [no-special-args (some #(= "--no-special-chars" %) args)
        password (generate-password default-password-size)] 
    (println 
      (if no-special-args password (add-chars-to-password special-chars 3 password)))))
