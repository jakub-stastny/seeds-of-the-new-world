(ns convert.string
  (:require [clojure.string :as str]
            [convert.helpers :refer [dbg]]))

(def stopwords
  #{"a" "an" "the" "and" "but" "or" "for" "nor" "on" "at" "to" "from" "by" "of" "in" "with" "over"})

(defn lowercase-or-titlecase? [word]
  (let [lower (str/lower-case word)
        title (str (str/upper-case (subs word 0 1))
                   (str/lower-case (subs word 1)))]
    (or (= word lower) (= word title))))

(defn title-case [s]
  (let [words (str/split s #"\s+")
        count-words (count words)]
    (->> words
         (map-indexed
          (fn [i word]
            (if (not (lowercase-or-titlecase? word))
              word
              (let [parts (str/split word #"-")
                    cap (fn [p] (if (or (= i 0) (= i (dec count-words)) (not (stopwords (str/lower-case p))))
                                  (str/capitalize (str/lower-case p))
                                  (str/lower-case p)))
                    word' (str/join "-" (map cap parts))]
                word'))))
         (str/join " "))))

(defn sentence-case [s]
  (str/replace s #"^." #(str/upper-case %)))

(defn slugify [s]
  (-> s
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"(^-|-$)" "")))
