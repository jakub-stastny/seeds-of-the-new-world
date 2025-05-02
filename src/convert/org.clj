(ns convert.org
  (:require [clojure.string :as str]
            [convert.helpers :refer [dbg]]))

(defn parse-org-block-args [arg-str]
  (let [arg-str (str/trim arg-str)
        [positional rest]
        (if (str/starts-with? arg-str "\"")
          (let [[_ quoted remaining] (re-matches #"^\"([^\"]*)\"\s*(.*)" arg-str)]
            [quoted remaining])
          (let [[first-word & more] (str/split arg-str #"\s+" 2)]
            [first-word (or (first more) "")]))
        tokens (str/split (str/trim rest) #"\s+")
        opts (->> (partition 2 tokens)
                  (map (fn [[k v]]
                         [(keyword (subs k 1))
                          (case v
                            "yes" true
                            "no" false
                            v)]))
                  (into {}))]
    [(when (seq positional) positional), opts]))
