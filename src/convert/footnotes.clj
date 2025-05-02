(ns convert.footnotes
  (:require [clojure.string :as str]
            [convert.helpers :refer [dbg]]))

(defn collect-footnotes-and-strip [lines]
  (reduce (fn [{:keys [lines footnotes]} line]
            (if-let [[_ key txt] (re-matches #".*\[fn:([^\]]+)\]\s+(.*)" (:text line))]
              {:lines lines
               :footnotes (if (get footnotes (keyword key))
                            (update footnotes (keyword key) conj txt)
                            (assoc footnotes (keyword key) [txt]))}
              {:lines (conj lines line) :footnotes footnotes}))
          {:lines [] :footnotes {}}
          lines))

(def footnote-types
  {:ref #(str "\\ref{\\goto{" % "}[url(" % ")]}")
   :ftn #(str "\\footnote{" % "}")})

(defn format-footnote [key item]
  (if-let [formatter (get footnote-types key)]
    (formatter item)
    (throw (str "No such footnote key " key))))

;; Process text with regex pattern and apply a handler to each match
(defn process-matches [text footnotes pattern handler-fn]
  (let [matcher (re-matcher pattern text)]
    (loop [result ""
           last-idx 0
           current-footnotes footnotes]
      (if (.find matcher)
        (let [start (.start matcher)
              match (.group matcher)
              groups (map #(.group matcher %) (range 1 (inc (.groupCount matcher))))
              [replacement new-footnotes] (apply handler-fn current-footnotes match groups)
              new-result (str result
                              (subs text last-idx start)
                              replacement)]
          (recur new-result (.end matcher) new-footnotes))
        ;; Add the remaining text after the last match
        [(str result (subs text last-idx)) current-footnotes]))))

;; Handle sequential footnotes [fn:name]
(defn process-sequential-footnotes [text footnotes]
  (process-matches text footnotes #"\[fn:(\w+)\]"
                   (fn [fns match key]
                     (let [k (keyword key)
                           content (first (get fns k))
                           remaining (rest (get fns k))
                           new-fns (if (empty? remaining)
                                     (dissoc fns k)
                                     (assoc fns k remaining))]
                       [(format-footnote k content) new-fns]))))

;; Replace inline footnotes [fn::text]
(defn replace-inline-footnotes [{:keys [text] :as line}]
  (str/replace text #"\[fn::(.+?)\]" (fn [[_ txt]] (format-footnote :ftn txt))))

;; Main function that combines both types of footnote processing
(defn replace-footnotes [{:keys [text] :as line} footnotes]
  (let [text-with-inline-replaced (replace-inline-footnotes line)
        [updated-text updated-footnotes] (process-sequential-footnotes text-with-inline-replaced footnotes)]

    ;; Return both the updated line and the updated footnotes
    [(assoc line :text updated-text) updated-footnotes]))
