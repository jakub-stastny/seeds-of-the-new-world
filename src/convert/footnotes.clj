(ns convert.footnotes
  (:require [clojure.string :as str]))

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

;; (if (str/starts-with? key "ref:")
;;   (str "\\ref{\\goto{" footnote "}[url(" footnote ")]}")
;;   (str "\\footnote{" footnote "}"))

(def footnote-types
  {:ref #(str "\\ref{\\goto{" % "}[url(" % ")]}")
   :ftn #(str "\\footnote{" % "}")})

(defn format-footnote [key item]
  (if-let [formatter (get footnote-types key)]
    (formatter item)
    (throw (str "No such footnote key " key))))

;; TODO: What are the right quotes? These look good: “basket”

;; (defn replace-footnotes [{:keys [file lineno text] :as line} footnotes]
;;   (-> text
;;       ;; Inline: [fn::text].
;;       (str/replace #"\[fn::(.+?)\]" (fn [[_ txt]] (format-footnote :ftn txt)))

;;       ;; Sequential: [fn:ftn] and [fn:ref].
;;       (str/replace #"\[fn:(\w+)\]"
;;                    (fn [[_ key]]
;;                      (let [key (keyword key)
;;                            list (get footnotes key)]
;;                        (format-footnote key (first list))))))

;;   ;; [(assoc footnotes key rest)]
;;   )

(defn replace-inline-footnotes [{:keys [text] :as line}]
  (str/replace text #"\[fn::(.+?)\]" (fn [[_ txt]] (format-footnote :ftn txt))))

(defn replace-footnotes [{:keys [text] :as line} footnotes]
  [footnotes (assoc line :text (replace-inline-footnotes line))]

  ;; (letfn [(process-matches [text footnotes pattern handler-fn]
  ;;           (let [matcher (re-matcher pattern text)]
  ;;             (loop [result ""
  ;;                    last-idx 0
  ;;                    current-footnotes footnotes]
  ;;               (if (.find matcher)
  ;;                 (let [start (.start matcher)
  ;;                       match (.group matcher)
  ;;                       groups (map #(.group matcher %) (range 1 (inc (.groupCount matcher))))
  ;;                       [replacement new-footnotes] (apply handler-fn current-footnotes match groups)
  ;;                       new-result (str result
  ;;                                       (subs text last-idx start)
  ;;                                       replacement)]
  ;;                   (recur new-result (.end matcher) new-footnotes))
  ;;                 ;; Add the remaining text after the last match
  ;;                 [(str result (subs text last-idx)) current-footnotes]))))]

  ;;   ;; First process inline footnotes using the extracted function
  ;;   (let [text-with-inline-replaced (replace-inline-footnotes line)

  ;;         ;; Then process sequential footnotes [fn:name]
  ;;         [text2 footnotes2]
  ;;         (process-matches text-with-inline-replaced footnotes #"\[fn:(\w+)\]"
  ;;                          (fn [fns match key]
  ;;                            (let [k (keyword key)
  ;;                                  content (first (get fns k))
  ;;                                  remaining (rest (get fns k))
  ;;                                  new-fns (if (empty? remaining)
  ;;                                            (dissoc fns k)
  ;;                                            (assoc fns k remaining))]
  ;;                              [(format-footnote k content) new-fns])))]

  ;;     ;; Return both the updated line and the updated footnotes
  ;;     [(assoc line :text text2) footnotes2]))
  )
