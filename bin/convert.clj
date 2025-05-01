#!/usr/bin/env bb

(ns convert
  (:require [clojure.string :as str]
            [babashka.fs :as fs]))

;; Bypass STDOUT redirect.
(defn dbg [& args]
  (binding [*out* *err*] (apply prn args)))

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

(defn replace-footnotes [{:keys [text] :as line} footnotes]
  (let [;; First handle inline footnotes: [fn::text]
        [new-text1 _] (reduce
                       (fn [[txt _] [match content]]
                         [(str/replace txt match (format-footnote :ftn content)) nil])
                       [text nil]
                       (re-seq #"\[fn::(.+?)\]" text))

        ;; Then handle sequential footnotes: [fn:ftn]
        [new-text2 updated-footnotes] (reduce
                                       (fn [[txt fns] [match key]]
                                         (let [k (keyword key)
                                               content (first (get fns k))
                                               remaining (rest (get fns k))
                                               new-fns (if (empty? remaining)
                                                         (dissoc fns k)
                                                         (assoc fns k remaining))]
                                           [(str/replace txt match (format-footnote k content))
                                            new-fns]))
                                       [new-text1 footnotes]
                                       (re-seq #"\[fn:(\w+)\]" new-text1))]

    ;; Return both the updated line and the updated footnotes
    [(assoc line :text new-text2) updated-footnotes]))

(def block-types
  {"disclaimer" :disclaimer, "warning" :warning, "quote" :blockquote, "note" :note})

(defn match-block-type [type args]
  (if-let [match (or (get block-types type)
                     (and (= (keyword type) :example)
                          (cond
                            (not (contains? args :type)) :tip
                            (= (:type args) "term") :definition)))]
    (do match)

    (throw (ex-info "Unknown block type" {:type type :args args}))))

(def heading-levels
  {1 "part" 2 "chapter" 3 "section" 4 "subsection" 5 "subsubsection"})

(defn comment-line? [line]
  (boolean (or (re-find #"^\s*#(?!\s+\\) " line)
               (re-find #"@@comment:" line))))

(defn list-item? [{:keys [file lineno text] :as line}]
  (re-matches #"^\s*-\s+.+$" text))

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

(defn block-start? [{:keys [file lineno text] :as line}]
  (when-let [[_ block-type raw-args] (re-find #"(?i)^#\+begin_(\w+)\s*(.*)" text)]
    (let [[title args] (parse-org-block-args raw-args)]
      (when-let [env (match-block-type (str/lower-case block-type) args)]
        {:env env :title title :args args}))))

(defn block-end? [{:keys [file lineno text] :as line}]
  (boolean (re-find #"(?i)^#\+end_(\w+)" text)))

(defn heading-line? [{:keys [file lineno text] :as line}]
  (re-matches #"^\*+ .+" text))

(def stopwords
  #{"a" "an" "the" "and" "but" "or" "for" "nor" "on" "at" "to" "from" "by" "of" "in" "with" "over"})

(defn all-caps? [word]
  (re-matches #"[A-Z0-9\+\-]+" word))

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

;; TODO: Handle this in TeX
;; Moc roztahany.
(defn chapter-case [s]
  (str/replace (title-case s) ":" "\\colon~"))

(defn sentence-case [s]
  (str/replace s #"^." #(str/upper-case %)))

(defn slugify [s]
  (-> s
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"(^-|-$)" "")))

;; [[https://example.com][Example Site]]
;; \goto{Example Site}[url(https://example.com)]
;;
;; [[#intro][Introduction Section]]
;; \goto{Introduction Section}[intro]
;;
;; Created by: \chapter[chapter-id]{Chapter Title}
(defn process-org-link [link]
  (cond
    ;; [[https://example.com][Custom Text]]
    (re-matches #"\[\[(.*?)\]\[(.*?)\]\]" link)
    (let [[_ target text] (re-matches #"\[\[(.*?)\]\[(.*?)\]\]" link)]
      (if (re-matches #"https?://.*" target)
        (str "\\goto{" text "}[url(" target ")]")
        (let [clean-target (if (str/starts-with? target "#")
                             (subs target 1)
                             target)]
          (str "\\goto{" text "}[" clean-target "]"))))

    ;; [[https://example.com]]
    (re-matches #"\[\[(https?://.*?)\]\]" link)
    (let [[_ target] (re-matches #"\[\[(https?://.*?)\]\]" link)]
      (str "\\goto{" target "}[url(" target ")]"))

    :else link))

(defn convert-utf-8 [line]
  (-> line
      ;; Quotes
      (str/replace #"“([^”]+)”" "\\\\quotation{$1}")
      (str/replace #"\"([^\"]+)\"" "\\\\quotation{$1}")

      ;; Apostrophes
      (str/replace #"’" "'")))

(defn convert-inline [{:keys [file lineno text] :as line}]
  (-> text
      ;; TeX comments (# \page).
      (str/replace #"^\s*#\s+" "")

      convert-utf-8

      ;; Italic: /italic/
      (str/replace #"(?<=^|\s)/(.+?)/(?=\s|\.|,|;|:|$)" "{\\\\em $1}")

      ;; Bold: *bold*
      (str/replace #"(?<=^|\s)\*(.+?)\*(?=\s|\.|,|;|:|$)" "{\\\\bf $1}")

      ;; Links
      (str/replace #"\[\[.*?\]\[.*?\]\]|\[\[.*?\]\]" #(process-org-link %))))

(defn process-comment-line [line] nil)

(defn process-block-start-line [{:keys [file lineno text] :as line}]
  (let [{:keys [env title args] :as context} (block-start? line)]
    [context

     ["" (str "\\start" (name env))

      (when (= env :blockquote)
        "\\quotationblock")

      (when (and (not (= env :blockquote)) title)
        (str "\\" (name env) "title{" title "}"))]]))

(defn process-block-end-line [{:keys [file lineno text] :as line} context]
  (if context
    (if (= (:env context) :blockquote)
      [(when (:title context) (str "\\author{" (:title context) "}"))
       "\\stopblockquote" ""]
      [(str "\\stop" (name (:env context))) ""])
    (let [messages
          ["Found end of block, but no active block"
           (pr-str (assoc line :context context))]]
      (throw (ex-info (str/join "\n\n" messages) {:babashka/exit 1})))))

(defn process-heading-line [{:keys [file lineno text] :as line}]
  (let [level (count (re-find #"^\*+" text))
        title (str/trim (subs text level))
        heading-type (get heading-levels level)
        format-fn (if (<= level 2) chapter-case sentence-case)]
    ["" (str "\\" heading-type "[" (slugify title) "]{" (format-fn (convert-utf-8 title)) "}")]))

(defn process-text-line [{:keys [file lineno text] :as line} state context footnotes]
  (when (not (empty? text))
    (if (= (:env context) :blockquote)
      [(str "\\quoteline{" (convert-utf-8 text) "}")]
      [(convert-inline line)])))

(defn process-list-item-line [{:keys [file lineno text] :as line}]
  [(str "\\item " (str/trim (subs text 1)))])

(defn process-lines [lines & {:keys [footnotes]}]
  (loop [lines lines
         out []
         restant-footnotes footnotes
         state :normal
         active-block nil]
    (if (empty? lines)
      (if (= state :list) (conj out "\\stopitemize\n") out)

      (let [line (first lines)
            rest-lines (rest lines)]
        (cond
          (empty? (:text line))
          (recur rest-lines (into out [""]) footnotes state active-block)

          (comment-line? (:text line))
          (recur rest-lines (into out (process-comment-line line)) footnotes state active-block)

          (block-start? line)
          (let [[context lines] (process-block-start-line line)
                new-out (when (= state :list) ["\\stopitemize\n"] [])]
            (recur rest-lines (into out (into (vec new-out) lines)) footnotes :block context))

          (block-end? line)
          (recur rest-lines (into out (process-block-end-line line active-block)) footnotes :normal nil)

          (heading-line? line)
          (let [new-out (if (= state :list) (conj out "\\stopitemize\n") out)]
            (recur rest-lines (into new-out (process-heading-line line)) footnotes :normal nil))

          (list-item? line)
          (let [new-out (if (= state :list) out (conj out "\n\\startitemize"))]
            (recur rest-lines (into new-out (process-list-item-line line)) footnotes :list nil))

          :else
          (let [new-out (if (= state :list) (conj out "\\stopitemize\n") out)
                updated-footnotes (replace-footnotes line footnotes)]
            (recur rest-lines (into new-out (process-text-line line state active-block footnotes)) updated-footnotes :normal active-block)))))))

(defn read-all-chapters [dir]
  (str/join "\n\n" (map (comp slurp str) (sort (fs/glob dir "*.org")))))


(defn read-all-chapters [dir]
  (let [paths (sort (map str (fs/glob dir "*.org")))]
    (mapcat (fn [path]
              (map-indexed
               (fn [i line] {:file path :lineno (inc i) :text (str/trim line)})
               (str/split-lines (slurp path))))
            paths)))

(defn -main [& args]
  (let [chapters-dir "chapters"
        lines (read-all-chapters chapters-dir)
        {:keys [lines footnotes]} (collect-footnotes-and-strip lines)
        processed (process-lines lines :footnotes footnotes)
        output (str/trim (str/replace (str/join "\n" processed) #"\n{2,}" "\n\n"))]
    (println output)))

;; Allow running as script
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
