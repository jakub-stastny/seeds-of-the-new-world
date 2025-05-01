#!/usr/bin/env bb

(ns convert
  (:require [clojure.string :as str]
            [babashka.fs :as fs]))

;; Bypass STDOUT redirect.
(defn dbg [& args]
  (binding [*out* *err*] (apply prn args)))

(defn collect-footnotes-and-strip [lines]
  (reduce (fn [{:keys [lines footnotes]} line]
            (if-let [[_ key txt] (re-matches #".*\[fn:([^\]]+)\]\s+(.*)" line)]
              {:lines lines
               :footnotes (assoc footnotes key txt)}
              {:lines (conj lines line)
               :footnotes footnotes}))
          {:lines [] :footnotes {}}
          lines))

(defn replace-footnotes [line footnotes]
  (-> line
      ;; inline: [fn::text]
      (str/replace #"\[fn::(.*?)\]"
                   (fn [[_ txt]] (str "\\footnote{" txt "}")))
      ;; named: [fn:1]
      (str/replace #"\[fn:([^\]]+)\]"
                   (fn [[_ key]]
                     (let [footnote (get footnotes key)]
                       (when-not footnote
                         (throw (ex-info "Footnote not found" {:key key})))

                       (if (str/starts-with? key "ref:")
                         (str "\\ref{\\goto{" footnote "}[url(" footnote ")]}")
                         (str "\\footnote{" footnote "}")))))))

(def block-types
  {"disclaimer" "disclaimer"
   "warning" "warning"
   "quote" "blockquote"
   "note" "note"})

(defn match-block-type [type args]
  (or (get block-types type)
      (and (= type "example")
           (cond
             (not (contains? args :type)) "tip"
             (= (:type args) "term") "definition"

             true (throw (ex-info "Unknown block type" {:type type :args args}))))))

(def heading-levels
  {1 "part"
   2 "chapter"
   3 "section"
   4 "subsection"
   5 "subsubsection"})

(defn comment-line? [line]
  (boolean (or (re-find #"^\s*#(?!\s+\\) " line)
               (re-find #"@@comment:" line))))

(defn list-item? [line]
  (re-matches #"^\s*-\s+.+$" line))

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

(defn block-start? [line]
  (when-let [[_ block-type raw-args] (re-find #"(?i)^#\+begin_(\w+)\s*(.*)" line)]
    (let [[title args] (parse-org-block-args raw-args)]
      (when-let [env (match-block-type (str/lower-case block-type) args)]
        {:env env :title title :args args}))))

(defn block-end? [line]
  (boolean (re-find #"(?i)^#\+end_(\w+)" line)))

(defn heading-line? [line]
  (re-matches #"^\*+ .+" line))

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

;; Moc roztahany.
(defn chapter-case [s]
  (str/replace (title-case s) ":" "\\colon~"))

(defn sentence-case [s]
  (let [words (str/split (str/trim s) #"\s+")]
    (if (empty? words)
      ""
      (str (let [first-word (first words)]
             (if (not (lowercase-or-titlecase? first-word))
               first-word
               (str (str/upper-case (subs first-word 0 1))
                    (str/lower-case (subs first-word 1)))))
           (when (> (count words) 1)
             (str " " (->> (rest words)
                           (map (fn [word]
                                  (if (not (lowercase-or-titlecase? word))
                                    word
                                    (if (= word (str/lower-case word))
                                      (str/lower-case word)
                                      word)))) ; preserve TitleCase words!
                           (str/join " "))))))))

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

(defn convert-inline [footnotes line]
  (-> line
      ;; TeX comments (# \page).
      (str/replace #"^\s*#\s+" "")
      ;; ;; Footnotes
      (replace-footnotes footnotes)
      ;; Quotes
      (str/replace #"“([^”]+)”" "\\\\quotation{$1}")
      (str/replace #"\"([^\"]+)\"" "\\\\quotation{$1}")
      ;; Apostrophes
      (str/replace #"’" "'")
      ;; Italic: /italic/
      (str/replace #"(?<=^|\s)/(.+?)/(?=\s|\.|,|;|:|$)" "\\\\emph{$1}")
      ;; Bold: *bold*
      (str/replace #"(?<=^|\s)\*(.+?)\*(?=\s|\.|,|;|:|$)" "{\\\\bf }")
      ;; Links
      (str/replace #"\[\[.*?\]\[.*?\]\]|\[\[.*?\]\]"
                   (fn [m] (process-org-link m)))))

(defn process-comment-line [line] nil)

(defn process-block-start-line [line]
  (let [{:keys [env title args] :as context} (block-start? line)]
    [context
     (cond
       (= env "blockquote")
       ["" "\\startblockquote" "\\scale[factor=27]{\\symbol[leftquotation]}" "\\vskip -1cm"]

       title
       ["" (str "\\start" env) (str "\\" env "title{" title "}")]

       :else
       ["" (str "\\start" env)])]))

(defn process-block-end-line [line context]
  (if context
    (if (= (:env context) "blockquote")
      [(when (:title context) (str "\\author{" (:title context) "}"))
       "\\stopblockquote" ""]
      [(str "\\stop" (:env context)) ""])
    (throw (ex-info "Found end of block, but no active block" {:babashka/exit 1}))))

(defn process-heading-line [line]
  (let [level (count (re-find #"^\*+" line))
        title (str/trim (subs line level))
        heading-type (get heading-levels level)
        format-fn (if (<= level 2) chapter-case sentence-case)]
    ["" (str "\\" heading-type "[" (slugify title) "]{" (format-fn title) "}")]))

(defn process-text-line [line state context footnotes]
  (when (not (empty? line))
    (let [processed-line (convert-inline footnotes line)]
      (if (= (:env context) "blockquote")
        [(str "\\quoteline{" processed-line "}")]
        [processed-line]))))

(defn process-list-item-line [line]
  [(str "\\item " (str/trim (subs line 1)))])

(defn process-lines [lines & {:keys [footnotes]}]
  (loop [lines lines
         out []
         state :normal
         active-block nil]
    (if (empty? lines)
      (if (= state :list)
        (conj out "\\stopitemize\n")
        out)
      (let [line (first lines)
            rest-lines (rest lines)]
        (cond
          (comment-line? line)
          (recur rest-lines (into out (process-comment-line line)) state active-block)

          (block-start? line)
          (let [[context lines] (process-block-start-line line)
                new-out (when (= state :list) ["\\stopitemize\n"] [])]
            (recur rest-lines (into out (into (vec new-out) lines)) :block context))

          (block-end? line)
          (recur rest-lines (into out (process-block-end-line line active-block)) :normal nil)

          (heading-line? line)
          (let [new-out (if (= state :list) (conj out "\\stopitemize\n") out)]
            (recur rest-lines (into new-out (process-heading-line line)) :normal nil))

          (list-item? line)
          (let [new-out (if (= state :list) out (conj out "\n\\startitemize"))]
            (recur rest-lines (into new-out (process-list-item-line line)) :list nil))

          :else
          (let [new-out (if (= state :list) (conj out "\\stopitemize\n") out)]
            (recur rest-lines (into new-out (process-text-line line state active-block footnotes)) :normal active-block)))))))

(defn read-all-chapters [dir]
  (str/join "\n\n" (map (comp slurp str) (sort (fs/glob dir "*.org")))))

(defn -main [& args]
  (let [chapters-dir "chapters"
        input (read-all-chapters chapters-dir)
        lines (str/split-lines input)
        {:keys [lines footnotes]} (collect-footnotes-and-strip lines)
        processed (process-lines lines :footnotes footnotes)
        output (str/join "\n" processed)]
    (println output)))

;; Allow running as script
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
