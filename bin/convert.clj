#!/usr/bin/env bb

;; FIXME: lineno doesn't quite work, we need to pass not just lines, but:
;; [{:file "chapters/01-introduction.org :line 1"} "* Introduction"]}

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
  {"disclaimer" :disclaimer, "warning" :warning, "quote" :blockquote, "note" :note})

(defn match-block-type [type args]
  (if-let [match (or (get block-types type)
                     (and (= (keyword type) :example)
                          (cond
                            (not (contains? args :type)) :tip
                            (= (:type args) "term") :definition)))]
    (do
      match)

    (throw (ex-info "Unknown block type" {:type type :args args}))))

(def heading-levels
  {1 "part" 2 "chapter" 3 "section" 4 "subsection" 5 "subsubsection"})

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

(defn convert-inline [footnotes line]
  (-> line
      ;; TeX comments (# \page).
      (str/replace #"^\s*#\s+" "")

      convert-utf-8

      ;; Footnotes
      (replace-footnotes footnotes)

      ;; Italic: /italic/
      (str/replace #"(?<=^|\s)/(.+?)/(?=\s|\.|,|;|:|$)" "{\\\\em $1}")

      ;; Bold: *bold*
      (str/replace #"(?<=^|\s)\*(.+?)\*(?=\s|\.|,|;|:|$)" "{\\\\bf $1}")

      ;; Links
      (str/replace #"\[\[.*?\]\[.*?\]\]|\[\[.*?\]\]"
                   (fn [m] (process-org-link m)))))

(defn process-comment-line [line lineno] nil)

(defn process-block-start-line [line lineno]
  (let [{:keys [env title args] :as context} (block-start? line)]
    [context

     ["" (str "\\start" (name env))

      (when (= env :blockquote)
        "\\quotationblock")

      (when (and (not (= env :blockquote)) title)
        (str "\\" (name env) "title{" title "}"))]]))

(defn process-block-end-line [line context lineno]
  (if context
    (if (= (:env context) :blockquote)
      [(when (:title context) (str "\\author{" (:title context) "}"))
       "\\stopblockquote" ""]
      [(str "\\stop" (name (:env context))) ""])
    (let [messages
          ["Found end of block, but no active block"
           (pr-str {:line line :lineno lineno :context context})]]
      (throw (ex-info (str/join "\n\n" messages) {:babashka/exit 1})))))

(defn process-heading-line [line lineno]
  (let [level (count (re-find #"^\*+" line))
        title (str/trim (subs line level))
        heading-type (get heading-levels level)
        format-fn (if (<= level 2) chapter-case sentence-case)]
    ["" (str "\\" heading-type "[" (slugify title) "]{" (format-fn (convert-utf-8 title)) "}")]))

(defn process-text-line [line state context footnotes lineno]
  (when (not (empty? line))
    (if (= (:env context) :blockquote)
      [(str "\\quoteline{" (convert-utf-8 line) "}")]
      [(convert-inline footnotes line)])))

(defn process-list-item-line [line lineno]
  [(str "\\item " (str/trim (subs line 1)))])

(defn process-lines [lines & {:keys [footnotes]}]
  (loop [lines lines
         lineno 0
         out []
         state :normal
         active-block nil]
    (if (empty? lines)
      (if (= state :list) (conj out "\\stopitemize\n") out)
      (let [line (first lines)
            lineno (inc lineno)
            rest-lines (rest lines)]
        (cond
          (empty? line)
          (recur rest-lines lineno (into out [""]) state active-block)

          (comment-line? line)
          (recur rest-lines lineno (into out (process-comment-line line lineno)) state active-block)

          (block-start? line)
          (let [[context lines] (process-block-start-line line lineno)
                new-out (when (= state :list) ["\\stopitemize\n"] [])]
            (recur rest-lines lineno (into out (into (vec new-out) lines)) :block context))

          (block-end? line)
          (recur rest-lines lineno (into out (process-block-end-line line active-block lineno)) :normal nil)

          (heading-line? line)
          (let [new-out (if (= state :list) (conj out "\\stopitemize\n") out)]
            (recur rest-lines lineno (into new-out (process-heading-line line lineno)) :normal nil))

          (list-item? line)
          (let [new-out (if (= state :list) out (conj out "\n\\startitemize"))]
            (recur rest-lines lineno (into new-out (process-list-item-line line lineno)) :list nil))

          :else
          (let [new-out (if (= state :list) (conj out "\\stopitemize\n") out)]
            (recur rest-lines lineno (into new-out (process-text-line line state active-block footnotes lineno)) :normal active-block)))))))

(defn read-all-chapters [dir]
  (str/join "\n\n" (map (comp slurp str) (sort (fs/glob dir "*.org")))))

(defn -main [& args]
  (let [chapters-dir "chapters"
        input (read-all-chapters chapters-dir)
        lines (map str/trim (str/split-lines input))
        {:keys [lines footnotes]} (collect-footnotes-and-strip lines)
        processed (process-lines lines :footnotes footnotes)
        output (str/trim (str/replace (str/join "\n" processed) #"\n{2,}" "\n\n"))]
    (println output)))

;; Allow running as script
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
