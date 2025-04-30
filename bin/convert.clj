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
   "note" "note"
   ;; If we use a custom block, GH won't show it.
   "example" "tip"})

(def heading-levels
  {1 "part"
   2 "chapter"
   3 "section"
   4 "subsection"
   5 "subsubsection"})

(defn comment-line? [line]
  (boolean (re-find #"^\s*#(?!\s+\\) " line)))

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
  (when-let [[_ block-type params] (re-find #"(?i)^#\+begin_(\w+)\s*(.*)" line)]
    (do
      (let [result (parse-org-block-args params)]
        (when-let [env (get block-types (str/lower-case block-type))]
          (let [trimmed (str/trim params)]
            (cond
              ;; case: quoted title → extract contents
              (re-matches #"^\"(.+)\"$" trimmed)
              {:env env :title (second (re-matches #"^\"(.+)\"$" trimmed))}

              ;; case: non-empty unquoted stuff → treat as args
              (seq trimmed)
              {:env env :args trimmed}

              ;; otherwise
              :else
              {:env env})))))))

(defn block-end? [line]
  (some->> (re-find #"(?i)^#\+end_(\w+)" line)
           (second)
           (str/lower-case)
           (get block-types)))

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

(defn process-heading [line]
  (let [level (count (re-find #"^\*+" line))
        title (str/trim (subs line level))
        heading-type (get heading-levels level)
        format-fn (if (<= level 2) chapter-case sentence-case)]
    (when heading-type
      (str "\\" heading-type "[" (slugify title) "]{" (format-fn title) "}"))))

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

(defn process-lines [footnotes lines]
  (loop [lines lines
         out []
         state :normal]
    (if (empty? lines)
      (if (= state :list)
        (conj out "\\stopitemize")
        out)
      (let [line (first lines)
            rest-lines (rest lines)]
        (cond
          ;; Skip full-line comments
          (comment-line? line)
          (do
            (recur rest-lines out state))

          ;; Start of a block
          (block-start? line)
          (let [{:keys [env title]} (block-start? line)
                new-out (if (= state :list) (conj out "\\stopitemize") out)
                start-block (cond
                              (= env "blockquote")
                              ["\\startblockquote" "\\scale[factor=27]{\\symbol[leftquotation]}" "\\vskip -1cm"]

                              title
                              ;; [(str "\\start" env "[title={" title "}]")] ; If there’s a title, inject it
                              [(str "\\start" env) "\\tiptitle{" title "}"]

                              :else
                              [(str "\\start" env)])]
            (recur rest-lines (into new-out start-block) :block))

          (block-end? line)
          (do
            (let [env (block-end? line)
                  stop-block (if (= env "blockquote")
                               ["\\stopblockquote"]
                               [(str "\\stop" env)])]
              (recur rest-lines (into out stop-block) :normal)))

          ;; Headings
          (heading-line? line)
          (do
            (let [new-out (if (= state :list)
                            (conj out "\\stopitemize")
                            out)]
              (recur rest-lines (conj new-out (process-heading line)) :normal)))

          ;; List items
          (list-item? line)
          (do
            (let [new-out (if (= state :list)
                            out
                            (conj out "\\startitemize"))]
              (recur rest-lines (conj new-out (str "\\item " (str/trim (subs line 1)))) :list)))

          ;; Regular paragraph or inline content
          :else
          (do
            (let [new-out (if (= state :list)
                            (conj out "\\stopitemize")
                            out)]
              (recur rest-lines (conj new-out (convert-inline footnotes line)) :normal))))))))

(defn read-all-chapters [dir]
  (str/join "\n\n" (map (comp slurp str) (sort (fs/glob dir "*.org")))))

(defn -main []
  (let [chapters-dir "chapters"
        input (read-all-chapters chapters-dir)
        lines (str/split-lines input)
        {:keys [lines footnotes]} (collect-footnotes-and-strip lines)
        processed (process-lines footnotes lines)
        output (str/join "\n" processed)]
    (println output)))

;; Allow running as script
(when (= *file* (System/getProperty "babashka.file"))
  (-main))
