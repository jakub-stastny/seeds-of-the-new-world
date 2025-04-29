#!/usr/bin/env bb

(ns convert
  (:require [clojure.string :as str]
            [babashka.fs :as fs]))

(def block-types
  {"disclaimer" "disclaimer"
   "warning" "warning"
   "quote" "blockquote"
   "note" "note"})

(def heading-levels
  {1 "part"
   2 "chapter"
   3 "section"
   4 "subsection"
   5 "subsubsection"})

(defn comment-line? [line]
  (boolean (re-find #"^\s*#\s+" line)))

(defn list-item? [line]
  (re-matches #"^\s*-\s+.+$" line))

(defn block-start? [line]
  (some->> (re-find #"(?i)^#\+begin_(\w+)" line)
           (second)
           (str/lower-case)
           (get block-types)))

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
         (map-indexed (fn [i word]
                        (cond
                          (not (lowercase-or-titlecase? word)) word ; leave weird or ALL-CAPS untouched
                          (or (= i 0) (= i (dec count-words)) (not (stopwords (str/lower-case word))))
                          (str/capitalize (str/lower-case word))
                          :else
                          (str/lower-case word))))
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
                                    (str/lower-case word))))
                           (str/join " "))))))))

(defn process-heading [line]
  (let [level (count (re-find #"^\*+" line))
        title (str/trim (subs line level))
        heading-type (get heading-levels level)
        format-fn (if (<= level 2) chapter-case sentence-case)]
    (when heading-type
      (str "\\" heading-type "{" (format-fn title) "}"))))

(defn convert-inline [line]
  (-> line
      ;; Footnotes
      (str/replace #"\[fn::([^\]]+)\]" "\\\\footnote{$1}")
      ;; Quotes
      (str/replace #"“([^”]+)”" "\\\\quotation{$1}")
      (str/replace #"\"([^\"]+)\"" "\\\\quotation{$1}")
      ;; Apostrophes
      (str/replace #"’" "'")
      ;; Italic: /italic/
      (str/replace #"(?<!\w)/(.*?)/(?!\w)" "\\\\emph{$1}")
      ;; Bold: *bold*
      (str/replace #"(?<!\w)\*(.*?)\*(?!\w)" "{\\\\bf $1}")))

(defn process-lines [lines]
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
          (recur rest-lines out state)

          ;; Start of a block
          (block-start? line)
          (do
            (let [env (block-start? line)
                  new-out (if (= state :list) (conj out "\\stopitemize") out)
                  start-block (if (= env "blockquote")
                                ["\\startblockquote" "\\scale[factor=27]{\\symbol[leftquotation]}" "\\vskip -1cm"]
                                [(str "\\start" env)])]
              (recur rest-lines (into new-out start-block) :block)))

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
              (recur rest-lines (conj new-out (convert-inline line)) :normal))))))))

(defn read-all-chapters [dir]
  (str/join "\n\n" (map (comp slurp str) (sort (fs/glob dir "*.org")))))

(defn -main []
  (let [chapters-dir "chapters"
        input (read-all-chapters chapters-dir)
        lines (str/split-lines input)
        processed (process-lines lines)
        output (str/join "\n" processed)]
    (println output)))

;; Allow running as script
(when (= *file* (System/getProperty "babashka.file"))
  (-main))
