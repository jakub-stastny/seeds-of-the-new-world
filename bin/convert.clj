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

(defn process-heading [line]
  (let [m (re-matches #"^(\*+) (.+)" line)
        stars (count (get m 1))
        title (get m 2)
        command (get heading-levels stars)]
    (str "\\" command "{" title "}")))

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
      ;; if list is open, close it at EOF
      (if (= state :list)
        (conj out "\\stopitemize")
        out)
      (let [line (first lines)
            rest-lines (rest lines)]
        (cond
          ;; Skip comments
          (comment-line? line)
          (recur rest-lines out state)

          ;; block start
          (block-start? line)
          (let [out (if (= state :list) (conj out "\\stopitemize") out)]
            (recur rest-lines
                   (conj out (str "\\start" (block-start? line)))
                   :block))

          ;; block end
          (block-end? line)
          (recur rest-lines
                 (conj out (str "\\stop" (block-end? line)))
                 :normal)

          ;; heading
          (heading-line? line)
          (let [out (if (= state :list) (conj out "\\stopitemize") out)]
            (recur rest-lines
                   (conj out (process-heading line))
                   :normal))

          ;; list item
          (list-item? line)
          (let [out (cond
                      (= state :list) out
                      :else (conj out "\\startitemize"))]
            (recur rest-lines
                   (conj out (str "\\item " (str/trim (subs line 1))))
                   :list))

          ;; normal line
          :else
          (let [out (if (= state :list) (conj out "\\stopitemize") out)]
            (recur rest-lines
                   (conj out (convert-inline line))
                   :normal)))))))

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
