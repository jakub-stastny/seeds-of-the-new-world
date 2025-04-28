#!/usr/bin/env bb

(ns convert
  (:require [clojure.string :as str]
            [babashka.fs :as fs]))

(def block-types
  {"disclaimer" "disclaimer"
   "warning" "warning"
   "note" "note"})

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

(defn convert-inline [line]
  ;; Simple inline footnote conversion
  (str/replace line #"\[fn::([^\]]+)\]" "\\\\footnote{$1}"))

(defn process-lines [lines]
  (loop [lines lines
         out []
         state :normal]
    (if (empty? lines)
      out
      (let [line (first lines)
            rest-lines (rest lines)]
        (cond
          (block-start? line)
          (recur rest-lines
                 (conj out (str "\\start" (block-start? line)))
                 :block)

          (block-end? line)
          (recur rest-lines
                 (conj out (str "\\stop" (block-end? line)))
                 :normal)

          :else
          (recur rest-lines
                 (conj out (convert-inline line))
                 state))))))

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
