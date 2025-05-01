#!/usr/bin/env bb -cp src

(ns convert
  (:require [clojure.string :as str]
            [convert.string :as cs]
            [convert.processors :as pc]
            [convert.footnotes :as fn]
            [babashka.fs :as fs]))

;; Bypass STDOUT redirect.
(defn dbg [& args]
  (binding [*out* *err*] (apply prn args)))

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
        {:keys [lines footnotes]} (fn/collect-footnotes-and-strip lines)
        processed (pc/process-lines lines :footnotes footnotes)
        output (str/trim (str/replace (str/join "\n" processed) #"\n{2,}" "\n\n"))]
    (println output)))

;; Allow running as script
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
