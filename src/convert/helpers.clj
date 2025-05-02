(ns convert.helpers)

;; Bypass STDOUT redirect by debugging onto STDERR.
(defn dbg [& args]
  (binding [*out* *err*] (apply prn args)))
