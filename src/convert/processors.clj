(ns convert.processors
  (:require [clojure.string :as str]
            [convert.string :as cs]
            [convert.footnotes :as fn]
            [convert.helpers :refer [dbg]]
            [convert.org :as org]))

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

(defn block-start? [{:keys [file lineno text] :as line}]
  (when-let [[_ block-type raw-args] (re-find #"(?i)^#\+begin_(\w+)\s*(.*)" text)]
    (let [[title args] (org/parse-org-block-args raw-args)]
      (when-let [env (match-block-type (str/lower-case block-type) args)]
        {:env env :title title :args args}))))

(defn block-end? [{:keys [file lineno text] :as line}]
  (boolean (re-find #"(?i)^#\+end_(\w+)" text)))

(defn heading-line? [{:keys [file lineno text] :as line}]
  (re-matches #"^\*+ .+" text))

;; TODO: Handle this in TeX
;; Moc roztahany.
(defn chapter-case [s]
  (str/replace (cs/title-case s) ":" "\\colon~"))

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
        format-fn (if (<= level 2) chapter-case cs/sentence-case)]
    ["" (str "\\" heading-type "[" (cs/slugify title) "]{" (format-fn (convert-utf-8 title)) "}")]))

(defn process-text-line [{:keys [file lineno text] :as line} state context]
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
          (recur rest-lines (into out [""]) restant-footnotes state active-block)

          (comment-line? (:text line))
          (recur rest-lines (into out (process-comment-line line)) restant-footnotes state active-block)

          (block-start? line)
          (let [[context lines] (process-block-start-line line)
                new-out (when (= state :list) ["\\stopitemize\n"] [])]
            (recur rest-lines (into out (into (vec new-out) lines)) restant-footnotes :block context))

          (block-end? line)
          (recur rest-lines (into out (process-block-end-line line active-block)) restant-footnotes :normal nil)

          (heading-line? line)
          (let [new-out (if (= state :list) (conj out "\\stopitemize\n") out)]
            (recur rest-lines (into new-out (process-heading-line line)) restant-footnotes :normal nil))

          (list-item? line)
          (let [new-out (if (= state :list) out (conj out "\n\\startitemize"))]
            (recur rest-lines (into new-out (process-list-item-line line)) restant-footnotes :list nil))

          :else
          (let [new-out (if (= state :list) (conj out "\\stopitemize\n") out)
                [updated-line updated-footnotes] (fn/replace-footnotes line restant-footnotes)]
            (recur rest-lines (into new-out (process-text-line updated-line state active-block)) updated-footnotes :normal active-block)))))))
