(ns onwebed-cli.compiler.document
  (:require
   [path :refer [extname join]]
   [onwebed-cli.compiler.html.html :as html]
   [fs :refer [readFileSync readdirSync mkdirSync existsSync writeFileSync]]))

(defn document? [name]
  (= ".od" (extname name)))

(defn save-compiled-document
  [content name destination]
  (let
   [filePath (join destination (str name ".html"))]
    (writeFileSync filePath content)))

(defn save-compiled-documents
  [content-items documents destination]
  (let [content-item (first content-items)
        document (first documents)
        rest-of-content-items (rest content-items)
        rest-of-documents (rest documents)]
    (save-compiled-document content-item document destination)
    (when (seq rest-of-content-items)
      (save-compiled-documents rest-of-content-items rest-of-documents destination))))

(defn get-content
  [document-path]
  (readFileSync document-path "utf8"))

(defn compile_
  ([source-directory destination]
   ;;  Create destination directory if it doesn't exist
   (when (not (existsSync destination)) (mkdirSync destination))
   (let
    [source-items (readdirSync source-directory "utf8")
     document-names (filter document? source-items)
     document-paths (map (fn [document] (join source-directory document)) document-names)
     document-contents (map get-content document-paths)
     compiled-documents (map (fn [document-content]
                               (html/from-document-content document-content
                                                           source-directory))
                             document-contents)]
    ;;  (println (js->clj (to-html-elements "base.od" "site" nil)))
     (save-compiled-documents compiled-documents document-names destination))))