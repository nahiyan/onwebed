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
  [contents documents destination]
  (let [content (first contents)
        document (first documents)
        restOfContents (rest contents)
        restOfDocuments (rest documents)]
    (save-compiled-document content document destination)
    (when (seq restOfContents)
      (save-compiled-documents restOfContents restOfDocuments destination))))

(defn get-content
  [documentPath]
  (readFileSync documentPath "utf8"))

(defn compile_
  ([source destination]
   ;;  Create destination directory if it doesn't exist
   (when (not (existsSync destination)) (mkdirSync destination))
   (let
    [sourceItems (readdirSync source "utf8")
     document-names (filter document? sourceItems)
     document-paths (map (fn [document] (join source document)) document-names)
     document-contents (map get-content document-paths)
     compiled-documents (map html/from-document-content document-contents)]
    ;;  (println (js->clj (to-html-elements "base.od" "site" nil)))
     (save-compiled-documents compiled-documents document-names destination))))