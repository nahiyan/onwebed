(ns core
  (:require
   [fs :refer [readFileSync writeFileSync readdirSync]]
   [path :refer [extname join]]))

(defn document?
  [name]
  (= ".od" (extname name)))

(defn public? [name]
  (and (not= "_" (first name)) (document? name)))

(defn content
  [document-path]
  (readFileSync document-path "utf8"))

(defn source-items
  [directory]
  (readdirSync directory "utf8"))

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