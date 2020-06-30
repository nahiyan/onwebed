(ns onwebed-cli.compiler.document
  (:require [cljs.nodejs :as nodejs]
            [path :refer [extname join]]
            [xml-js :refer [xml2js js2xml]]
            [xml-formatter :as xml-format]
            [onwebed-cli.compiler.xml :as xml]
            [onwebed-cli.compiler.bones.bones :as bones]
            [onwebed-cli.compiler.flesh-items :as flesh-items]
            [fs :refer [readFileSync readdirSync mkdirSync existsSync writeFileSync]]))

(nodejs/enable-util-print!)

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

(defn content-to-html-elements
  [content]
  (let
   [xml-elements (js->clj (xml2js content) :keywordize-keys true)
    document-body (filter (fn [element]
                            (= (get element :name) "document_body"))
                          (get xml-elements :elements))
    document-body-content (if (not= nil document-body)
                            (get (first document-body) :elements)
                            '())
    bones-and-flesh (xml/to-bones-and-flesh document-body-content)
    bones (filter (fn
                    [item]
                    (= (get item :type) "bone"))
                  bones-and-flesh)
    flesh-items (filter (fn
                          [item]
                          (= (get item :type) "flesh"))
                        bones-and-flesh)
    descriptor-element-targets (flesh-items/to-descriptor-element-targets flesh-items)
    html-elements (clj->js {:elements (bones/to-html-elements bones descriptor-element-targets)})]
    html-elements))

;; Process document to HTML elements
(defn to-html-elements
  [name source targets]
  (let
   [document-path (join source name)
    document-content (get-content document-path)
    html-elements (content-to-html-elements document-content)]
    html-elements))

;; Take document content and convert it to HTML
(defn to-html
  [content]
  (let
   [html-elements (content-to-html-elements content)]
    ;;  (println (xml-format (js2xml xml-js-object)))
    (xml-format (js2xml html-elements))))

(defn compile_
  ([source destination]
   ;;  Create destination directory if it doesn't exist
   (when (not (existsSync destination)) (mkdirSync destination))
   (let
    [sourceItems (readdirSync source "utf8")
     document-names (filter document? sourceItems)
     document-paths (map (fn [document] (join source document)) document-names)
     document-contents (map get-content document-paths)
     compiled-documents (map to-html document-contents)]
     (println (js->clj (to-html-elements "base.od" "onwebed-cli-site" nil)))
     (save-compiled-documents compiled-documents document-names destination))))