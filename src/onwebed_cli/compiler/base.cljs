(ns onwebed-cli.compiler.base
  (:require [cljs.nodejs :as nodejs]
            [path :refer [extname join]]
            [xml-js :refer [xml2js js2xml]]
            [xml-formatter :as xml-format]
            [clojure.string :refer [split]]
            [onwebed-cli.compiler.bones.descriptor :as descriptor]
            [fs :refer [readFileSync readdirSync mkdirSync existsSync writeFileSync]]))

(nodejs/enable-util-print!)

(defn document? [item]
  (= ".od" (extname item)))

(defn save-compiled-document
  [content document destination]
  (let
   [filePath (join destination (str document ".html"))]
    (writeFileSync filePath content)))

(defn save-compiled-documents
  [contents documents destination]
  (let [content (first contents)
        document (first documents)
        restOfContents (rest contents)
        restOfDocuments (rest documents)]
    (save-compiled-document content document destination)
    (if (= restOfContents ()) () (save-compiled-documents restOfContents restOfDocuments destination))))

(defn get-document-content
  [documentPath]
  (readFileSync documentPath "utf8"))

(defn combine-text-elements
  [children]
  (let
   [textElements (filter (fn
                           [element]
                           (let
                            [type (get element :type)]
                             (= type "text")))
                         children)
    texts (map (fn [element] (get element :text)) textElements)]
    (apply str texts)))

; Take XML elements and retrieve bones and flesh in processed form
(defn collect-bones-and-flesh
  [elements]
  (filter
   (fn [item] (not= item nil))
   (map (fn
          [element]
          (let
           [name (get element :name)
            attributes (get element :attributes)
            elements (get element :elements)]
            (if (= name "bone")
              (let
               [children (if (not= elements nil)
                           (collect-bones-and-flesh elements)
                           nil)
                descriptor (get attributes :descriptor)]
                {:type "bone" :children children :descriptor descriptor})
              (if (= name "flesh")
                (let
                 [for_ (get attributes :for)
                  content (combine-text-elements elements)]
                  {:type "flesh" :for for_ :content content})
                nil)))) elements)))

; Generate map of flesh targets and their respective contents
(defn map-targets
  [flesh-items]
  (let
   [content-items (vec
                   (map (fn
                          [item]
                          (get item :content))
                        flesh-items))
    targets (reduce (fn
                      [acc, item]
                      (merge acc
                             (reduce conj
                                     {}
                                     (map (fn
                                            [keyValueList]
                                            (let
                                             [key (first keyValueList)
                                              value (last keyValueList)
                                              existing-key (get acc key)]
                                              (if (not= existing-key nil)
                                                ;; If key already exists
                                                {key (conj existing-key (first value))}
                                                {key value})))
                                          item))))
                    {}
                    (map (fn
                           [flesh-item content-item-index]
                           (let
                            [targets (js->clj (split (get flesh-item :for) #"\s"))]
                             (reduce conj {} (map (fn
                                                    [target]
                                                    {target (vector content-item-index)})
                                                  targets))))
                         flesh-items
                         (range (count content-items))))]
    {:content-items content-items
     :targets targets}))

; Take a bone (XML element), and process it to a form representing HTML elements
(defn process-bone
  [bone mapped-targets]
  (let
   [descriptor (get bone :descriptor)
    children (get bone :children)
    descriptorElements (descriptor/parse descriptor)
    content (map (fn [bone]
                   (process-bone bone mapped-targets))
                 children)
    processed-descriptor-elements (descriptor/process-elements descriptorElements mapped-targets content)]
    processed-descriptor-elements))

(defn process-bones-and-flesh
  [bones-and-flesh]
  (let
   [flesh-items (filter (fn
                          [item]
                          (= (get item :type) "flesh"))
                        bones-and-flesh)
    bones (filter (fn
                    [item]
                    (= (get item :type) "bone"))
                  bones-and-flesh)
    mapped-targets (map-targets flesh-items)]
    (map (fn [bone]
           (process-bone bone mapped-targets))
         bones)))

(defn compile_
  ([source destination]
   ;;  Create destination directory if it doesn't exist
   (if (existsSync destination) () (mkdirSync destination))
   (let
    [sourceItems (js->clj (readdirSync source "utf8"))
     documents (filter document? sourceItems)
     document-paths (map (fn [document] (join source document)) documents)
     document-contents (map get-document-content document-paths)
     compiled-documents (map compile_ document-contents)]
    ;;  Compile documents
     (save-compiled-documents compiled-documents documents destination)))
  ([document-content]
   (let
    [parsed-content (js->clj (xml2js document-content) :keywordize-keys true)
     document-body (filter (fn [element]
                             (= (get element :name) "document_body")) (get parsed-content :elements))
     document-body-content (if (not= nil document-body) (get (first document-body) :elements) '())
     bones-and-flesh (collect-bones-and-flesh document-body-content)
     xml-js-object (clj->js {:elements (process-bones-and-flesh bones-and-flesh)})]
     (println (xml-format (js2xml xml-js-object)))
     (xml-format (js2xml xml-js-object)))))