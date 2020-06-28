(ns onwebed-cli.compiler.base
  (:require [cljs.nodejs :as nodejs]
            [path :refer [extname join]]
            [xml-js :refer [xml2js js2xml]]
            [clojure.string :refer [split]]
            [onwebed-cli.compiler.bones.descriptor :refer [parseDescriptor processBoneDescriptorElements]]
            [fs :refer [readFileSync readdirSync mkdirSync existsSync writeFileSync]]))

(nodejs/enable-util-print!)

(defn document? [item]
  (= ".od" (extname item)))

(defn saveCompiledDocument
  [content document destination]
  (let
   [filePath (join destination (str document ".html"))]
    (writeFileSync filePath content)))

(defn saveCompiledDocuments
  [contents documents destination]
  (let [content (first contents)
        document (first documents)
        restOfContents (rest contents)
        restOfDocuments (rest documents)]
    (saveCompiledDocument content document destination)
    (if (= restOfContents ()) () (saveCompiledDocuments restOfContents restOfDocuments destination))))

(defn getDocumentContent
  [documentPath]
  (readFileSync documentPath "utf8"))

(defn combineTextElements
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
(defn collectBonesAndFlesh
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
                           (collectBonesAndFlesh elements)
                           nil)
                descriptor (get attributes :descriptor)]
                {:type "bone" :children children :descriptor descriptor})
              (if (= name "flesh")
                (let
                 [for_ (get attributes :for)
                  content (combineTextElements elements)]
                  {:type "flesh" :for for_ :content content})
                nil)))) elements)))

; Generate map of flesh targets and their respective contents
(defn mapTargets
  [fleshItems]
  (let
   [contentItems (vec
                  (map (fn
                         [item]
                         (get item :content))
                       fleshItems))
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
                                              existingKey (get acc key)]
                                              (if (not= existingKey nil)
                                                ;; If key already exists
                                                {key (conj existingKey (first value))}
                                                {key value})))
                                          item))))
                    {}
                    (map (fn
                           [fleshItem contentItemIndex]
                           (let
                            [targets (js->clj (split (get fleshItem :for) #"\s"))]
                             (reduce conj {} (map (fn
                                                    [target]
                                                    {target (vector contentItemIndex)})
                                                  targets))))
                         fleshItems
                         (range (count contentItems))))]
    {:contentItems contentItems
     :targets targets}))

; Process attributes to a form which can be fed into xml.js-supported objects
(defn processAttributes
  [attributes]
  (let
   [dummyElementXml (str "<dummy " attributes "/>")
    dummyElementAttributes (get (first (get (js->clj (xml2js dummyElementXml) :keywordize-keys true) :elements)) :attributes)]
    dummyElementAttributes))

; Take a bone (XML element), and process it to a form representing HTML elements
(defn processBone
  [bone mappedTargets]
  (let
   [descriptor (get bone :descriptor)
    children (get bone :children)
    descriptorElements (parseDescriptor descriptor)
    content (map (fn [bone]
                   (processBone bone mappedTargets))
                 children)
    processedDescriptorElements (processBoneDescriptorElements descriptorElements mappedTargets content)]
    processedDescriptorElements))

(defn processBonesAndFlesh
  [bonesAndFlesh]
  (let
   [fleshItems (filter (fn
                         [item]
                         (= (get item :type) "flesh"))
                       bonesAndFlesh)
    bones (filter (fn
                    [item]
                    (= (get item :type) "bone"))
                  bonesAndFlesh)
    mappedTargets (mapTargets fleshItems)]
    (map (fn [bone]
           (processBone bone mappedTargets))
         bones)))

(defn compile_
  ([source destination]
   ;;  Create destination directory if it doesn't exist
   (if (existsSync destination) () (mkdirSync destination))
   (let
    [sourceItems (js->clj (readdirSync source "utf8"))
     documents (filter document? sourceItems)
     documentPaths (map (fn [document] (join source document)) documents)
     documentContents (map getDocumentContent documentPaths)
     compiledDocuments (map compile_ documentContents)]
    ;;  Compile documents
     (saveCompiledDocuments compiledDocuments documents destination)))
  ([documentContent]
   (let
    [parsedContent (js->clj (xml2js documentContent) :keywordize-keys true)
     documentBody (filter (fn [element]
                            (= (get element :name) "document_body")) (get parsedContent :elements))
     documentBodyContent (if (not= nil documentBody) (get (first documentBody) :elements) '())
     bonesAndFlesh (collectBonesAndFlesh documentBodyContent)
     xmlJsObject (clj->js {:elements (processBonesAndFlesh bonesAndFlesh)})]
     (println (js2xml xmlJsObject))
     (js2xml xmlJsObject))))