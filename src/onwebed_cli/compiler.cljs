(ns onwebed-cli.compiler
  (:require [cljs.nodejs :as nodejs]
            [path :refer [extname join]]
            [xml-js :refer [xml2js js2xml]]
            [clojure.string :refer [trim split]]
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

(def blankDescriptorElement {:element_name ""
                             :bone_name ""
                             :attributes ""
                             :classes ""
                             :id ""})

(defn startOfAttributes?
  [character]
  (= character \[))

(defn startOfClasses?
  [character]
  (= character \.))

(defn startOfId?
  [character]
  (= character \#))

(defn startOfBoneName?
  [character]
  (= character \@))

(defn endOfDescriptorElement?
  [character]
  (or (= character \space) (= character \tab) (= character \newline)))

(defn parseDescriptor
  ([descriptor]
   (parseDescriptor descriptor "element_name" (vector blankDescriptorElement)))
  ([descriptor mode elements]
   (let
    [firstCharacter (first descriptor)
     restOfCharacters (rest descriptor)
     currentElement (peek elements)]
     (if (not= firstCharacter nil)
       (if (= mode "element_name")
         (if (startOfAttributes? firstCharacter)
           (parseDescriptor restOfCharacters "attributes" elements)
           (if (startOfClasses? firstCharacter)
             (parseDescriptor restOfCharacters "classes" elements)
             (if (= firstCharacter \#)
               (parseDescriptor restOfCharacters "id" elements)
               (if (= firstCharacter \@)
                 (parseDescriptor restOfCharacters "bone_name" elements)
                 (if (endOfDescriptorElement? firstCharacter)
                ;;  End current element
                   (let
                    [newElements (conj elements blankDescriptorElement)]
                     (parseDescriptor restOfCharacters "element_name" newElements))
                ;;  Construct element name
                   (let
                    [newElementName (str (get currentElement :element_name) firstCharacter)
                     newElement (assoc currentElement :element_name newElementName)
                     newElements (conj (pop elements) newElement)]
                     (parseDescriptor restOfCharacters "element_name" newElements)))))))
         (if (= mode "attributes")
           (if (= firstCharacter \])
            ;;  End attributes mode
             (parseDescriptor restOfCharacters "element_name" elements)
             (if (endOfDescriptorElement? firstCharacter)
                ;;  End current element
               (let
                [newElements (conj elements blankDescriptorElement)]
                 (parseDescriptor restOfCharacters "element_name" newElements))
               ;;  Construct attributes
               (let
                [newAttributes (str (get currentElement :attributes) firstCharacter)
                 newElement (assoc currentElement :attributes newAttributes)
                 newElements (conj (pop elements) newElement)]
                 (parseDescriptor restOfCharacters "attributes" newElements))))
           (if (= mode "classes")
             (if (= firstCharacter \[)
               (parseDescriptor restOfCharacters "attributes" elements)
               (if (= firstCharacter \#)
                 (parseDescriptor restOfCharacters "id" elements)
                 (if (= firstCharacter \@)
                   (parseDescriptor restOfCharacters "bone_name" elements)
                   (if (or (endOfDescriptorElement? firstCharacter))
                    ;;  End current element
                     (let
                      [newElements (conj elements blankDescriptorElement)]
                       (parseDescriptor restOfCharacters "element_name" newElements))
                    ;;  Construct classes
                     (let
                      [newClasses (str (get currentElement :classes)
                                       (if (= firstCharacter \.)
                                         \space
                                         firstCharacter))
                       newElement (assoc currentElement :classes newClasses)
                       newElements (conj (pop elements) newElement)]
                       (parseDescriptor restOfCharacters "classes" newElements))))))
             (if (= mode "id")
               (if (= firstCharacter \[)
                 (parseDescriptor restOfCharacters "attributes" elements)
                 (if (= firstCharacter \.)
                   (parseDescriptor restOfCharacters "classes" elements)
                   (if (= firstCharacter \@)
                     (parseDescriptor restOfCharacters "bone_name" elements)
                     (if (endOfDescriptorElement? firstCharacter)
                      ;;  End current element
                       (let
                        [newElements (conj elements blankDescriptorElement)]
                         (parseDescriptor restOfCharacters "element_name" newElements))
                      ;;  Construct ID
                       (let
                        [newId (str (get currentElement :id) firstCharacter)
                         newElement (assoc currentElement :id newId)
                         newElements (conj (pop elements) newElement)]
                         (parseDescriptor restOfCharacters "id" newElements))))))
            ;;  Process bone name
               (if (= firstCharacter \[)
                 (parseDescriptor restOfCharacters "attributes" elements)
                 (if (= firstCharacter \.)
                   (parseDescriptor restOfCharacters "classes" elements)
                   (if (= firstCharacter \#)
                     (parseDescriptor restOfCharacters "id" elements)
                     (if (endOfDescriptorElement? firstCharacter)
                      ;;  End current element
                       (let
                        [newElements (conj elements blankDescriptorElement)]
                         (parseDescriptor restOfCharacters "element_name" newElements))
                      ;;  Construct bone name
                       (let
                        [newBoneName (str (get currentElement :bone_name) firstCharacter)
                         newElement (assoc currentElement :bone_name newBoneName)
                         newElements (conj (pop elements) newElement)]
                         (parseDescriptor restOfCharacters "bone_name" newElements))))))))))
    ;;  End of processing
       elements))))

(defn processAttributes
  [attributes]
  (let
   [dummyElementXml (str "<dummy " attributes "/>")
    dummyElementAttributes (get (first (get (js->clj (xml2js dummyElementXml) :keywordize-keys true) :elements)) :attributes)]
    dummyElementAttributes))

;; Process bone descriptor elements into XML elements
(defn processBoneDescriptorElements
  [descriptorElements mappedTargets content]
  (let
   [currentDescriptorElement (first descriptorElements)
    restOfDescriptorElements (rest descriptorElements)
    elementName (get currentDescriptorElement :element_name)]
    (if (not= currentDescriptorElement nil)
      ;; Not end of descriptor elements
      (let
       [elements (processBoneDescriptorElements restOfDescriptorElements mappedTargets content)
        elementsListified (if (map? elements) (list elements) elements)
        boneName (get currentDescriptorElement :bone_name)
        targetIndices (get (get mappedTargets :targets) boneName)
        targetedContent (if (seq targetIndices)
                          (let
                           [contentItems (get mappedTargets :contentItems)
                            targetContent (reduce str
                                                  ""
                                                  (map (fn [targetIndex]
                                                         (trim (nth contentItems targetIndex)))
                                                       targetIndices))]
                            [{:type "text" :text targetContent}])
                          [])
        newElements (if (not= nil elementsListified)
                      (if (seq targetedContent)
                        (concat targetedContent elementsListified)
                        elementsListified)
                      targetedContent)
        classes (get currentDescriptorElement :classes)
        id (get currentDescriptorElement :id)
        customAttributes (processAttributes (get currentDescriptorElement :attributes))
        allAttributes (merge customAttributes
                             {:id (if (> (count id) 0) id nil)}
                             {:class (if (> (count classes) 0) classes nil)})]
        {:type "element" :name elementName :elements newElements :attributes allAttributes})
      (if (empty? restOfDescriptorElements)
        ;; No items remaining, and we can show the contents of the box
        content
        nil))))

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

(defn processDocumentRoot
  [elements]
  (map (fn [element]
         (let
          [elements (get element :elements)
           bonesAndFlesh (collectBonesAndFlesh elements)]
           (processBonesAndFlesh bonesAndFlesh)))
       elements))

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
     (js2xml xmlJsObject))))