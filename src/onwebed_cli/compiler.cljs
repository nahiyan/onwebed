(ns onwebed-cli.compiler
  (:require [cljs.nodejs :as nodejs]
            [path :refer [extname join]]
            [xml-js :refer [xml2js]]
            [helpers :as h]
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
                       fleshItems))]
    (merge (map (fn
                  [fleshItem contentItemIndex]
                  (let
                   [targets (h/split (get fleshItem :for))]
                    (merge (map (fn
                                  [target]
                                  {target contentItemIndex})
                                targets))))
                fleshItems
                (range (count contentItems))))))

(def blankDescriptorElement {:element_name ""
                             :bone_name ""
                             :attributes ""
                             :classes ""
                             :id ""})

(defn parseDescriptor
  ([descriptor]
   (parseDescriptor descriptor "element_name" (list blankDescriptorElement)))
  ([descriptor mode elements]
   (let
    [firstCharacter (first descriptor)
     restOfCharacters (rest descriptor)
     currentElement (peek elements)]
     (if (not= firstCharacter nil)
       (if (= mode "element_name")
         (if (= firstCharacter \[)
           (parseDescriptor restOfCharacters "attributes" elements)
           (if (= firstCharacter \.)
             (parseDescriptor restOfCharacters "classes" elements)
             (if (= firstCharacter \#)
               (parseDescriptor restOfCharacters "id" elements)
               (if (= firstCharacter \@)
                 (parseDescriptor restOfCharacters "bone_name" elements)
                 (if (or (= firstCharacter \space) (= firstCharacter \tab) (= firstCharacter \newline))
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
          ;;  Construct attributes
             (let
              [newAttributes (str (get currentElement :attributes) firstCharacter)
               newElement (assoc currentElement :attributes newAttributes)
               newElements (conj (pop elements) newElement)]
               (parseDescriptor restOfCharacters "attributes" newElements)))
           (if (= mode "classes")
             (if (= firstCharacter \[)
               (parseDescriptor restOfCharacters "attributes" elements)
               (if (= firstCharacter \#)
                 (parseDescriptor restOfCharacters "id" elements)
                 (if (= firstCharacter \@)
                   (parseDescriptor restOfCharacters "bone_name" elements)
                   (let
                    [newClasses (str (get currentElement :classes) firstCharacter)
                     newElement (assoc currentElement :classes newClasses)
                     newElements (conj (pop elements) newElement)]
                     (parseDescriptor restOfCharacters "classes" newElements)))))
             (if (= mode "id")
               (if (= firstCharacter \[)
                 (parseDescriptor restOfCharacters "attributes" elements)
                 (if (= firstCharacter \.)
                   (parseDescriptor restOfCharacters "classes" elements)
                   (if (= firstCharacter \@)
                     (parseDescriptor restOfCharacters "bone_name" elements)
                     (let
                      [newId (str (get currentElement :id) firstCharacter)
                       newElement (assoc currentElement :id newId)
                       newElements (conj (pop elements) newElement)]
                       (parseDescriptor restOfCharacters "id" newElements)))))
            ;;  Process bone name
               (if (= firstCharacter \[)
                 (parseDescriptor restOfCharacters "attributes" elements)
                 (if (= firstCharacter \.)
                   (parseDescriptor restOfCharacters "classes" elements)
                   (if (= firstCharacter \#)
                     (parseDescriptor restOfCharacters "id" elements)
                     (let
                      [newBoneName (str (get currentElement :bone_name) firstCharacter)
                       newElement (assoc currentElement :bone_name newBoneName)
                       newElements (conj (pop elements) newElement)]
                       (parseDescriptor restOfCharacters "bone_name" newElements)))))))))
    ;;  End of processing
       elements))))

(defn compileBone
  [descriptorElements content mappedTargets]
  (let
   [currentDescriptorElement (first descriptorElements)
    restOfDescriptorElements (rest descriptorElements)
    elementName (get currentDescriptorElement :element_name)
    elements (compileBone restOfDescriptorElements content mappedTargets)]
    (if (not= currentDescriptorElement nil)
      {:type "element" :name elementName :elements elements}
      content)))

(defn compileBones
  [bones mappedTargets]
  ;; (map (fn
  ;;        [bone]
  ;;        (let
  ;;         [descriptor (get bone :descriptor)
  ;;          children (get bone :children)
  ;;          parsedDescriptor (parseDescriptor descriptor)
  ;;          content (compileBones children mappedTargets)]
  ;;          (compileBone parsedDescriptor content mappedTargets)))
  ;;      bones)
  ())

(defn processBonesAndFlesh
  [bonesAndFlesh]
  (let
   [fleshItems (filter (fn [item] (= (get item :type) "flesh")) bonesAndFlesh)
    bones (filter (fn
                    [item]
                    (= (get item :type) "bone")) bonesAndFlesh)
    mappedTargets (mapTargets fleshItems)]
    (compileBones bones mappedTargets)))

(defn processElements
  [elements]
  (apply str (map (fn
                    [element]
                    (let
                     [name (get element :name)]
                      (if (= name "document_body")
                        (let
                         [elements (get element :elements)
                          bonesAndFlesh (collectBonesAndFlesh elements)]
                          (processBonesAndFlesh bonesAndFlesh))
                        "")))
                  elements)))

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
     rootElements (get parsedContent :elements)]
     (processElements rootElements))))