(ns onwebed-cli.compiler.bones.descriptor
  (:require [onwebed-cli.compiler.base :refer [processAttributes]]
            [clojure.string :refer [trim]]))

(def blankDescriptorElement {:element_name ""
                             :bone_name ""
                             :attributes ""
                             :classes ""
                             :id ""})

(declare parseDescriptor)

(defn constructDescriptorElementProperty
  [elementType firstCharacter restOfCharacters currentElement elements]
  (let
   [newElementProperty (str (get currentElement (keyword elementType)) firstCharacter)
    newElement (assoc currentElement (keyword elementType) newElementProperty)
    newElements (conj (pop elements) newElement)]
    (vector restOfCharacters elementType newElements)))

(defn endCurrentDescriptorElement
  [restOfCharacters elements]
  (let
   [newElements (conj elements blankDescriptorElement)]
    (vector restOfCharacters "element_name" newElements)))

(defn handleStartOfDescriptorElements
  [characterType restOfCharacters elements]
  (case characterType
    :start_of_attributes
    (vector restOfCharacters "attributes" elements)
    :start_of_classes
    (vector restOfCharacters "classes" elements)
    :start_of_id
    (vector restOfCharacters "id" elements)
    :start_of_bone_name
    (vector restOfCharacters "bone_name" elements)
    :whitespace
    (endCurrentDescriptorElement restOfCharacters elements)))

(defn parseDescriptorElement
  [characters mode elements]
  (let
   [firstCharacter (first characters)
    restOfCharacters (rest characters)
    currentElement (peek elements)
    characterType (case firstCharacter
                    \[ :start_of_attributes
                    \] :end_of_attributes
                    \. :start_of_classes
                    \# :start_of_id
                    \@ :start_of_bone_name
                    \space :whitespace
                    \tab :whitespace
                    \newline :whitespace
                    :other)
    newArguments (case mode
                   "element_name"
                   (case characterType
                     :other
                     (constructDescriptorElementProperty "element_name" firstCharacter restOfCharacters currentElement elements)
                     (handleStartOfDescriptorElements characterType restOfCharacters elements))
                   "attributes"
                   (case characterType
                     :end_of_attributes
                     (vector restOfCharacters "element_name" elements)
                     (constructDescriptorElementProperty "attributes" firstCharacter restOfCharacters currentElement elements))
                   "classes"
                   (case characterType
                     :other
                     (constructDescriptorElementProperty "classes" firstCharacter restOfCharacters currentElement elements)
                     (let
                      [additionOfSpace (constructDescriptorElementProperty "classes" \space restOfCharacters currentElement elements)
                       newElements (get additionOfSpace 2)]
                       (handleStartOfDescriptorElements characterType restOfCharacters newElements)))
                   "id"
                   (case characterType
                     :other
                     (constructDescriptorElementProperty "id" firstCharacter restOfCharacters currentElement elements)
                     (handleStartOfDescriptorElements characterType restOfCharacters elements))
                  ;;  bone_name
                   (case characterType
                     :other
                     (constructDescriptorElementProperty "bone_name" firstCharacter restOfCharacters currentElement elements)
                     (handleStartOfDescriptorElements characterType restOfCharacters elements)))
    newCharacters (get newArguments 0)
    newMode (get newArguments 1)
    newElements (get newArguments 2)]

    (parseDescriptor newCharacters newMode newElements)))

; Process descriptor to get descriptor elements
(defn parseDescriptor
  ([descriptor]
   (parseDescriptor descriptor "element_name" (vector blankDescriptorElement)))
  ([descriptor mode elements]
   (if (empty? descriptor)
      ;  No character of descriptor left for processing, just return the result
     elements
      ;  Process current character based on processing mode
     (parseDescriptorElement descriptor mode elements))))

;; Process bone descriptor elements into a form representing HTML elements
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
        classes (trim (get currentDescriptorElement :classes))
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