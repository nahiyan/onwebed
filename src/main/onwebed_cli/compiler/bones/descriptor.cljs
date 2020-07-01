(ns onwebed-cli.compiler.bones.descriptor)

(def blank-element {:element_name ""
                    :bone_name ""
                    :attributes ""
                    :classes ""
                    :id ""
                    :closing-tag true})

(declare to-elements)

(defn add-to-element-property
  [property current-character rest-of-characters current-element elements]
  (let
   [new-element-property (str (get current-element (keyword property)) current-character)
    new-element (assoc current-element (keyword property) new-element-property)
    new-elements (conj (pop elements) new-element)]
    (vector rest-of-characters property new-elements)))

(defn set-element-property
  [property value rest-of-characters new-mode current-element elements]
  (let
   [new-element (assoc current-element (keyword property) value)
    new-elements (conj (pop elements) new-element)]
    (vector rest-of-characters new-mode new-elements)))

(defn end-element
  [rest-of-characters elements]
  (let
   [new-elements (conj elements blank-element)]
    (vector rest-of-characters "element_name" new-elements)))

(defn get-character-type
  [character]
  (case character
    \[ :start_of_attributes
    \] :end_of_attributes
    \. :start_of_classes
    \# :start_of_id
    \@ :start_of_bone_name
    \space :whitespace
    \tab :whitespace
    \newline :whitespace
    nil :end
    :other))

(defn handle-start-of-element
  [character-type rest-of-characters elements]
  (case character-type
    :start_of_attributes
    (vector rest-of-characters "attributes" elements)
    :start_of_classes
    (let
     [character-type (get-character-type (first rest-of-characters))
      current-element (peek elements)]
      (if (or (= character-type :whitespace) (= character-type :end))
        (set-element-property "closing-tag" false rest-of-characters "end" current-element elements)
        (vector rest-of-characters "classes" elements)))
    :start_of_id
    (vector rest-of-characters "id" elements)
    :start_of_bone_name
    (vector rest-of-characters "bone_name" elements)
    :whitespace
    (end-element rest-of-characters elements)))

; Process descriptor to get descriptor elements
(defn to-elements
  ([descriptor]
   (to-elements descriptor "element_name" (vector blank-element)))
  ([characters mode elements]
   (if (empty? characters)
      ;  No character of descriptor left for processing, so return the result
     elements
      ;  Process current character based on processing mode
     (let
      [current-character (first characters)
       rest-of-characters (rest characters)
       current-element (peek elements)
       character-type (get-character-type current-character)
       new-arguments (case mode
                       "element_name"
                       (case character-type
                         :other
                         (add-to-element-property "element_name" current-character rest-of-characters current-element elements)
                         (handle-start-of-element character-type rest-of-characters elements))
                       "attributes"
                       (case character-type
                         :end_of_attributes
                         (vector rest-of-characters "element_name" elements)
                         (add-to-element-property "attributes" current-character rest-of-characters current-element elements))
                       "classes"
                       (case character-type
                         :other
                         (add-to-element-property "classes" current-character rest-of-characters current-element elements)
                         (let
                          [additionOfSpace (add-to-element-property "classes" \space rest-of-characters current-element elements)
                           new-elements (get additionOfSpace 2)]
                           (handle-start-of-element character-type rest-of-characters new-elements)))
                       "id"
                       (case character-type
                         :other
                         (add-to-element-property "id" current-character rest-of-characters current-element elements)
                         (handle-start-of-element character-type rest-of-characters elements))
                       "end"
                       (vector () nil elements)
                       ;;  bone_name
                       (case character-type
                         :other
                         (add-to-element-property "bone_name" current-character rest-of-characters current-element elements)
                         (handle-start-of-element character-type rest-of-characters elements)))
       new-characters (get new-arguments 0)
       new-mode (get new-arguments 1)
       new-elements (get new-arguments 2)]

       (to-elements new-characters new-mode new-elements)))))
