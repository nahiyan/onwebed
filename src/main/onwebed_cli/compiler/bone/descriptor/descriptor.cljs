(ns onwebed-cli.compiler.bone.descriptor.descriptor)

(def blank-element {:name ""
                    :id ""
                    :attributes ""
                    :x-class ""
                    :x-id ""
                    :has-closing-tag true})

(declare to-elements)

(defn append-element-property
  [property current-character rest-of-characters current-element elements]
  (let
   [new-element-property (str (get current-element property) current-character)
    new-element (assoc current-element property new-element-property)
    new-elements (conj (pop elements) new-element)]
    (vector rest-of-characters property new-elements)))

(defn set-element-property
  [property value rest-of-characters new-mode current-element elements]
  (let
   [new-element (assoc current-element property value)
    new-elements (conj (pop elements) new-element)]
    (vector rest-of-characters new-mode new-elements)))

(defn end-element
  [rest-of-characters elements]
  (let
   [new-elements (conj elements blank-element)]
    (vector rest-of-characters :name new-elements)))

(defn get-character-type
  [character]
  (case character
    \[ :start-of-attributes
    \] :end-of-attributes
    \. :start-of-x-class
    \# :start-of-x-id
    \@ :start-of-id
    \space :whitespace
    \tab :whitespace
    \newline :whitespace
    nil :end
    :other))

(defn handle-start-of-element
  [character-type rest-of-characters elements]
  (case character-type
    :start-of-attributes
    (vector rest-of-characters :attributes elements)
    :start-of-x-class
    (let
     [character-type (get-character-type (first rest-of-characters))
      current-element (peek elements)]
      (if (or (= character-type :whitespace) (= character-type :end))
        (set-element-property :has-closing-tag false rest-of-characters :end current-element elements)
        (vector rest-of-characters :x-class elements)))
    :start-of-x-id
    (vector rest-of-characters :x-id elements)
    :start-of-id
    (vector rest-of-characters :id elements)
    :whitespace
    (end-element rest-of-characters elements)))

; Process descriptor to get descriptor elements
(defn to-elements
  ([descriptor]
   (to-elements descriptor :name (vector blank-element)))
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
                       :name
                       (case character-type
                         :other
                         (append-element-property :name current-character rest-of-characters current-element elements)
                         (handle-start-of-element character-type rest-of-characters elements))
                       :attributes
                       (case character-type
                         :end-of-attributes
                         (vector rest-of-characters :name elements)
                         (append-element-property :attributes current-character rest-of-characters current-element elements))
                       :x-class
                       (case character-type
                         :other
                         (append-element-property :x-class current-character rest-of-characters current-element elements)
                         (let
                          [additionOfSpace (append-element-property :x-class \space rest-of-characters current-element elements)
                           new-elements (get additionOfSpace 2)]
                           (handle-start-of-element character-type rest-of-characters new-elements)))
                       :x-id
                       (case character-type
                         :other
                         (append-element-property :x-id current-character rest-of-characters current-element elements)
                         (handle-start-of-element character-type rest-of-characters elements))
                       :end
                       (vector () nil elements)
                       ;;  id
                       (case character-type
                         :other
                         (append-element-property :id current-character rest-of-characters current-element elements)
                         (handle-start-of-element character-type rest-of-characters elements)))
       new-characters (get new-arguments 0)
       new-mode (get new-arguments 1)
       new-elements (get new-arguments 2)]

       (to-elements new-characters new-mode new-elements)))))
