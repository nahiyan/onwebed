(ns xml.elements)

;; Combine XML text elements into a string
(defn to-string
  [elements]
  (let
   [text-elements (filter (fn [element]
                            (let
                             [type (get element :type)]
                              (= type "text")))
                          elements)
    texts (map (fn [element] (get element :text)) text-elements)]
    (apply str texts)))

; Take XML elements and retrieve bones and flesh in processed form
(defn to-bones-and-flesh
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
                           (to-bones-and-flesh elements)
                           nil)
                descriptor (get attributes :descriptor)]
                {:type "bone" :children children :descriptor descriptor})
              (if (= name "flesh")
                (let
                 [for_ (get attributes :for)
                  content (to-string elements)]
                  {:type "flesh" :for for_ :content content})
                nil)))) elements)))