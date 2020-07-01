(ns onwebed-cli.compiler.html.elements
  (:require [onwebed-cli.compiler.bones.descriptor :as descriptor]
            [onwebed-cli.compiler.bones.attributes :as attributes]
            [onwebed-cli.compiler.xml :as xml]
            [clojure.string :refer [trim]]
            [onwebed-cli.compiler.flesh_items :as flesh-items]
            [path :refer [join]]
            [xml-js :refer [xml2js]]
            [fs :refer [readFileSync]]))

(declare from-document)

;; Process descriptor elements into HTML elements
(defn from-bone-descriptor-elements
  [elements targets content]
  (if (seq elements)
    (let
     [current-element (first elements)
      rest-of-elements (rest elements)
      element-name (get current-element :element_name)
      elements (from-bone-descriptor-elements rest-of-elements targets content)
      elements-listified (if (map? elements) (list elements) elements)
      bone-name (get current-element :bone_name)
      has-closing-tag? (get current-element :closing-tag)
      element-target-indices (get (get targets :targets) bone-name)
      element-targets (if (seq element-target-indices)
                        (let
                         [content-items (get targets :content-items)
                          target-content (reduce str
                                                 ""
                                                 (map (fn [targetIndex]
                                                        (trim (nth content-items targetIndex)))
                                                      element-target-indices))]
                          [{:type "text" :text target-content}])
                        [])
      new-elements (if (not= nil elements-listified)
                     (if (seq element-targets)
                       (concat element-targets elements-listified)
                       (if (and has-closing-tag? (empty? elements-listified))
                         (list {})
                         elements-listified))
                     element-targets)
      classes (trim (get current-element :classes))
      id (get current-element :id)
      custom-attributes (attributes/to-map (get current-element :attributes))
      all-attributes (merge custom-attributes
                            (if (seq id) {:id id} nil)
                            (if (seq classes) {:class classes} nil))
      attributes-map (if (seq all-attributes) {:attributes all-attributes} nil)]
    ;;   (when (= "page" (get current-element :element_name))
    ;;     (println (from-document (str id ".od") "site" nil)))
      (merge {:type "element" :name element-name :elements new-elements} attributes-map))
    ;; No items remaining, and we can show the contents of the box
    content))

; Take a bone (XML element), and process it to a form representing HTML elements
(defn from-bone
  [bone descriptor-element-targets]
  (let
   [descriptor (get bone :descriptor)
    children (get bone :children)
    descriptor-elements (descriptor/to-elements descriptor)
    content (map (fn [bone]
                   (from-bone bone descriptor-element-targets))
                 children)]
    (from-bone-descriptor-elements descriptor-elements descriptor-element-targets content)))

(defn from-bones
  [bones descriptor-element-targets]
  (map (fn [bone]
         (from-bone bone descriptor-element-targets))
       bones))

(defn from-document-content
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
    html-elements (clj->js {:elements (from-bones bones descriptor-element-targets)})]
    html-elements))

;; Process document to HTML elements
(defn from-document
  [name source _descriptor-element-targets]
  (let
   [document-path (join source name)
    document-content (readFileSync document-path "utf8")
    html-elements (from-document-content document-content)]
    html-elements))
