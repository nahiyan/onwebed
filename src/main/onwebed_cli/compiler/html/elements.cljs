(ns onwebed-cli.compiler.html.elements
  (:require [onwebed-cli.compiler.bone.descriptor.descriptor :as descriptor]
            [onwebed-cli.compiler.xml.element.attributes :as attributes]
            [onwebed-cli.compiler.xml.elements :as xml-elements]
            [clojure.string :as string]
            [onwebed-cli.compiler.bone.descriptor.element.targets :as descriptor-element-targets]
            ;; [onwebed-cli.compiler.flesh_items :as flesh-items]
            [path :refer [join]]
            [xml-js :refer [xml2js]]
            [fs :refer [readFileSync]]))

(declare from-document)

;; Process descriptor elements into HTML elements
(defn from-bone-descriptor-elements
  [bone-descriptor-elements bone-descriptor-element-targets content]
  (if (seq bone-descriptor-elements)
    (let
     [current-bone-descriptor-element (first bone-descriptor-elements)
      rest-of-bone-descriptor-elements (rest bone-descriptor-elements)

      ;; Properties of current descriptor element
      bone-descriptor-element-name (get current-bone-descriptor-element :name)
      has-closing-tag? (get current-bone-descriptor-element :has-closing-tag)
      bone-descriptor-element-id (get current-bone-descriptor-element :id)
      x-class (string/trim (get current-bone-descriptor-element
                                :x-class))
      x-id (get current-bone-descriptor-element :x-id)
      custom-attributes (attributes/to-map (get current-bone-descriptor-element
                                                :attributes))

      children (from-bone-descriptor-elements rest-of-bone-descriptor-elements
                                              bone-descriptor-element-targets content)

      current-bone-descriptor-element-target-indices
      (get (get bone-descriptor-element-targets
                :association)
           bone-descriptor-element-id)

      ;; Generate html elements out of all the targeted text items
      targeted-text-items-to-html-elements
      (if (seq current-bone-descriptor-element-target-indices)
        (let
         [text-items (get bone-descriptor-element-targets :text-items)
          combined-text-items (reduce str
                                      ""
                                      (map (fn [index]
                                             (string/trim (nth text-items index)))
                                           current-bone-descriptor-element-target-indices))]
          [{:type "text" :text combined-text-items}])
        [])

      new-children (if (not= nil children)
                     (if (seq targeted-text-items-to-html-elements)
                       (concat targeted-text-items-to-html-elements children)
                       (if (and has-closing-tag? (empty? children))
                         (list {})
                         children))
                     targeted-text-items-to-html-elements)
      all-attributes (merge custom-attributes
                            (if (seq x-id) {:id x-id} nil)
                            (if (seq x-class) {:class x-class} nil))]
      (if (= "page" (get current-bone-descriptor-element :name))
        (get (js->clj (from-document (str x-id ".od")
                                     "site"
                                     nil)
                      :keywordize-keys true)
             :elements)
        (list (merge {:type "element"
                      :name bone-descriptor-element-name
                      :elements new-children}
                     (if (seq all-attributes)
                       {:attributes all-attributes}
                       nil)))))
    ;; No items remaining, and we can show the contents of the box
    content))

; Take a bone (XML element), and process it to a form representing HTML elements
(defn from-bone
  [bone descriptor-element-targets]
  (let
   [descriptor (get bone :descriptor)
    children (get bone :children)
    descriptor-elements (descriptor/to-elements descriptor)
    content (reduce concat
                    (list)
                    (map (fn [bone]
                           (from-bone bone descriptor-element-targets))
                         children))]
    (from-bone-descriptor-elements descriptor-elements descriptor-element-targets content)))

(defn from-bones
  [bones descriptor-element-targets]
  (reduce concat
          (list)
          (map (fn [bone]
                 (from-bone bone descriptor-element-targets))
               bones)))

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
    bones-and-flesh (xml-elements/to-bones-and-flesh document-body-content)
    bones (filter (fn
                    [item]
                    (= (get item :type) "bone"))
                  bones-and-flesh)
    flesh-items (filter (fn
                          [item]
                          (= (get item :type) "flesh"))
                        bones-and-flesh)
    descriptor-element-targets (descriptor-element-targets/from-flesh-items flesh-items)
    html-elements (clj->js {:elements (from-bones bones descriptor-element-targets)})]
    html-elements))

;; Process document to HTML elements
(defn from-document
  [name source descriptor-element-targets]
  (let
   [document-path (join source name)
    document-content (readFileSync document-path "utf8")
    html-elements (from-document-content document-content)]
    html-elements))
