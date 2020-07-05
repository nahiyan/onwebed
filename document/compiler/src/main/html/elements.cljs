(ns html.elements
  (:require [bone.descriptor.descriptor :as descriptor]
            [xml.element.attributes :as attributes]
            [xml.elements :as xml-elements]
            [clojure.string :as string]
            [bone.descriptor.element.targets :as descriptor-element-targets]
            [path :refer [join]]
            [xml-js :refer [xml2js]]
            [fs :refer [readFileSync]]))

(declare from-document-name)

;; Process descriptor elements into HTML elements
(defn from-bone-descriptor-elements
  [bone-descriptor-elements bone-descriptor-element-targets content source-directory]
  (if (empty? bone-descriptor-elements)
    ;; No items remaining, and we can show the contents of the box
    content
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
                                              bone-descriptor-element-targets content
                                              source-directory)

      targetted-text (get bone-descriptor-element-targets (keyword bone-descriptor-element-id))

      ;; Generate html elements out of all the targeted text items
      targeted-text-to-html-elements
      (if targetted-text [{:type "text" :text targetted-text}] [])

      new-children (if children
                     (if (seq targeted-text-to-html-elements)
                       (concat targeted-text-to-html-elements children)
                       (if (and has-closing-tag? (empty? children))
                         (list {:type "text" :text ""})
                         children))
                     targeted-text-to-html-elements)
      all-attributes (merge custom-attributes
                            (if (seq x-id) {:id x-id} nil)
                            (if (seq x-class) {:class x-class} nil))]
      (if (= "page" (get current-bone-descriptor-element :name))
        (get (js->clj (from-document-name (str x-id ".od")
                                          source-directory
                                          bone-descriptor-element-targets)
                      :keywordize-keys true)
             :elements)
        (list (merge {:type "element"
                      :name bone-descriptor-element-name
                      :elements new-children}
                     (if (seq all-attributes)
                       {:attributes all-attributes}
                       nil)))))))

; Take a bone (XML element), and process it to a form representing HTML elements
(defn from-bone
  [bone descriptor-element-targets source-directory]
  (let
   [descriptor (get bone :descriptor)
    children (get bone :children)
    descriptor-elements (descriptor/to-elements descriptor)
    content (reduce concat
                    (list)
                    (map (fn [bone]
                           (from-bone bone descriptor-element-targets source-directory))
                         children))]
    (from-bone-descriptor-elements descriptor-elements descriptor-element-targets content source-directory)))

(defn from-bones
  [bones descriptor-element-targets source-directory]
  (reduce concat
          (list)
          (map (fn [bone]
                 (from-bone bone descriptor-element-targets source-directory))
               bones)))

(declare to-fills)

(defn to-fills-of-element
  ([element]
   (to-fills-of-element element {}))
  ([element fills]
   (case (get element :type)
     "element"
     (let
      [name (get element :name)
       children (get element :elements)]
       (if (= name "fill")
         (let
          [id (get (get element :attributes) :id)
           new-fills (assoc fills (keyword id) children)]
           new-fills)
         (to-fills children fills)))
     element)))

(defn to-fills
  ([elements]
   (to-fills elements {}))
  ([elements fills]
   (reduce conj
           fills
           (map to-fills-of-element
                elements))))

(declare fill-holes)

(defn fill-holes-of-element
  [element fills]
  (case (get element :type)
    "element"
    (let
     [name (get element :name)
      children (get element :elements)]
      (case name
        "hole"
        (let
         [id (get (get element :attributes) :id)
          fill (get fills (keyword id))]
          (if fill
            fill
            children))
        "fill"
        nil
        (list (assoc element
                     :elements
                     (fill-holes children
                                 fills)))))
    "text"
    (list element)
    element))

(defn fill-holes
  ([elements]
   (fill-holes elements
               (to-fills elements)))
  ([elements fills]
   (reduce concat
           (list)
           (filter (fn [element]
                     (if element
                       true
                       false))
                   (map
                    (fn [element]
                      (fill-holes-of-element element
                                             fills))
                    elements)))))

(defn from-document-content
  ([content source-directory]
   (from-document-content content {} source-directory true))
  ([content bone-descriptor-element-targets source-directory fill-holes?]
   (let
    [xml-elements (js->clj (xml2js content) :keywordize-keys true)
     document-body (filter (fn [element]
                             (= (get element :name) "document_body"))
                           (get xml-elements :elements))
     document-body-content (if (not= nil document-body)
                             (get (first document-body) :elements)
                             '())
     bones-and-flesh (xml-elements/to-bones-and-flesh document-body-content)
     bones (filter (fn [item]
                     (= (get item :type) "bone"))
                   bones-and-flesh)
     flesh-items (filter (fn [item]
                           (= (get item :type) "flesh"))
                         bones-and-flesh)

     descriptor-element-targets
     (descriptor-element-targets/merge_ bone-descriptor-element-targets
                                        (descriptor-element-targets/from-flesh-items flesh-items))

     bones-to-html (from-bones bones
                               descriptor-element-targets source-directory)



     html-elements (if fill-holes?
                     (let
                      [filled-holes (fill-holes bones-to-html)]
                       (clj->js {:elements filled-holes}))
                     (clj->js {:elements bones-to-html}))]
     html-elements)))

(defn from-document-name
  [name source-directory descriptor-element-targets]
  (let
   [document-path (join source-directory name)
    document-content (readFileSync document-path "utf8")
    html-elements (from-document-content document-content descriptor-element-targets source-directory false)]
    html-elements))
