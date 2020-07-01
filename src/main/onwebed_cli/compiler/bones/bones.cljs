(ns onwebed-cli.compiler.bones.bones
  (:require
   [onwebed-cli.compiler.bones.descriptor :as descriptor]))

; Take a bone (XML element), and process it to a form representing HTML elements
(defn bone-to-html-elements
  [bone descriptor-element-targets]
  (let
   [descriptor (get bone :descriptor)
    children (get bone :children)
    descriptor-elements (descriptor/to-elements descriptor)
    content (map (fn [bone]
                   (bone-to-html-elements bone descriptor-element-targets))
                 children)]
    (descriptor/elements-to-html-elements descriptor-elements descriptor-element-targets content)))

(defn to-html-elements
  [bones descriptor-element-targets]
  (map (fn [bone]
         (bone-to-html-elements bone descriptor-element-targets))
       bones))