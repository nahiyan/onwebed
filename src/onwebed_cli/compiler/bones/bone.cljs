(ns onwebed-cli.compiler.bones.bone
  (:require
   [onwebed-cli.compiler.bones.descriptor :as descriptor]))

; Take a bone (XML element), and process it to a form representing HTML elements
(defn to-html-elements
  [bone mapped-targets]
  (let
   [descriptor (get bone :descriptor)
    children (get bone :children)
    descriptorElements (descriptor/parse descriptor)
    content (map (fn [bone]
                   (to-html-elements bone mapped-targets))
                 children)]
    (descriptor/process-elements descriptorElements mapped-targets content)))

(defn bones-to-html-elements
  [bones descriptor-element-targets]
  (map (fn [bone]
         (to-html-elements bone descriptor-element-targets))
       bones))