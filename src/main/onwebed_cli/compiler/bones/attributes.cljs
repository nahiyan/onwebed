(ns onwebed-cli.compiler.bones.attributes
  (:require [xml-js :refer [xml2js]]))

; Convert attributes defined in a string to a map (key-value pairs)
(defn to-map
  [attributes]
  (let
   [dummyElementXml (str "<dummy " attributes "/>")
    dummyElementAttributes (get (first (get (js->clj (xml2js dummyElementXml) :keywordize-keys true) :elements)) :attributes)]
    dummyElementAttributes))