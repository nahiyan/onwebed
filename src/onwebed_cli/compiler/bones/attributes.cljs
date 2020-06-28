(ns onwebed-cli.compiler.bones.attributes
  (:require [xml-js :refer [xml2js]]))

; Process attributes to a form which can be fed into xml.js-supported objects
(defn process
  [attributes]
  (let
   [dummyElementXml (str "<dummy " attributes "/>")
    dummyElementAttributes (get (first (get (js->clj (xml2js dummyElementXml) :keywordize-keys true) :elements)) :attributes)]
    dummyElementAttributes))