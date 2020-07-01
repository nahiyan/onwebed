(ns onwebed-cli.compiler.html.html
  (:require [onwebed-cli.compiler.html.elements :as elements]
            [xml-js :refer [js2xml]]
            ["./html" :refer (format)]))

;; Take document content and convert it to HTML
(defn from-document-content
  [content]
  (let
   [html-elements (elements/from-document-content content)]
    ;;  (println (xml-format (js2xml xml-js-object)))
    (format (js2xml html-elements))))