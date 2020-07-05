(ns html.html
  (:require [html.elements :as html-elements]
            [xml-js :refer [js2xml]]
            ["./html" :refer (format)]))

;; Take document content and convert it to HTML
(defn from-document-content
  [content source-directory]
  (let
   [html-elements (html-elements/from-document-content content source-directory)]
    (format (js2xml html-elements))))