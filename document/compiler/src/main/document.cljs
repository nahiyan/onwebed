(ns document
  (:require
   [path :refer [join]]
   ["../../../../lib/document/base" :refer [isPublic
                                            getContent
                                            saveCompiledDocuments
                                            sourceItems]]
   [html.html :as html]
   [fs :refer [mkdirSync
               existsSync]]))

(defn compile_
  ([source-directory destination]
   ;;  Create destination directory if it doesn't exist
   (when (not (existsSync destination)) (mkdirSync destination))
   (let
    [source-items (sourceItems source-directory)
     document-names (filter isPublic
                            source-items)
     document-paths (map (fn [document]
                           (join source-directory document))
                         document-names)
     document-contents (map getContent document-paths)
     compiled-documents (map (fn [document-content]
                               (html/from-document-content document-content
                                                           source-directory))
                             document-contents)]
     (saveCompiledDocuments compiled-documents document-names destination))))