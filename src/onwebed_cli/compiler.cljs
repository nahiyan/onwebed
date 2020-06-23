(ns onwebed-cli.compiler
  (:require [cljs.nodejs :as nodejs]))

(nodejs/enable-util-print!)

(defn compile_ [source destination]
  (println source destination))