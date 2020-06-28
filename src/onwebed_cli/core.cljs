(ns onwebed-cli.core
  (:require [onwebed-cli.compiler.base :refer [compile_]]
            [cljs.nodejs :as nodejs]
            [clojure.tools.cli :refer [parse-opts]]))

(nodejs/enable-util-print!)

(def cli-options
  [["-v" "--version", "Get version information."]
   ["-h" "--help", "Show this help text."]
   ["-d" "--destination <path>" "Destination directory." :default "build"]])

(defn show-version [] (println "Onwebed 0.1.0."))
(defn show-help [summary] (println summary))
(defn show-error [] (println "Error: Not enough arguments. Run with --help flag to see all arguments."))

(defn -main [& args]
  (let
   [processed-arguments (parse-opts args cli-options)
    options (get processed-arguments :options)
    destination (get options :destination)
    version (get options :version)
    help (get options :help)
    summary (get processed-arguments :summary)
    source (first (get processed-arguments :arguments))]
    (if (not= source nil)
      (compile_ source destination)
      (if (not= help nil)
        (show-help summary)
        (if (not= version nil)
          (show-version)
          (show-error))))))

(set! *main-cli-fn* -main)