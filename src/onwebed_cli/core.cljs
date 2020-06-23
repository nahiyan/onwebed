(ns onwebed-cli.core
  (:require [onwebed-cli.compiler :refer [compile_]] [cljs.nodejs :as nodejs] [clojure.tools.cli :refer [parse-opts]]))

(nodejs/enable-util-print!)

(def cli-options
  [["-v" "--version", "Get version information."]
   ["-h" "--help", "Show this help text."]
   ["-d" "--destination <path>" "Destination directory." :default "build"]])

(defn showVersion [] (println "Onwebed 0.1.0."))
(defn showHelp [summary] (println summary))
(defn showError [] (println "Error: Not enough arguments. Run with --help flag to see all arguments."))

(defn -main [& args]
  (let
   [processedArguments (parse-opts args cli-options)
    options (get processedArguments :options)
    destination (get options :destination)
    version (get options :version)
    help (get options :help)
    summary (get processedArguments :summary)
    source (first (get processedArguments :arguments))]
    (if (not= source nil)
      (compile_ source destination)
      (if (not= help nil)
        (showHelp summary)
        (if (not= version nil)
          (showVersion)
          (showError))))))

(set! *main-cli-fn* -main)