(ns onwebed-cli.core
  (:require
   [onwebed-cli.compiler.document :as document]
  ;;  [cljs.nodejs :as nodejs]
   [clojure.tools.cli :refer [parse-opts]]
   ["./core" :refer [startServer]]))

;; (nodejs/enable-util-print!)

(def cli-options
  [["-v" "--version", "Get version information."]
   ["-h" "--help", "Show this help text."]
   ["-s" "--server", "Start an HTTP server which features a visual document editor."]
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
    server (get options :server)
    summary (get processed-arguments :summary)
    source (first (get processed-arguments :arguments))]
    (cond
      help (show-help summary)
      server (startServer source destination)
      source (document/compile_ source destination)
      version (show-version)
      :else (show-error))))

(set! *main-cli-fn* -main)