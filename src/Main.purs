module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Options.Applicative

foreign import startServer :: String -> String -> Int -> Boolean

type Arguments
  = { version :: Boolean, server :: Boolean, port :: Int, destinationDirectory :: String, sourceDirectory :: String }

argumentsParser :: Parser Arguments
argumentsParser = ado
  version <-
    switch
      ( long "version"
          <> short 'v'
          <> help "Display version of Onwebed."
      )
  server <-
    switch
      ( long "server"
          <> short 's'
          <> help "Start a server which hosts the visual editor."
      )
  port <-
    option int
      ( long "port"
          <> short 'p'
          <> value 3000
          <> metavar "PORT"
          <> help "Port which the server will use."
      )
  destinationDirectory <-
    strOption
      ( long "dest"
          <> metavar "DESTINATION_PATH"
          <> short 'd'
          <> value "build"
          <> help "Directory to save compiled files."
      )
  sourceDirectory <-
    argument str
      ( metavar "SOURCE_PATH"
          <> value ""
          <> help "Directory where your documents reside."
      )
  in { version: version, server: server, port: port, destinationDirectory: destinationDirectory, sourceDirectory: sourceDirectory }

main :: Effect Unit
main = do
  run =<< execParser opts
  where
  opts =
    info (argumentsParser <**> helper)
      ( fullDesc
          <> progDesc "A template system for static web pages."
          <> header "Onwebed 0.1.0."
      )

showVersion :: Effect Unit
showVersion = log "v0.1.0"

run :: Arguments -> Effect Unit
run arguments =
  if arguments.server then
    if (startServer arguments.sourceDirectory arguments.destinationDirectory arguments.port) then
      log ("Server started at: localhost:" <> (show arguments.port))
    else
      log "Failed to start server."
  else
    if arguments.version then
      showVersion
    else
      log "Compiling..."
