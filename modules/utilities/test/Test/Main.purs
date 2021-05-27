module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Effect.Aff (launchAff_)
import Main (isPublic, isDocument)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Distinguishes document from non-document files" do
          it "Recognizes document files" do
            isDocument "lorem.od" `shouldEqual` true
          it "Recognizes non-document files" do
            isDocument "lorem.html" `shouldEqual` false
          describe "Detects the visibility of documents" do
            it "Recognizes public documents" do
              isPublic "lorem.od" `shouldEqual` true
            it "Recognizes private documents" do
              isPublic "_lorem.od" `shouldEqual` false
            it "Defaults to private for invalid file extensions" do
              isPublic "lorem.css" `shouldEqual` false
