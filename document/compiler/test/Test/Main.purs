module Test.Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Bone.Descriptor.Element.Targets as Targets
import Xml as Xml
import Data.Map as Map
import Prelude
import Bone.Descriptor as Descriptor

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Bone Descriptor Element Targets" do
          it "Processes empty map for no flesh item" do
            (Targets.fromFleshItems []) `shouldEqual` Map.empty
          it "Processes single flesh item" do
            (Targets.fromFleshItems [ Xml.Flesh { targets: "title", content: "Lorem Ipsum" } ]) `shouldEqual` Map.singleton "title" "Lorem Ipsum"
          it "Processes multiple flesh items" do
            (Targets.fromFleshItems [ Xml.Flesh { targets: "title content", content: "Lorem Ipsum" }, Xml.Flesh { targets: "body", content: "Lorem Ipsum again" } ]) `shouldEqual` (Map.singleton "title" "Lorem Ipsum" # Map.insert "content" "Lorem Ipsum" # Map.insert "body" "Lorem Ipsum again")
          it "Processes flesh items with conflicting targets" do
            (Targets.fromFleshItems [ Xml.Flesh { targets: "title", content: "Lorem Ipsum" }, Xml.Flesh { targets: "title", content: "Lorem Ipsum again" } ]) `shouldEqual` Map.singleton "title" "Lorem Ipsum again"
        describe "Bone Descriptor" do
          it "Processes empty descriptor" do
            Descriptor.toElements "" `shouldEqual` [ Descriptor.emptyElement ]
          it "Processes descriptor with multiple elements" do
            Descriptor.toElements "html body div" `shouldEqual` [ Descriptor.emptyElement { name = "html" }, Descriptor.emptyElement { name = "body" }, Descriptor.emptyElement { name = "div" } ]
          it "Processes descriptor with multiple elements with complex properties" do
            Descriptor.toElements "div.container hole#content input.btn.btn-primary[style='margin-left: 1em' type='submit' value='Submit']@button." `shouldEqual` [ Descriptor.emptyElement { name = "div", xClass = "container " }, Descriptor.emptyElement { name = "hole", xId = "content" }, Descriptor.emptyElement { name = "input", xClass = "btn btn-primary ", attributes = "style='margin-left: 1em' type='submit' value='Submit'", id = "button", hasClosingTag = false } ]
