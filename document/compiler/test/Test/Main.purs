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
import Html.Elements.Tree as HtmlElementsTree
import Foreign.Object as FObject
import Tree as Tree
import Document.Body.Fills as Fills
import Html as Html

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
            ( Targets.fromFleshItems
                [ Xml.Flesh { targets: "title content", content: "Lorem Ipsum" }
                , Xml.Flesh { targets: "body", content: "More text" }
                ]
            )
              `shouldEqual`
                ( Map.singleton "title" "Lorem Ipsum"
                    # Map.insert "content" "Lorem Ipsum"
                    # Map.insert "body" "More text"
                )
          it "Processes flesh items with conflicting targets non-destructively" do
            ( Targets.fromFleshItems
                [ Xml.Flesh { targets: "title", content: "This is the " }
                , Xml.Flesh { targets: "title", content: "title" }
                ]
            )
              `shouldEqual`
                Map.singleton "title" "This is the title"
          it "Shouldn't register blank targets of flesh items" do
            ( Targets.fromFleshItems
                [ Xml.Flesh { targets: "", content: "Lorem Ipsum" }
                , Xml.Flesh { targets: "    ", content: "Lorem Ipsum" }
                , Xml.Flesh { targets: "hey         now", content: "Lorem Ipsum" }
                ]
            )
              `shouldEqual`
                ( Map.singleton "hey" "Lorem Ipsum"
                    # Map.insert "now" "Lorem Ipsum"
                )
          it "Merges targets non-destructively" do
            ( Targets.merge (Map.singleton "title" " the title.")
                ( Map.singleton "title" "This is"
                    # Map.insert "content" "Lorem Ipsum"
                )
            )
              `shouldEqual`
                ( Map.singleton "title" "This is the title."
                    # Map.insert "content" "Lorem Ipsum"
                )
        describe "Bone Descriptor" do
          it "Processes empty descriptor" do
            Descriptor.toElements "" `shouldEqual` []
          it "Processes descriptor with multiple elements" do
            Descriptor.toElements "html body div" `shouldEqual` [ Descriptor.emptyElement { name = "html" }, Descriptor.emptyElement { name = "body" }, Descriptor.emptyElement { name = "div" } ]
          it "Processes descriptor with multiple elements, and complex properties" do
            Descriptor.toElements "div.container hole#content input.btn.btn-primary(style='margin-left: 1em' type='submit' value='Submit')@button." `shouldEqual` [ Descriptor.emptyElement { name = "div", htmlClass = "container " }, Descriptor.emptyElement { name = "hole", htmlId = "content" }, Descriptor.emptyElement { name = "input", htmlClass = "btn btn-primary ", attributes = "style='margin-left: 1em' type='submit' value='Submit'", id = "button", hasClosingTag = false } ]
          it "Deals with misleading whitespace" do
            Descriptor.toElements "div    div" `shouldEqual` [ Descriptor.emptyElement { name = "div" }, Descriptor.emptyElement { name = "div" } ]
        describe "HTML Elements Tree" do
          it "Processes descriptor elements" do
            ( HtmlElementsTree.fromBoneDescriptorElements
                [ Descriptor.emptyElement { name = "div", id = "content" } ]
                (Map.singleton "content" "Lorem Ipsum")
                []
                "src"
            )
              `shouldEqual`
                Tree.Tree (Xml.Element { name: "div", attributes: FObject.empty }) [ Tree.singleton (Xml.Text "Lorem Ipsum") ]
          it "Processes document content with nested bone" do
            HtmlElementsTree.fromDocumentContent "<document><body><bone descriptor='html'><bone descriptor='head body'/></bone></body></document>" "src"
              `shouldEqual`
                ( Tree.Tree Xml.Root
                    [ Tree.Tree
                        ( Xml.Element
                            { name: "html", attributes: FObject.empty }
                        )
                        [ Tree.singleton (Xml.Text "")
                        , Tree.Tree
                            ( Xml.Element
                                { name: "head", attributes: FObject.empty }
                            )
                            [ Tree.singleton (Xml.Text "")
                            , Tree.Tree
                                ( Xml.Element
                                    { name: "body", attributes: FObject.empty }
                                )
                                [ Tree.singleton (Xml.Text "") ]
                            ]
                        ]
                    ]
                )
          it "Processes document content with bone and flesh" do
            HtmlElementsTree.fromDocumentContent "<document><body><bone descriptor='div p@content'/><flesh for='content'>Lorem Ipsum</flesh></body></document>" "src" `shouldEqual` (Tree.Tree Xml.Root [ Tree.Tree (Xml.Element { name: "div", attributes: FObject.empty }) [ Tree.singleton (Xml.Text ""), Tree.Tree (Xml.Element { name: "p", attributes: FObject.empty }) [ Tree.singleton (Xml.Text "Lorem Ipsum") ] ], Tree.singleton Xml.Blank ])
          it "Processes document content with holes and fills" do
            HtmlElementsTree.fromDocumentContent "<document><body><bone descriptor='hole#content'/><bone descriptor='hole#content2'/><bone descriptor='fill#content p@p'/><bone descriptor='fill#content2 p@p2'/><flesh for='p'>Lorem Ipsum</flesh><flesh for='p2'>Lorem Ipsum2</flesh></body></document>" "src"
              `shouldEqual`
                ( Tree.Tree Xml.Root
                    [ Tree.Tree
                        (Xml.Element { name: "p", attributes: FObject.empty })
                        [ Tree.singleton (Xml.Text "Lorem Ipsum") ]
                    , Tree.Tree
                        (Xml.Element { name: "p", attributes: FObject.empty })
                        [ Tree.singleton (Xml.Text "Lorem Ipsum2") ]
                    , Tree.singleton Xml.Blank
                    , Tree.singleton Xml.Blank
                    , Tree.singleton Xml.Blank
                    , Tree.singleton Xml.Blank
                    ]
                )
          it "Processes document content with complex bone descriptors" do
            HtmlElementsTree.fromDocumentContent "<document><body><bone descriptor='div@content b p'/><flesh for=\"content\">Hey!</flesh></body></document>" "src"
              `shouldEqual`
                ( Tree.Tree Xml.Root
                    [ Tree.Tree
                        ( Xml.Element
                            { name: "div", attributes: FObject.empty }
                        )
                        [ Tree.singleton (Xml.Text "Hey!")
                        , Tree.Tree
                            (Xml.Element { name: "b", attributes: FObject.empty })
                            [ Tree.singleton (Xml.Text "")
                            , Tree.Tree
                                (Xml.Element { name: "p", attributes: FObject.empty })
                                [ Tree.singleton (Xml.Text "") ]
                            ]
                        ]
                    , Tree.singleton Xml.Blank
                    ]
                )
        describe "Document Fills" do
          it "Collects fills from document body" do
            Fills.fromBody
              ( Tree.Tree Xml.Body
                  [ Tree.Tree
                      ( Xml.Element
                          { name: "fill", attributes: FObject.fromHomogeneous { id: "content" } }
                      )
                      [ Tree.Tree
                          (Xml.Element { name: "p", attributes: FObject.empty })
                          [ Tree.singleton (Xml.Text "Lorem Ipsum") ]
                      ]
                  ]
              )
              `shouldEqual`
                ( Map.empty
                    # Map.insert "content"
                        [ Tree.Tree
                            (Xml.Element { name: "p", attributes: FObject.empty })
                            [ Tree.singleton (Xml.Text "Lorem Ipsum") ]
                        ]
                )
        describe "HTML" do
          it "Generates HTML from document content" do
            Html.fromDocumentContent "src" "<document><body><bone descriptor=\"div@content p\"/><flesh for=\"content\">Hey</flesh></body></document>" `shouldEqual` "<div>Hey<p></p>\n</div>"
