module Test.Main where

import Prelude
import Bone.Descriptor as Descriptor
import Bone.Descriptor.Element.Targets as Targets
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Document.Body.Fills as Fills
import Document.Body.Holes as Holes
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign.Object as FObject
import Html as Html
import Html.Elements.Tree as HtmlElementsTree
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Tree as Tree
import Xml as Xml

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Bone Descriptor Element Targets" do
          it "Processes empty map for no flesh item" do
            (Targets.fromFleshItems []) `shouldEqual` Map.empty
          it "Processes targeted content" do
            ( Targets.fromFleshItems
                [ Xml.Flesh { for: "(=)title", content: Maybe.Just " is an ", attributes: Maybe.Nothing }
                , Xml.Flesh { for: "(>)title", content: Maybe.Just "amazing ", attributes: Maybe.Nothing }
                , Xml.Flesh { for: "title", content: Maybe.Just "title!", attributes: Maybe.Nothing }
                , Xml.Flesh { for: "(<)title", content: Maybe.Just "This", attributes: Maybe.Nothing }
                , Xml.Flesh { for: "content", content: Maybe.Just "Lorem Ipsum", attributes: Maybe.Nothing }
                ]
            )
              `shouldEqual`
                ( Map.singleton "title"
                    [ (Targets.TargetProperties Targets.Set Maybe.Nothing (Maybe.Just " is an "))
                    , (Targets.TargetProperties Targets.Append Maybe.Nothing (Maybe.Just "amazing "))
                    , (Targets.TargetProperties Targets.Append Maybe.Nothing (Maybe.Just "title!"))
                    , (Targets.TargetProperties Targets.Prepend Maybe.Nothing (Maybe.Just "This"))
                    ]
                    # Map.insert "content"
                        [ (Targets.TargetProperties Targets.Append Maybe.Nothing (Maybe.Just "Lorem Ipsum")) ]
                )
          it "Processes targeted attributes" do
            ( Targets.fromFleshItems
                [ Xml.Flesh { for: "(=)title", content: Maybe.Nothing, attributes: Maybe.Just $ FObject.singleton "class" "container" }
                , Xml.Flesh { for: "(>)title", content: Maybe.Nothing, attributes: Maybe.Just $ FObject.singleton "class" " mb-1" # FObject.insert "id" "title" }
                , Xml.Flesh { for: "(<)title", content: Maybe.Nothing, attributes: Maybe.Just $ FObject.singleton "id" "main-" }
                ]
            )
              `shouldEqual`
                ( Map.singleton "title"
                    [ (Targets.TargetProperties Targets.Set (Maybe.Just $ FObject.singleton "class" "container") Maybe.Nothing)
                    , (Targets.TargetProperties Targets.Append (Maybe.Just $ FObject.singleton "class" " mb-1" # FObject.insert "id" "title") Maybe.Nothing)
                    , (Targets.TargetProperties Targets.Prepend (Maybe.Just $ FObject.singleton "id" "main-") Maybe.Nothing)
                    ]
                )
          it "Processes multiple flesh items" do
            ( Targets.fromFleshItems
                [ Xml.Flesh { for: "title content", content: Maybe.Just "Lorem Ipsum", attributes: Maybe.Just (FObject.singleton "class" "container") }
                , Xml.Flesh { for: "body", content: Maybe.Just "More text", attributes: Maybe.Just (FObject.singleton "id" "body") }
                ]
            )
              `shouldEqual`
                ( Map.singleton "title"
                    [ (Targets.TargetProperties Targets.Append (Maybe.Just $ FObject.singleton "class" "container") (Maybe.Just "Lorem Ipsum"))
                    ]
                    # Map.insert "content"
                        [ (Targets.TargetProperties Targets.Append (Maybe.Just $ FObject.singleton "class" "container") (Maybe.Just "Lorem Ipsum"))
                        ]
                    # Map.insert "body"
                        [ (Targets.TargetProperties Targets.Append (Maybe.Just $ FObject.singleton "id" "body") (Maybe.Just "More text"))
                        ]
                )
          it "Shouldn't register blank targets of flesh items" do
            ( Targets.fromFleshItems
                [ Xml.Flesh { for: "", content: Maybe.Just "Lorem Ipsum", attributes: Maybe.Nothing }
                , Xml.Flesh { for: "    ", content: Maybe.Just "Lorem Ipsum", attributes: Maybe.Nothing }
                , Xml.Flesh { for: "hey         now", content: Maybe.Just "Lorem Ipsum", attributes: Maybe.Nothing }
                ]
            )
              `shouldEqual`
                ( Map.singleton "hey" [ (Targets.TargetProperties Targets.Append Maybe.Nothing (Maybe.Just "Lorem Ipsum")) ]
                    # Map.insert "now" [ (Targets.TargetProperties Targets.Append Maybe.Nothing (Maybe.Just "Lorem Ipsum")) ]
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
                (Map.singleton "content" [ Targets.TargetProperties Targets.Set Maybe.Nothing $ Maybe.Just "Lorem Ipsum" ])
                []
                "src"
            )
              `shouldEqual`
                Tree.tree Xml.Root [ Tree.Tree (Xml.Element { name: "div", attributes: FObject.empty }) [ Tree.singleton (Xml.Text "Lorem Ipsum") ] ]
          it ("Processes document content with nested bone") do
            HtmlElementsTree.fromDocumentContent ("<document><body><bone descriptor='html'><bone descriptor='head body'/></bone></body></document>") "src"
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
            HtmlElementsTree.fromDocumentContent "<document><body><bone descriptor='hole#content span@span'/><bone descriptor='hole#content2'/><bone descriptor='fill#content.append p@p'/><bone descriptor='fill#content2 p@p2'/><flesh for='p span'>Lorem Ipsum</flesh><flesh for='p2'>Lorem Ipsum2</flesh></body></document>" "src"
              `shouldEqual`
                ( Tree.Tree Xml.Root
                    [ Tree.Tree
                        (Xml.Element { name: "span", attributes: FObject.empty })
                        [ Tree.singleton (Xml.Text "Lorem Ipsum") ]
                    , Tree.Tree
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
          it "Processes external documents" do
            HtmlElementsTree.fromDocumentContent "<document><body><bone descriptor='page#base'/></body></document>" "test/site"
              `shouldEqual`
                ( Tree.Tree Xml.Root
                    [ Tree.tree
                        ( Xml.Element
                            { name: "h1", attributes: FObject.empty }
                        )
                        [ Tree.singleton $ Xml.Text "" ]
                    , Tree.tree
                        ( Xml.Element
                            { name: "p", attributes: FObject.empty }
                        )
                        [ Tree.singleton $ Xml.Text "" ]
                    , Tree.tree
                        ( Xml.Element
                            { name: "div", attributes: FObject.empty }
                        )
                        [ Tree.singleton $ Xml.Text "" ]
                    ]
                )
        describe "Document Fills" do
          it "Collects fills from document body" do
            Fills.fromBody
              ( Tree.Tree Xml.Body
                  [ Tree.Tree
                      ( Xml.Element
                          { name: "fill", attributes: FObject.fromHomogeneous { id: "content", class: "prepend" } }
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
                        ( Tuple.Tuple Fills.Prepend
                            [ Tree.Tree
                                (Xml.Element { name: "p", attributes: FObject.empty })
                                [ Tree.singleton (Xml.Text "Lorem Ipsum") ]
                            ]
                        )
                )
          it "Fills holes by prepending, appending, and setting" do
            ( Holes.fill
                ( Map.singleton "content"
                    ( Tuple.Tuple Fills.Prepend
                        [ Tree.Tree
                            (Xml.Element { name: "p", attributes: FObject.empty })
                            [ Tree.singleton (Xml.Text "Lorem Ipsum") ]
                        ]
                    )
                    # Map.insert "more-content"
                        ( Tuple.Tuple Fills.Append
                            [ Tree.Tree
                                (Xml.Element { name: "p", attributes: FObject.empty })
                                [ Tree.singleton (Xml.Text "Lorem Ipsum") ]
                            ]
                        )
                    # Map.insert "even-more-content"
                        ( Tuple.Tuple Fills.Set
                            [ Tree.Tree
                                (Xml.Element { name: "p", attributes: FObject.empty })
                                [ Tree.singleton (Xml.Text "Lorem Ipsum") ]
                            ]
                        )
                )
                ( Tree.tree Xml.Root
                    [ Tree.tree (Xml.Element { name: "hole", attributes: FObject.fromHomogeneous { id: "content" } })
                        [ Tree.tree (Xml.Element { name: "span", attributes: FObject.empty }) [ Tree.singleton (Xml.Text "Text inside span.") ]
                        ]
                    , Tree.tree (Xml.Element { name: "hole", attributes: FObject.fromHomogeneous { id: "more-content" } })
                        [ Tree.tree (Xml.Element { name: "span", attributes: FObject.empty }) [ Tree.singleton (Xml.Text "Text inside span.") ]
                        ]
                    , Tree.tree (Xml.Element { name: "hole", attributes: FObject.fromHomogeneous { id: "even-more-content" } })
                        [ Tree.tree (Xml.Element { name: "span", attributes: FObject.empty }) [ Tree.singleton (Xml.Text "Text inside span.") ]
                        ]
                    ]
                )
            )
              `shouldEqual`
                ( Tree.tree Xml.Root
                    [ Tree.tree (Xml.Element { name: "p", attributes: FObject.empty }) [ Tree.singleton $ Xml.Text "Lorem Ipsum" ]
                    , Tree.tree
                        (Xml.Element { name: "span", attributes: FObject.empty })
                        [ Tree.singleton (Xml.Text "Text inside span.") ]
                    , Tree.tree
                        (Xml.Element { name: "span", attributes: FObject.empty })
                        [ Tree.singleton (Xml.Text "Text inside span.") ]
                    , Tree.tree (Xml.Element { name: "p", attributes: FObject.empty }) [ Tree.singleton $ Xml.Text "Lorem Ipsum" ]
                    , Tree.tree (Xml.Element { name: "p", attributes: FObject.empty }) [ Tree.singleton $ Xml.Text "Lorem Ipsum" ]
                    ]
                )
        describe "HTML" do
          it "Generates HTML from document content" do
            Html.fromDocumentContent "src" "<document><body><bone descriptor=\"div@content p\"/><flesh for=\"content\">Hey</flesh></body></document>" `shouldEqual` "<div>Hey<p></p>\n</div>"
          it "Minifies content of flesh items" do
            Html.fromDocumentContent "src" "<document><body><bone descriptor=\"div@div\"/><flesh for=\"div\">Hey\n    now\nbrown\n            cow!</flesh></body></document>" `shouldEqual` "<div>Hey now brown cow!</div>"
          it "Processes special text" do
            Html.fromDocumentContent "src" "<document><body><bone descriptor=\"div@div\"/><flesh for=\"div\">Hello{space}freaking{space}world!{new-line}Hello again.</flesh></body></document>" `shouldEqual` "<div>Hello&nbsp;freaking&nbsp;world!<br />Hello again.</div>"
