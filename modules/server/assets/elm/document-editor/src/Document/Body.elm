module Document.Body exposing (addElementAbsolute, addElementRelative, allFleshIds, applyIndex, expireBabyElement, mapElements, markAlternateHierarchy, removeElement, replaceElement)

import Document.Element exposing (Element(..))
import Tree exposing (Tree)
import Tree.Zipper


applyIndex : Tree Element -> Tree Element
applyIndex tree =
    Tree.indexedMap
        (\index element ->
            case element of
                Bone bone ->
                    Bone { bone | id = index }

                Flesh flesh ->
                    Flesh { flesh | id = index }

                _ ->
                    element
        )
        tree


markAlternateHierarchy : Tree.Zipper.Zipper Element -> Tree Element
markAlternateHierarchy zipper =
    let
        newZipper =
            case Tree.Zipper.label zipper of
                Bone currentBone ->
                    case Tree.Zipper.parent zipper of
                        Just parentZipper ->
                            let
                                parent =
                                    parentZipper |> Tree.Zipper.label
                            in
                            case parent of
                                Bone parentBone ->
                                    zipper |> Tree.Zipper.replaceLabel (Bone { currentBone | alternateHierarchy = not parentBone.alternateHierarchy }) |> Tree.Zipper.forward

                                _ ->
                                    Tree.Zipper.forward zipper

                        _ ->
                            Tree.Zipper.forward zipper

                _ ->
                    Tree.Zipper.forward zipper
    in
    case newZipper of
        Nothing ->
            Tree.Zipper.toTree zipper

        Just justNewZipper ->
            markAlternateHierarchy justNewZipper


checkElementWithIndex : Int -> Element -> Bool
checkElementWithIndex index element =
    case element of
        Bone bone ->
            bone.id == index

        Flesh flesh ->
            flesh.id == index

        _ ->
            False


checkElementWithBabyId : Int -> Element -> Bool
checkElementWithBabyId id element =
    case element of
        Bone bone ->
            bone.babyId == Just id

        Flesh flesh ->
            flesh.babyId == Just id

        _ ->
            False


replaceElement : Int -> (Element -> Element) -> Tree Element -> Tree Element
replaceElement index replace tree =
    let
        zipper =
            Tree.Zipper.fromTree tree
    in
    case Tree.Zipper.findFromRoot (checkElementWithIndex index) zipper of
        Just newZipper ->
            let
                replacement =
                    replace (newZipper |> Tree.Zipper.label)
            in
            Tree.Zipper.replaceLabel replacement newZipper
                |> Tree.Zipper.toTree

        Nothing ->
            tree


expireBabyElement : Int -> Tree Element -> Tree Element
expireBabyElement id tree =
    let
        zipper =
            Tree.Zipper.fromTree tree

        replace =
            \element ->
                case element of
                    Bone bone ->
                        Bone { bone | babyId = Nothing }

                    Flesh flesh ->
                        Flesh { flesh | babyId = Nothing }

                    _ ->
                        element
    in
    case Tree.Zipper.findFromRoot (checkElementWithBabyId id) zipper of
        Just newZipper ->
            Tree.Zipper.replaceLabel (newZipper |> Tree.Zipper.label |> replace) newZipper
                |> Tree.Zipper.toTree

        Nothing ->
            tree


removeElement : Int -> Tree Element -> Tree Element
removeElement index tree =
    let
        zipper =
            Tree.Zipper.fromTree tree
    in
    case Tree.Zipper.findFromRoot (checkElementWithIndex index) zipper of
        Just newZipper ->
            case Tree.Zipper.removeTree newZipper of
                Just newZipperAfterRemoval ->
                    newZipperAfterRemoval |> Tree.Zipper.toTree |> applyIndex

                Nothing ->
                    Tree.tree Root []

        Nothing ->
            tree


mapElements : (Element -> Element) -> Tree Element -> Tree Element
mapElements map tree =
    Tree.map map tree


addElementRelative : Document.Element.AdditionType -> Int -> Element -> Tree Element -> Tree Element
addElementRelative type_ index element body =
    case Tree.Zipper.fromTree body |> Tree.Zipper.findFromRoot (checkElementWithIndex index) of
        Just newZipper ->
            let
                newTree =
                    Tree.tree element []

                newZipper2 =
                    case type_ of
                        Document.Element.Before ->
                            Tree.Zipper.prepend newTree newZipper

                        Document.Element.After ->
                            Tree.Zipper.append newTree newZipper

                        _ ->
                            let
                                newCurrentTree =
                                    Tree.Zipper.tree newZipper
                                        |> (if type_ == Document.Element.InsideFirst then
                                                Tree.prependChild newTree

                                            else
                                                Tree.appendChild newTree
                                           )
                            in
                            Tree.Zipper.replaceTree newCurrentTree newZipper
            in
            newZipper2
                |> Tree.Zipper.toTree
                |> applyIndex

        Nothing ->
            body


addElementAbsolute : Document.Element.AdditionType -> Element -> Tree Element -> Tree Element
addElementAbsolute type_ element tree =
    let
        newChild =
            Tree.tree element []
    in
    tree
        |> (if type_ == Document.Element.First then
                Tree.prependChild newChild

            else
                Tree.appendChild newChild
           )
        |> applyIndex


allFleshIds : Tree Element -> List Int
allFleshIds tree =
    tree
        |> Tree.foldl
            (\label acc ->
                case label of
                    Flesh { id } ->
                        id :: acc

                    _ ->
                        acc
            )
            []
