module Tree
  ( Tree(..)
  , singleton
  , tree
  , label
  , children
  , mapLabel
  , replaceLabel
  , mapChildren
  , replaceChildren
  , prependChild
  , appendChild
  , foldl
  , foldr
  , count
  , flatten
  , map
  , indexedMap
  , mapAccumulate
  , map2
  , indexedMap2
  , mapAccumulate2
  , andMap
  , unfold
  , restructure
  ) where

import Data.Array as Array
import Prelude
import Data.Tuple as Tuple
import Data.Maybe as Maybe
import Data.Argonaut.Decode as JsonDecode
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.:?))
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Argonaut.Core as JsonCore
import Xml as Xml
import Foreign.Object as FObject

data Empty
  = Empty

data Tree a
  = Tree a (Array (Tree a))

instance compareXmlElementTrees :: Eq (Tree Xml.Element) where
  eq a b = flatten a == flatten b

instance showXmlElementTree :: Show (Tree Xml.Element) where
  show a = flatten a # show

instance encodeJsonElement :: EncodeJson (Tree Xml.Element) where
  encodeJson (Tree element children_) = case element of
    Xml.Root ->
      "elements" := children_
        ~> JsonCore.jsonEmptyObject
    Xml.Body ->
      "type" := "element"
        ~> ("name" := "body")
        ~> ("elements" := children_)
        ~> JsonCore.jsonEmptyObject
    Xml.Head ->
      "type" := "element"
        ~> ("name" := "head")
        ~> ("elements" := children_)
        ~> JsonCore.jsonEmptyObject
    Xml.Document ->
      "type" := "element"
        ~> ("name" := "document")
        ~> ("elements" := children_)
        ~> JsonCore.jsonEmptyObject
    Xml.Text text ->
      "type" := "text"
        ~> ("text" := text)
        ~> JsonCore.jsonEmptyObject
    Xml.Element { name } ->
      "type" := "element"
        ~> ("name" := name)
        ~> ("elements" := children_)
        ~> JsonCore.jsonEmptyObject
    _ ->
      "type" := "element"
        ~> ("name" := "undefined")
        ~> JsonCore.jsonEmptyObject

instance decodeJsonElement :: DecodeJson (Tree Xml.Element) where
  decodeJson json = do
    obj <- JsonDecode.decodeJson json
    maybeType <- obj .:? "type"
    maybeName <- obj .:? "name"
    maybeText <- obj .:? "text"
    maybeChildren <- obj .:? "elements"
    maybeAttributes <- obj .:? "attributes"
    case maybeType of
      Maybe.Just type_ ->
        if type_ == "element" then
          let
            children_ = Maybe.fromMaybe [] maybeChildren

            attributes = Maybe.fromMaybe FObject.empty maybeAttributes
          in
            case Maybe.fromMaybe "undefined" maybeName of
              "bone" ->
                let
                  descriptor = attributes # FObject.lookup "descriptor" # Maybe.fromMaybe ""
                in
                  pure $ tree (Xml.Bone { descriptor: descriptor }) children_
              "flesh" ->
                let
                  content =
                    children_
                      # Array.foldl
                          ( \acc (Tree element _) -> case element of
                              Xml.Text text -> acc <> text
                              _ -> acc
                          )
                          ""

                  targets = attributes # FObject.lookup "for" # Maybe.fromMaybe ""
                in
                  pure $ singleton (Xml.Flesh { targets: targets, content: content })
              "document" -> pure $ tree Xml.Document children_
              "head" -> pure $ tree Xml.Head children_
              "body" -> pure $ tree Xml.Body children_
              name -> pure $ tree (Xml.Element { name: name, attributes: attributes }) children_
        else
          pure $ singleton (Xml.Text (Maybe.fromMaybe "" maybeText))
      Maybe.Nothing -> pure $ tree Xml.Root (Maybe.fromMaybe [] maybeChildren)

singleton :: forall a. a -> Tree a
singleton v = Tree v []

tree :: forall a. a -> Array (Tree a) -> Tree a
tree = Tree

label :: forall a. Tree a -> a
label (Tree v _) = v

mapLabel :: forall a. (a -> a) -> Tree a -> Tree a
mapLabel f (Tree v cs) = Tree (f v) cs

replaceLabel :: forall a. a -> Tree a -> Tree a
replaceLabel v (Tree _ cs) = Tree v cs

children :: forall a. Tree a -> Array (Tree a)
children (Tree _ c) = c

mapChildren :: forall a. (Array (Tree a) -> Array (Tree a)) -> Tree a -> Tree a
mapChildren f (Tree v cs) = Tree v (f cs)

replaceChildren :: forall a. Array (Tree a) -> Tree a -> Tree a
replaceChildren cs (Tree v _) = Tree v cs

prependChild :: forall a. Tree a -> Tree a -> Tree a
prependChild c (Tree v cs) = Tree v ([ c ] <> cs)

appendChild :: forall a. Tree a -> Tree a -> Tree a
appendChild c (Tree v cs) = Tree v (cs <> [ c ])

count :: forall a. Tree a -> Int
count t = foldl (\_ x -> x + 1) 0 t

foldl :: forall a b. (a -> b -> b) -> b -> Tree a -> b
foldl f acc t = foldlHelp f acc [ t ] []

foldr :: forall a b. (b -> a -> b) -> b -> Tree a -> b
foldr f acc t = Array.foldl f acc $ foldl (\item acc_ -> [ item ] <> acc_) [] t

foldlHelp :: forall a b. (a -> b -> b) -> b -> Array (Tree a) -> Array (Array (Tree a)) -> b
foldlHelp f acc trees nextSets =
  let
    rest = trees # Array.tail # Maybe.fromMaybe []
  in
    case trees # Array.head of
      Maybe.Nothing -> case nextSets of
        [] -> acc
        _ ->
          let
            set = nextSets # Array.head # Maybe.fromMaybe []

            sets = nextSets # Array.tail # Maybe.fromMaybe []
          in
            foldlHelp f acc set sets
      Maybe.Just (Tree d []) -> foldlHelp f (f d acc) rest nextSets
      Maybe.Just (Tree d xs) -> foldlHelp f (f d acc) xs ([ rest ] <> nextSets)

flatten :: forall a. Tree a -> Array a
flatten t = foldr (\acc item -> [ item ] <> acc) [] t

unfold :: forall a b. (b -> (Tuple.Tuple a (Array b))) -> b -> Tree a
unfold f seed =
  let
    Tuple.Tuple v next = f seed
  in
    unfoldHelp f { todo: next, label: v, done: [] } []

unfoldHelp ::
  forall a b.
  (b -> (Tuple.Tuple a (Array b))) ->
  UnfoldAcc a b ->
  Array (UnfoldAcc a b) ->
  Tree a
unfoldHelp f acc stack =
  let
    xs = acc.todo # Array.tail # Maybe.fromMaybe []
  in
    case acc.todo # Array.head of
      Maybe.Nothing ->
        let
          node = Tree acc.label (Array.reverse acc.done)

          rest = stack # Array.tail # Maybe.fromMaybe []
        in
          case stack # Array.head of
            Maybe.Nothing -> node
            Maybe.Just top ->
              unfoldHelp f
                (top { done = [ node ] <> top.done })
                rest
      Maybe.Just x -> case f x of
        Tuple.Tuple label_ [] ->
          unfoldHelp f
            ( acc
                { todo = xs
                , done = [ singleton label_ ] <> acc.done
                }
            )
            stack
        Tuple.Tuple label_ todo ->
          unfoldHelp
            f
            { todo: todo
            , label: label_
            , done: []
            }
            ([ acc { todo = xs } ] <> stack)

type UnfoldAcc a b
  = { todo :: Array b
    , done :: Array (Tree a)
    , label :: a
    }

map :: forall a b. (a -> b) -> Tree a -> Tree b
map f t =
  mapAccumulate (\_ e -> Tuple.Tuple Empty (f e)) Empty t
    # Tuple.snd

indexedMap :: forall a b. (Int -> a -> b) -> Tree a -> Tree b
indexedMap f t =
  mapAccumulate (\idx elem -> Tuple.Tuple (idx + 1) (f idx elem)) 0 t
    # Tuple.snd

mapAccumulate :: forall a b s. (s -> a -> Tuple.Tuple s b) -> s -> Tree a -> Tuple.Tuple s (Tree b)
mapAccumulate f s (Tree d cs) =
  let
    Tuple.Tuple s_ d_ = f s d
  in
    mapAccumulateHelp f
      s_
      { todo: cs
      , done: []
      , label: d_
      }
      []

mapAccumulateHelp ::
  forall a b s.
  (s -> a -> Tuple.Tuple s b) ->
  s ->
  MapAcc a b ->
  Array (MapAcc a b) ->
  Tuple.Tuple s (Tree b)
mapAccumulateHelp f state acc stack =
  let
    rest = acc.todo # Array.tail # Maybe.fromMaybe []
  in
    case acc.todo # Array.head of
      Maybe.Nothing ->
        let
          node = Tree acc.label (Array.reverse acc.done)

          rest_ = stack # Array.tail # Maybe.fromMaybe []
        in
          case stack # Array.head of
            Maybe.Nothing -> Tuple.Tuple state node
            Maybe.Just top -> mapAccumulateHelp f state (top { done = [ node ] <> top.done }) rest_
      Maybe.Just (Tree d []) ->
        let
          Tuple.Tuple state_ label_ = f state d
        in
          mapAccumulateHelp f
            state_
            ( acc
                { todo = rest
                , done = [ Tree label_ [] ] <> acc.done
                }
            )
            stack
      Maybe.Just (Tree d cs) ->
        let
          Tuple.Tuple state_ label_ = f state d
        in
          mapAccumulateHelp f
            state_
            { todo: cs
            , done: []
            , label: label_
            }
            ([ acc { todo = rest } ] <> stack)

type MapAcc a b
  = { todo :: Array (Tree a)
    , done :: Array (Tree b)
    , label :: b
    }

map2 :: forall a b c. (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 f left right =
  mapAccumulate2 (\s a b -> Tuple.Tuple s (f a b)) Empty left right
    # Tuple.snd

indexedMap2 :: forall a b c. (Int -> a -> b -> c) -> Tree a -> Tree b -> Tree c
indexedMap2 f left right =
  mapAccumulate2 (\s a b -> Tuple.Tuple (s + 1) (f s a b)) 0 left right
    # Tuple.snd

andMap :: forall a b. Tree (a -> b) -> Tree a -> Tree b
andMap = map2 ($)

mapAccumulate2 :: forall a b c s. (s -> a -> b -> Tuple.Tuple s c) -> s -> Tree a -> Tree b -> Tuple.Tuple s (Tree c)
mapAccumulate2 f s_ (Tree a xs) (Tree b ys) =
  let
    Tuple.Tuple s z = f s_ a b
  in
    mapAccumulate2Help f
      s
      { todoL: xs
      , todoR: ys
      , done: []
      , label: z
      }
      []

mapAccumulate2Help ::
  forall a b c s.
  (s -> a -> b -> Tuple.Tuple s c) ->
  s ->
  Map2Acc a b c ->
  Array (Map2Acc a b c) ->
  Tuple.Tuple s (Tree c)
mapAccumulate2Help f state acc stack = case Tuple.Tuple acc.todoL acc.todoR of
  Tuple.Tuple left right ->
    let
      restL = left # Array.tail # Maybe.fromMaybe []

      restR = right # Array.tail # Maybe.fromMaybe []

      leftTree = left # Array.head

      rightTree = right # Array.head
    in
      case Tuple.Tuple leftTree rightTree of
        Tuple.Tuple Maybe.Nothing _ ->
          let
            node = Tree acc.label (Array.reverse acc.done)

            rest = stack # Array.tail # Maybe.fromMaybe []
          in
            case stack # Array.head of
              Maybe.Nothing -> Tuple.Tuple state node
              Maybe.Just top -> mapAccumulate2Help f state (top { done = [ node ] <> top.done }) rest
        Tuple.Tuple _ Maybe.Nothing ->
          let
            node = Tree acc.label (Array.reverse acc.done)

            rest = stack # Array.tail # Maybe.fromMaybe []
          in
            case stack # Array.head of
              Maybe.Nothing -> Tuple.Tuple state node
              Maybe.Just top -> mapAccumulate2Help f state (top { done = [ node ] <> top.done }) rest
        Tuple.Tuple (Maybe.Just justLeftTree) (Maybe.Just justRightTree) ->
          let
            (Tree a xs) = justLeftTree

            (Tree b ys) = justRightTree

            Tuple.Tuple state_ label_ = f state a b
          in
            mapAccumulate2Help f
              state_
              { todoL: xs
              , todoR: ys
              , done: []
              , label: label_
              }
              ([ acc { todoL = restL, todoR = restR } ] <> stack)

type Map2Acc a b c
  = { todoL :: Array (Tree a)
    , todoR :: Array (Tree b)
    , done :: Array (Tree c)
    , label :: c
    }

restructure :: forall a b c. (a -> b) -> (b -> Array c -> c) -> Tree a -> c
restructure convertLabel convertTree (Tree l c) =
  restructureHelp convertLabel
    convertTree
    { todo: c
    , label: convertLabel l
    , done: []
    }
    []

restructureHelp ::
  forall a b c.
  (a -> b) ->
  (b -> Array c -> c) ->
  ReAcc a b c ->
  Array (ReAcc a b c) ->
  c
restructureHelp fLabel fTree acc stack =
  let
    head = acc.todo # Array.head

    rest = acc.todo # Array.tail # Maybe.fromMaybe []
  in
    if Array.null acc.todo then
      let
        node = fTree acc.label (Array.reverse acc.done)
      in
        case stack # Array.head of
          Maybe.Nothing -> node
          Maybe.Just top ->
            let
              rest_ = Array.tail stack # Maybe.fromMaybe []
            in
              restructureHelp
                fLabel
                fTree
                (top { done = [ node ] <> top.done })
                rest_
    else case head of
      Maybe.Just (Tree l []) ->
        restructureHelp
          fLabel
          fTree
          ( acc
              { todo = rest
              , done = [ fTree (fLabel l) [] ] <> acc.done
              }
          )
          stack
      Maybe.Just (Tree l cs) ->
        restructureHelp
          fLabel
          fTree
          { todo: cs
          , done: []
          , label: fLabel l
          }
          ([ acc { todo = rest } ] <> stack)
      _ -> fTree acc.label (Array.reverse acc.done)

type ReAcc a b c
  = { todo :: Array (Tree a)
    , done :: Array c
    , label :: b
    }
