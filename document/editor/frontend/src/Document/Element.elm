module Document.Element exposing (Element(..))


type Element
    = Bone { id : Int, descriptor : String }
    | Flesh { id : Int, for : String, content : String }
    | Text String
    | Root
