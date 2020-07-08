module Document.Element exposing (Element(..))


type Element
    = Bone { id : Int, descriptor : String, alternateHierarchy : Bool, selected : Bool }
    | Flesh { id : Int, targets : String, content : String, selected : Bool }
    | Text String
    | Root
