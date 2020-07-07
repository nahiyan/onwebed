module Document.Element exposing (Element(..))


type Element
    = Bone { id : Int, descriptor : String, alternateHierarchy : Bool }
    | Flesh { id : Int, targets : String, content : String }
    | Text String
    | Root
