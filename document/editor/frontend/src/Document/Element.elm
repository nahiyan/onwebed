module Document.Element exposing (Element(..), emptyBone, emptyFlesh)


type Element
    = Bone { id : Int, descriptor : String, alternateHierarchy : Bool, selected : Bool }
    | Flesh { id : Int, targets : String, content : String, selected : Bool }
    | Text String
    | Root


emptyBone : Element
emptyBone =
    Bone { id = 0, descriptor = "", alternateHierarchy = False, selected = False }


emptyFlesh : Element
emptyFlesh =
    Flesh { id = 0, targets = "", content = "", selected = False }
