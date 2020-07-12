module Document.Element exposing (Element(..), emptyBone, emptyFlesh)


type Element
    = Bone { id : Int, descriptor : String, alternateHierarchy : Bool, selected : Bool, babyId : Maybe Int }
    | Flesh { id : Int, targets : String, content : String, selected : Bool, babyId : Maybe Int }
    | Text String
    | Body
    | Head
    | Name String
    | Root


emptyBone : Maybe Int -> Element
emptyBone babyId =
    Bone { id = 0, descriptor = "", alternateHierarchy = False, selected = False, babyId = babyId }


emptyFlesh : Maybe Int -> Element
emptyFlesh babyId =
    Flesh { id = 0, targets = "", content = "", selected = False, babyId = babyId }
