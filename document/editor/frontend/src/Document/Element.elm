module Document.Element exposing (AdditionType(..), Element(..), SelectionPurpose(..), SelectionType(..), emptyBone, emptyFlesh)


type Element
    = Bone { id : Int, descriptor : String, alternateHierarchy : Bool, selected : Bool, babyId : Maybe Int }
    | Flesh { id : Int, for : String, attributes : String, content : String, selected : Bool, babyId : Maybe Int }
    | Text String
    | Body
    | Head
    | Name String
    | Root


type AdditionType
    = Before
    | After
    | InsideFirst
    | InsideLast
    | First
    | Last


type SelectionPurpose
    = Removal
    | Addition AdditionType


type SelectionType
    = Bones
    | FleshItems
    | All


emptyBone : Maybe Int -> Element
emptyBone babyId =
    Bone { id = 0, descriptor = "", alternateHierarchy = False, selected = False, babyId = babyId }


emptyFlesh : Maybe Int -> Element
emptyFlesh babyId =
    Flesh { id = 0, for = "", content = "(ignore)", attributes = "(ignore)", selected = False, babyId = babyId }
