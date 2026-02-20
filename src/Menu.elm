module Menu exposing (MenuItem, MenuCategory(..), categoryName, categoryToString)


type MenuCategory
    = Base
    | Topping
    | Grilled
    | Drink


type alias MenuItem =
    { id : String, name : String, price : Int, category : MenuCategory }


categoryName : MenuCategory -> String
categoryName category =
    case category of
        Base ->
            "お好み焼き"

        Topping ->
            "トッピング"

        Grilled ->
            "焼き物"

        Drink ->
            "飲み物"


categoryToString : MenuCategory -> String
categoryToString category =
    case category of
        Base ->
            "base"

        Topping ->
            "topping"

        Grilled ->
            "grilled"

        Drink ->
            "drink"
