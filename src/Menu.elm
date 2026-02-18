module Menu exposing (MenuItem(..), MenuCategory(..), categoryName, categoryToString, menuItemId, menuItemName, menuItemCategory)


type MenuCategory
    = Base
    | Noodle
    | Topping
    | Grilled
    | Drink


type MenuItem
    = StandardItem { id : String, name : String, price : Int, category : MenuCategory }
    | NoodleItem { id : String, name : String, basePrice : Int, pricePerHalfBall : Int, category : MenuCategory }


menuItemId : MenuItem -> String
menuItemId item =
    case item of
        StandardItem r ->
            r.id

        NoodleItem r ->
            r.id


menuItemName : MenuItem -> String
menuItemName item =
    case item of
        StandardItem r ->
            r.name

        NoodleItem r ->
            r.name


menuItemCategory : MenuItem -> MenuCategory
menuItemCategory item =
    case item of
        StandardItem r ->
            r.category

        NoodleItem r ->
            r.category


categoryName : MenuCategory -> String
categoryName category =
    case category of
        Base ->
            "お好み焼き"

        Noodle ->
            "麺"

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

        Noodle ->
            "noodle"

        Topping ->
            "topping"

        Grilled ->
            "grilled"

        Drink ->
            "drink"
