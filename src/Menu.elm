module Menu exposing (MenuItem(..), MenuCategory(..), categoryName, categoryToString, menuItemId, menuItemName, menuItemCategory, menuItemPrice)


type MenuCategory
    = Base
    | Topping
    | Grilled
    | Drink


type MenuItem
    = StandardItem { id : String, name : String, price : Int, category : MenuCategory }


menuItemId : MenuItem -> String
menuItemId item =
    case item of
        StandardItem r ->
            r.id


menuItemName : MenuItem -> String
menuItemName item =
    case item of
        StandardItem r ->
            r.name


menuItemCategory : MenuItem -> MenuCategory
menuItemCategory item =
    case item of
        StandardItem r ->
            r.category


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


-- MenuItem の種類に応じた価格計算
menuItemPrice : MenuItem -> Int -> Int
menuItemPrice item quantity =
    case item of
        StandardItem r ->
            r.price * quantity
