module Menu exposing (MenuItem(..), MenuCategory(..), categoryName, categoryToString, menuItemId, menuItemName, menuItemCategory, menuItemPrice)


type MenuCategory
    = Base
    | Noodle
    | Topping
    | Grilled
    | Drink


type MenuItem
    = StandardItem { id : String, name : String, price : Int, category : MenuCategory }
    | OkonomiyakiItem
        { id : String
        , name : String
        , price : Int
        , defaultNoodle : Maybe MenuItem
        , category : MenuCategory
        }
    | NoodleItem { id : String, name : String, basePrice : Int, pricePerHalfBall : Int, category : MenuCategory }


menuItemId : MenuItem -> String
menuItemId item =
    case item of
        StandardItem r ->
            r.id

        OkonomiyakiItem r ->
            r.id

        NoodleItem r ->
            r.id


menuItemName : MenuItem -> String
menuItemName item =
    case item of
        StandardItem r ->
            r.name

        OkonomiyakiItem r ->
            r.name

        NoodleItem r ->
            r.name


menuItemCategory : MenuItem -> MenuCategory
menuItemCategory item =
    case item of
        StandardItem r ->
            r.category

        OkonomiyakiItem r ->
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


-- MenuItem の種類に応じた価格計算
menuItemPrice : MenuItem -> Int -> Int
menuItemPrice item quantity =
    case item of
        StandardItem r ->
            r.price * quantity

        OkonomiyakiItem r ->
            r.price * quantity

        NoodleItem r ->
            r.basePrice + r.pricePerHalfBall * quantity
