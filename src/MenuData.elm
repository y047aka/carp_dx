module MenuData exposing
    ( allMenuItems
    , toppingIkaten
    , toppingMochi
    , toppingNegi
    , toppingGarlic
    , toppingCheese
    , toppingSquid
    , toppingShrimp
    , grilledKaki
    , grilledHotate
    , grilledIka
    , grilledNegiYaki
    , grilledTonpei
    , drinkBeer
    , drinkSoft
    )

import Menu exposing (MenuCategory(..), MenuItem(..))
import Okonomiyaki



-- トッピング


toppingIkaten : MenuItem
toppingIkaten =
    StandardItem
        { id = "topping-ikaten"
        , name = "イカ天"
        , price = 200
        , category = Topping
        }


toppingMochi : MenuItem
toppingMochi =
    StandardItem
        { id = "topping-mochi"
        , name = "もち"
        , price = 200
        , category = Topping
        }


toppingNegi : MenuItem
toppingNegi =
    StandardItem
        { id = "topping-negi"
        , name = "ねぎかけ"
        , price = 250
        , category = Topping
        }


toppingGarlic : MenuItem
toppingGarlic =
    StandardItem
        { id = "topping-garlic"
        , name = "ニンニク"
        , price = 250
        , category = Topping
        }


toppingCheese : MenuItem
toppingCheese =
    StandardItem
        { id = "topping-cheese"
        , name = "チーズ"
        , price = 300
        , category = Topping
        }


toppingSquid : MenuItem
toppingSquid =
    StandardItem
        { id = "topping-squid"
        , name = "イカ"
        , price = 400
        , category = Topping
        }


toppingShrimp : MenuItem
toppingShrimp =
    StandardItem
        { id = "topping-shrimp"
        , name = "エビ"
        , price = 400
        , category = Topping
        }



-- 焼き物


grilledKaki : MenuItem
grilledKaki =
    StandardItem
        { id = "grilled-kaki"
        , name = "カキ焼き"
        , price = 1500
        , category = Grilled
        }


grilledHotate : MenuItem
grilledHotate =
    StandardItem
        { id = "grilled-hotate"
        , name = "ホタテ焼き"
        , price = 1000
        , category = Grilled
        }


grilledIka : MenuItem
grilledIka =
    StandardItem
        { id = "grilled-ika"
        , name = "イカ焼き"
        , price = 900
        , category = Grilled
        }


grilledNegiYaki : MenuItem
grilledNegiYaki =
    StandardItem
        { id = "grilled-negi-yaki"
        , name = "ネギ焼き"
        , price = 1000
        , category = Grilled
        }


grilledTonpei : MenuItem
grilledTonpei =
    StandardItem
        { id = "grilled-tonpei"
        , name = "とん平"
        , price = 1000
        , category = Grilled
        }



-- 飲み物


drinkBeer : MenuItem
drinkBeer =
    StandardItem
        { id = "drink-beer"
        , name = "ビール（瓶）"
        , price = 750
        , category = Drink
        }


drinkSoft : MenuItem
drinkSoft =
    StandardItem
        { id = "drink-soft"
        , name = "ソフトドリンク"
        , price = 300
        , category = Drink
        }



-- 全メニュー項目のリスト


allMenuItems : List MenuItem
allMenuItems =
    [ -- お好み焼き
      Okonomiyaki.baseYasai
    , Okonomiyaki.baseSoba
    , Okonomiyaki.baseUdon
    , Okonomiyaki.baseZenbuIri

    -- 麺
    , Okonomiyaki.noodleSoba
    , Okonomiyaki.noodleUdon

    -- トッピング
    , toppingIkaten
    , toppingMochi
    , toppingNegi
    , toppingGarlic
    , toppingCheese
    , toppingSquid
    , toppingShrimp

    -- 焼き物
    , grilledKaki
    , grilledHotate
    , grilledIka
    , grilledNegiYaki
    , grilledTonpei

    -- 飲み物
    , drinkBeer
    , drinkSoft
    ]
