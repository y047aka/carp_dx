module MenuData exposing
    ( allMenuItems
    , grilledKaki
    , grilledHotate
    , grilledIka
    , grilledNegiYaki
    , grilledTonpei
    , drinkBeer
    , drinkSoft
    )

import Menu exposing (MenuCategory(..), MenuItem)
import Okonomiyaki



-- 焼き物


grilledKaki : MenuItem
grilledKaki =
    { id = "grilled-kaki"
    , name = "カキ焼き"
    , price = 1500
    , category = Grilled
    }


grilledHotate : MenuItem
grilledHotate =
    { id = "grilled-hotate"
    , name = "ホタテ焼き"
    , price = 1000
    , category = Grilled
    }


grilledIka : MenuItem
grilledIka =
    { id = "grilled-ika"
    , name = "イカ焼き"
    , price = 900
    , category = Grilled
    }


grilledNegiYaki : MenuItem
grilledNegiYaki =
    { id = "grilled-negi-yaki"
    , name = "ネギ焼き"
    , price = 1000
    , category = Grilled
    }


grilledTonpei : MenuItem
grilledTonpei =
    { id = "grilled-tonpei"
    , name = "とん平"
    , price = 1000
    , category = Grilled
    }



-- 飲み物


drinkBeer : MenuItem
drinkBeer =
    { id = "drink-beer"
    , name = "ビール（瓶）"
    , price = 750
    , category = Drink
    }


drinkSoft : MenuItem
drinkSoft =
    { id = "drink-soft"
    , name = "ソフトドリンク"
    , price = 300
    , category = Drink
    }



-- 全メニュー項目のリスト


allMenuItems : List MenuItem
allMenuItems =
    -- お好み焼き
    (Okonomiyaki.allBases |> List.map .menuItem)
        ++ [ -- 焼き物
             grilledKaki
           , grilledHotate
           , grilledIka
           , grilledNegiYaki
           , grilledTonpei

           -- 飲み物
           , drinkBeer
           , drinkSoft
           ]
