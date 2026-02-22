module MenuData exposing
    ( allMenuItems
    , baseYasai
    , baseSoba
    , baseUdon
    , baseZenbuIri
    , menuItemToBaseKind
    , grilledKaki
    , grilledHotate
    , grilledIka
    , grilledNegiYaki
    , grilledTonpei
    , drinkBeer
    , drinkSoft
    )

import Menu exposing (MenuCategory(..), MenuItem)
import Okonomiyaki exposing (BaseKind(..))



-- お好み焼きベース


{-| 野菜入りお好み焼き（デフォルト麺なし、900円）。 -}
baseYasai : MenuItem
baseYasai =
    { id = "base-yasai"
    , name = "野菜入り"
    , price = 900
    , category = Base
    }


{-| そば入りお好み焼き（1200円）。

`price` にそば1玉分の料金が含まれている。

-}
baseSoba : MenuItem
baseSoba =
    { id = "base-soba"
    , name = "そば入り"
    , price = 1200
    , category = Base
    }


{-| うどん入りお好み焼き（1200円）。

`price` にうどん1玉分の料金が含まれている。

-}
baseUdon : MenuItem
baseUdon =
    { id = "base-udon"
    , name = "うどん入り"
    , price = 1200
    , category = Base
    }


{-| 全部入りお好み焼き（ベース600円）。

麺の種類を選ばず、イカ・エビのトッピング込みで麺1玉時に1700円になる。
価格計算: basePrice(600) + noodlePrice(100 + 100×qty) + toppingsPrice(800)
例: そばまたはうどん1玉(qty=2): 600 + (100 + 100×2) + 800 = 1700円

-}
baseZenbuIri : MenuItem
baseZenbuIri =
    { id = "base-zenbu-iri"
    , name = "全部入り"
    , price = 600
    , category = Base
    }


{-| `MenuItem` から `BaseKind` へ変換する。

ベース用 `MenuItem` の `id` で検索し、対応する `BaseKind` を返す。
ベース以外の `MenuItem` は `Nothing` を返す。

-}
menuItemToBaseKind : MenuItem -> Maybe Okonomiyaki.BaseKind
menuItemToBaseKind menuItem =
    baseMenuEntries
        |> List.filter (\e -> e.menuItem.id == menuItem.id)
        |> List.head
        |> Maybe.map .baseKind


baseMenuEntries : List { menuItem : MenuItem, baseKind : BaseKind }
baseMenuEntries =
    [ { menuItem = baseYasai, baseKind = Yasai }
    , { menuItem = baseSoba, baseKind = Soba }
    , { menuItem = baseUdon, baseKind = Udon }
    , { menuItem = baseZenbuIri, baseKind = ZenbuIri }
    ]



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
    (baseMenuEntries |> List.map .menuItem)
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
