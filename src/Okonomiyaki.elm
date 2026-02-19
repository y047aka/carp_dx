module Okonomiyaki exposing
    ( Addition
    , BaseOrderItem
    , baseYasai
    , baseSoba
    , baseUdon
    , okonomiyakiBases
    , noodleSoba
    , noodleUdon
    , allNoodles
    , noodleQuantityDisplay
    , baseForNoodle
    , isDefaultNoodleOf
    , initialBaseOrderItem
    , normalizeBaseOnNoodleAdd
    , normalizeBaseOnNoodleChange
    , calculateBaseItemTotal
    )

{-| お好み焼きドメインのモジュール。

お好み焼きのベース・麺・トッピングに関する型定義、マスタデータ、および
`BaseOrderItem` の構築・計算・正規化ロジックを提供する。

## 設計方針

  - **ベース** (`OkonomiyakiItem`) は `defaultNoodle` を持つ場合があり、
    その麺1玉分の価格はベースの `price` に含まれている。
  - **麺の数量** は内部的に「半玉単位」で管理する（quantity 2 = 1玉）。
    表示変換には `noodleQuantityDisplay` を使用する。
  - ベースと麺の整合性を保つ正規化関数は、麺の増減操作の直後に呼び出す。

-}

import Menu exposing (MenuCategory(..), MenuItem(..), menuItemId, menuItemPrice)


-- 型定義


{-| お好み焼きに付属する追加オプション（麺・トッピング）。

`quantity` の単位はオプションの種類に依存する。
麺は半玉単位（`2` = 1玉）、トッピングは個数単位。

-}
type alias Addition =
    { menuItem : MenuItem
    , quantity : Int
    }


{-| お好み焼き1件分の注文アイテム。

`baseItem` にベースの `OkonomiyakiItem` を持ち、
`noodles` と `toppings` にそれぞれの追加オプションを格納する。
`quantity` はこのお好み焼き自体の枚数を表す。

-}
type alias BaseOrderItem =
    { baseItem : MenuItem
    , quantity : Int
    , noodles : List Addition
    , toppings : List Addition
    }


-- マスタデータ：ベース


{-| 野菜入りお好み焼き（デフォルト麺なし、900円）。 -}
baseYasai : MenuItem
baseYasai =
    OkonomiyakiItem
        { id = "base-yasai"
        , name = "野菜入り"
        , price = 900
        , defaultNoodle = Nothing
        , category = Base
        }


{-| そば入りお好み焼き（defaultNoodle: noodleSoba、1200円）。

`price` にそば1玉分の料金が含まれている。

-}
baseSoba : MenuItem
baseSoba =
    OkonomiyakiItem
        { id = "base-soba"
        , name = "そば入り"
        , price = 1200
        , defaultNoodle = Just noodleSoba
        , category = Base
        }


{-| うどん入りお好み焼き（defaultNoodle: noodleUdon、1200円）。

`price` にうどん1玉分の料金が含まれている。

-}
baseUdon : MenuItem
baseUdon =
    OkonomiyakiItem
        { id = "base-udon"
        , name = "うどん入り"
        , price = 1200
        , defaultNoodle = Just noodleUdon
        , category = Base
        }


{-| 全ベース一覧。UI のメニュー表示や `baseForNoodle` の検索に使用する。 -}
okonomiyakiBases : List MenuItem
okonomiyakiBases =
    [ baseYasai, baseSoba, baseUdon ]


-- マスタデータ：麺


{-| そば（basePrice: 100円、pricePerHalfBall: 100円）。 -}
noodleSoba : MenuItem
noodleSoba =
    NoodleItem
        { id = "noodle-soba"
        , name = "そば"
        , basePrice = 100
        , pricePerHalfBall = 100
        , category = Noodle
        }


{-| うどん（basePrice: 100円、pricePerHalfBall: 100円）。 -}
noodleUdon : MenuItem
noodleUdon =
    NoodleItem
        { id = "noodle-udon"
        , name = "うどん"
        , basePrice = 100
        , pricePerHalfBall = 100
        , category = Noodle
        }


{-| 全麺一覧。UI の麺選択画面に使用する。 -}
allNoodles : List MenuItem
allNoodles =
    [ noodleSoba, noodleUdon ]


-- 表示・クエリ


{-| 麺の内部 quantity 値（半玉単位）を表示用文字列に変換する。

    noodleQuantityDisplay 1  == "0.5"
    noodleQuantityDisplay 2  == "1"
    noodleQuantityDisplay 3  == "1.5"
    noodleQuantityDisplay 4  == "2"

-}
noodleQuantityDisplay : Int -> String
noodleQuantityDisplay internalQuantity =
    let
        wholePart =
            internalQuantity // 2

        hasHalf =
            modBy 2 internalQuantity /= 0
    in
    if hasHalf then
        if wholePart == 0 then
            "0.5"

        else
            String.fromInt wholePart ++ ".5"

    else
        String.fromInt wholePart


{-| 麺 ID からその麺を `defaultNoodle` として持つベースを逆引きする。

`baseSoba` は `noodleSoba`、`baseUdon` は `noodleUdon` を持つ。
該当するベースが存在しない場合は `Nothing` を返す。

-}
baseForNoodle : String -> Maybe MenuItem
baseForNoodle noodleId =
    okonomiyakiBases
        |> List.filter
            (\base ->
                case base of
                    OkonomiyakiItem r ->
                        case r.defaultNoodle of
                            Just defaultNoodle ->
                                menuItemId defaultNoodle == noodleId

                            Nothing ->
                                False

                    _ ->
                        False
            )
        |> List.head


{-| `noodle` が `base` の `defaultNoodle` として内包されているか判定する。

    isDefaultNoodleOf noodleSoba baseSoba  == True
    isDefaultNoodleOf noodleUdon baseSoba  == False
    isDefaultNoodleOf noodleSoba baseYasai == False

-}
isDefaultNoodleOf : MenuItem -> MenuItem -> Bool
isDefaultNoodleOf noodle base =
    case base of
        OkonomiyakiItem r ->
            r.defaultNoodle
                |> Maybe.map (\dn -> menuItemId dn == menuItemId noodle)
                |> Maybe.withDefault False

        _ ->
            False


-- 構築・正規化・計算


{-| `OkonomiyakiItem` から初期 `BaseOrderItem` を生成する。

`defaultNoodle` を持つベースの場合、`noodles` に1玉（`quantity = 2`）をセットする。
`defaultNoodle` を持たない `baseYasai` の場合、`noodles` は空になる。

-}
initialBaseOrderItem : MenuItem -> BaseOrderItem
initialBaseOrderItem menuItem =
    let
        initialNoodles =
            case menuItem of
                OkonomiyakiItem r ->
                    case r.defaultNoodle of
                        Just noodleItem ->
                            [ { menuItem = noodleItem, quantity = 2 } ]

                        Nothing ->
                            []

                _ ->
                    []
    in
    { baseItem = menuItem
    , quantity = 1
    , noodles = initialNoodles
    , toppings = []
    }


{-| 麺の「＋」操作後に呼び、ベースと麺の整合性を保つ。

`baseYasai`（`defaultNoodle` なし）に麺を追加した場合、
追加した麺に対応するベース（`baseSoba` または `baseUdon`）へ切り替える。
すでに `defaultNoodle` を持つベースの場合は何もしない。

-}
normalizeBaseOnNoodleAdd : MenuItem -> BaseOrderItem -> BaseOrderItem
normalizeBaseOnNoodleAdd noodleItem baseItem =
    let
        newBase =
            case baseItem.baseItem of
                OkonomiyakiItem r ->
                    if r.defaultNoodle == Nothing then
                        baseForNoodle (menuItemId noodleItem)
                            |> Maybe.withDefault baseItem.baseItem

                    else
                        baseItem.baseItem

                _ ->
                    baseItem.baseItem
    in
    { baseItem | baseItem = newBase }


{-| 麺の「−」操作後に呼び、ベースと麺の整合性を保つ。

`defaultNoodle` を持つベース（`baseSoba`・`baseUdon`）で、
その `defaultNoodle` の合計 quantity が 0 になった場合、
ベースを `baseYasai` へ切り替える。
`defaultNoodle` を持たないベースには影響しない。

-}
normalizeBaseOnNoodleChange : BaseOrderItem -> BaseOrderItem
normalizeBaseOnNoodleChange baseItem =
    let
        newBase =
            case baseItem.baseItem of
                OkonomiyakiItem r ->
                    case r.defaultNoodle of
                        Just defaultNoodleItem ->
                            let
                                defaultNoodleQty =
                                    baseItem.noodles
                                        |> List.filter (\n -> menuItemId n.menuItem == menuItemId defaultNoodleItem)
                                        |> List.map .quantity
                                        |> List.sum
                            in
                            if defaultNoodleQty == 0 then
                                baseYasai

                            else
                                baseItem.baseItem

                        Nothing ->
                            baseItem.baseItem

                _ ->
                    baseItem.baseItem
    in
    { baseItem | baseItem = newBase }


{-| `BaseOrderItem` の小計を計算する。

計算式：`(ベース価格 + 麺追加料金 + トッピング料金) × 枚数`

**麺の料金ルール：**

  - `defaultNoodle` を持つベースは、その麺1玉分（`quantity = 2`）の料金が
    すでに `price` に含まれているため、超過分のみを加算する。
  - `defaultNoodle` を持たないベース（`baseYasai`）に麺を追加した場合は、
    `basePrice`（入場料）と `pricePerHalfBall × quantity` を加算する。

-}
calculateBaseItemTotal : BaseOrderItem -> Int
calculateBaseItemTotal baseItem =
    let
        basePrice =
            case baseItem.baseItem of
                StandardItem r ->
                    r.price

                OkonomiyakiItem r ->
                    r.price

                NoodleItem r ->
                    r.basePrice

        noodlePrice =
            let
                -- defaultNoodle の1玉分（qty=2）はベース price に含まれるため除外
                defaultNoodleId =
                    case baseItem.baseItem of
                        OkonomiyakiItem r ->
                            Maybe.map menuItemId r.defaultNoodle

                        _ ->
                            Nothing

                extraNoodleQuantity =
                    baseItem.noodles
                        |> List.map
                            (\n ->
                                case defaultNoodleId of
                                    Just dnId ->
                                        if menuItemId n.menuItem == dnId then
                                            max 0 (n.quantity - 2)

                                        else
                                            n.quantity

                                    Nothing ->
                                        n.quantity
                            )
                        |> List.sum
            in
            if extraNoodleQuantity == 0 then
                0

            else if defaultNoodleId /= Nothing then
                -- defaultNoodle の基本料金はベース価格に含まれるため、追加半玉分のみ
                100 * extraNoodleQuantity

            else
                -- defaultNoodle なし：basePrice（入場料）+ 半玉単価 × quantity
                100 + 100 * extraNoodleQuantity

        toppingsPrice =
            baseItem.toppings
                |> List.map (\t -> menuItemPrice t.menuItem t.quantity)
                |> List.sum
    in
    (basePrice + noodlePrice + toppingsPrice) * baseItem.quantity
