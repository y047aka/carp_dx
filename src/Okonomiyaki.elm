module Okonomiyaki exposing
    ( Addition
    , BaseOrderItem
    , baseYasai
    , baseSoba
    , baseUdon
    , baseZenbuIri
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
    , normalizeBaseOnToppingChange
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
        , defaultToppings = []
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
        , defaultToppings = []
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
        , defaultToppings = []
        , category = Base
        }


{-| 全部入りお好み焼き（defaultNoodle: なし、defaultToppings: イカ・エビ、ベース600円）。

麺の種類を選ばず、イカ・エビのトッピング込みで麺1玉時に1700円になる。
価格計算: basePrice(600) + noodlePrice(100 + 100×qty) + toppingsPrice(800)
例: そばまたはうどん1玉(qty=2): 600 + (100 + 100×2) + 800 = 1700円

-}
baseZenbuIri : MenuItem
baseZenbuIri =
    OkonomiyakiItem
        { id = "base-zenbu-iri"
        , name = "全部入り"
        , price = 600
        , defaultNoodle = Nothing
        , defaultToppings =
            [ StandardItem { id = "topping-squid", name = "イカ", price = 400, category = Topping }
            , StandardItem { id = "topping-shrimp", name = "エビ", price = 400, category = Topping }
            ]
        , category = Base
        }


{-| 全ベース一覧。UI のメニュー表示や `baseForNoodle` の検索に使用する。 -}
okonomiyakiBases : List MenuItem
okonomiyakiBases =
    [ baseYasai, baseSoba, baseUdon, baseZenbuIri ]


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
`defaultToppings` を持つベース（`baseZenbuIri` など）の場合、`toppings` に各トッピングを quantity=1 でセットする。

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
                            if menuItemId menuItem == "base-zenbu-iri" then
                                [ { menuItem = noodleSoba, quantity = 2 } ]

                            else
                                []

                _ ->
                    []

        initialToppings =
            case menuItem of
                OkonomiyakiItem r ->
                    List.map (\t -> { menuItem = t, quantity = 1 }) r.defaultToppings

                _ ->
                    []
    in
    { baseItem = menuItem
    , quantity = 1
    , noodles = initialNoodles
    , toppings = initialToppings
    }


{-| 麺の「＋」操作後に呼び、ベースと麺の整合性を保つ。

`baseYasai`（`defaultNoodle` なし、かつ全部入りでない）に麺を追加した場合、
追加した麺に対応するベース（`baseSoba` または `baseUdon`）へ切り替える。
`baseZenbuIri` は `defaultNoodle` を持たないが、トッピングによって決まるベースであるため
麺の追加によるベース切替の対象外とする。
すでに `defaultNoodle` を持つベースの場合は何もしない。

-}
normalizeBaseOnNoodleAdd : MenuItem -> BaseOrderItem -> BaseOrderItem
normalizeBaseOnNoodleAdd noodleItem baseItem =
    let
        newBase =
            case baseItem.baseItem of
                OkonomiyakiItem r ->
                    if r.defaultNoodle == Nothing && menuItemId baseItem.baseItem /= "base-zenbu-iri" then
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

`baseZenbuIri`（`defaultNoodle` なし）は麺量に関わらず切り替えない。
全部入りはイカ・エビが両方ある限り成立し、麺なし全部入り（1400円）も有効な状態とする。
全部入りのベース切替はトッピング操作（`normalizeBaseOnToppingChange`）が担う。

それ以外の `defaultNoodle` を持たないベースには影響しない。

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


{-| トッピングの追加・削除操作後に呼び、ベースと全部入り条件の整合性を保つ。

イカ（`topping-squid`）とエビ（`topping-shrimp`）が両方 `toppings` に存在する場合、
`baseSoba`・`baseUdon`・`baseYasai` を `baseZenbuIri` へ切り替える。
このとき `noodles` はそのまま引き継がれる。

どちらかが欠けている場合、`baseZenbuIri` だったベースを麺の状態に応じて切り替える：
  - `noodle-soba` が残っていれば `baseSoba`
  - `noodle-udon` が残っていれば `baseUdon`
  - 麺なしなら `baseYasai`

-}
normalizeBaseOnToppingChange : BaseOrderItem -> BaseOrderItem
normalizeBaseOnToppingChange baseItem =
    let
        hasSquid =
            List.any (\t -> menuItemId t.menuItem == "topping-squid") baseItem.toppings

        hasShrimp =
            List.any (\t -> menuItemId t.menuItem == "topping-shrimp") baseItem.toppings

        hasNoodleSoba =
            List.any (\n -> menuItemId n.menuItem == "noodle-soba") baseItem.noodles

        hasNoodleUdon =
            List.any (\n -> menuItemId n.menuItem == "noodle-udon") baseItem.noodles

        currentBaseId =
            menuItemId baseItem.baseItem

        newBase =
            if hasSquid && hasShrimp then
                case currentBaseId of
                    "base-soba" ->
                        baseZenbuIri

                    "base-udon" ->
                        baseZenbuIri

                    "base-yasai" ->
                        baseZenbuIri

                    _ ->
                        baseItem.baseItem

            else if currentBaseId == "base-zenbu-iri" then
                if hasNoodleSoba then
                    baseSoba

                else if hasNoodleUdon then
                    baseUdon

                else
                    baseYasai

            else
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
                                            -- 1玉(qty=2)分はベース price に含まれるため差分を計算
                                            -- 0.5玉(qty=1)なら -1 → 100円の割引になる
                                            n.quantity - 2

                                        else
                                            n.quantity

                                    Nothing ->
                                        n.quantity
                            )
                        |> List.sum
            in
            case defaultNoodleId of
                Nothing ->
                    if extraNoodleQuantity == 0 then
                        0

                    else
                        -- defaultNoodle なし：basePrice（入場料）+ 半玉単価 × quantity
                        100 + 100 * extraNoodleQuantity

                Just _ ->
                    -- defaultNoodle の基本料金はベース価格に含まれるため、差分のみ加算（負なら割引）
                    100 * extraNoodleQuantity

        toppingsPrice =
            baseItem.toppings
                |> List.map (\t -> menuItemPrice t.menuItem t.quantity)
                |> List.sum
    in
    (basePrice + noodlePrice + toppingsPrice) * baseItem.quantity
