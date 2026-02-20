module Okonomiyaki exposing
    ( Addition
    , BaseOrderItem
    , NoodleKind(..)
    , OkonomiyakiBase
    , OkonomiyakiBaseKind(..)
    , baseYasai
    , baseSoba
    , baseUdon
    , baseZenbuIri
    , baseYasaiBase
    , baseSobaBase
    , baseUdonBase
    , baseZenbuIriBase
    , noodleSoba
    , noodleUdon
    , allNoodles
    , noodleQuantityDisplay
    , isDefaultNoodleOf
    , noodleKindOf
    , menuItemToOkonomiyakiBase
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

  - **責務分離**: `MenuItem`（`OkonomiyakiItem`）は UI のメニュー選択専用。
    注文状態の表現には `OkonomiyakiBase` を使用する。
  - **ベース** (`OkonomiyakiBase`) は `includedNoodleKind` を持つ場合があり、
    その麺1玉分の価格はベースの `basePrice` に含まれている。
  - **麺の数量** は内部的に「半玉単位」で管理する（quantity 2 = 1玉）。
    表示変換には `noodleQuantityDisplay` を使用する。
  - ベースと麺の整合性を保つ正規化関数は、麺の増減操作の直後に呼び出す。
  - **境界変換**: `MenuItem` → `OkonomiyakiBase` への変換は
    `menuItemToOkonomiyakiBase` の1箇所に局所化している。

-}

import Menu exposing (MenuCategory(..), MenuItem(..), menuItemId, menuItemPrice)


-- 型定義


{-| 麺の種類。`OkonomiyakiBase.includedNoodleKind` での比較に使用する。 -}
type NoodleKind
    = NoodleKindSoba
    | NoodleKindUdon


{-| お好み焼きベースの種類。IDハードコードの代替として使用する。 -}
type OkonomiyakiBaseKind
    = Yasai
    | Soba
    | Udon
    | ZenbuIri


{-| お好み焼きベースのドメイン情報。注文状態として使用する。

`MenuItem` の `OkonomiyakiItem` は UI のメニュー選択専用とし、
注文状態（`BaseOrderItem`）はこの型で管理する。

  - `kind` はベースの種類を表す（IDハードコードの代替）
  - `basePrice` は麺・トッピングを含まない純粋なベース料金
  - `includedNoodleKind` はベース価格に込みの麺の種類（あれば）

-}
type alias OkonomiyakiBase =
    { kind : OkonomiyakiBaseKind
    , name : String
    , basePrice : Int
    , includedNoodleKind : Maybe NoodleKind
    }


{-| お好み焼きに付属する追加オプション（麺・トッピング）。

`quantity` の単位はオプションの種類に依存する。
麺は半玉単位（`2` = 1玉）、トッピングは個数単位。

-}
type alias Addition =
    { menuItem : MenuItem
    , quantity : Int
    }


{-| お好み焼き1件分の注文アイテム。

`base` にベースのドメイン情報（`OkonomiyakiBase`）を持ち、
`noodles` と `toppings` にそれぞれの追加オプションを格納する。
`quantity` はこのお好み焼き自体の枚数を表す。

-}
type alias BaseOrderItem =
    { base : OkonomiyakiBase
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


-- マスタデータ：OkonomiyakiBase


{-| 野菜入りのベースドメイン情報。 -}
baseYasaiBase : OkonomiyakiBase
baseYasaiBase =
    { kind = Yasai
    , name = "野菜入り"
    , basePrice = 900
    , includedNoodleKind = Nothing
    }


{-| そば入りのベースドメイン情報（そば1玉分込み）。 -}
baseSobaBase : OkonomiyakiBase
baseSobaBase =
    { kind = Soba
    , name = "そば入り"
    , basePrice = 1200
    , includedNoodleKind = Just NoodleKindSoba
    }


{-| うどん入りのベースドメイン情報（うどん1玉分込み）。 -}
baseUdonBase : OkonomiyakiBase
baseUdonBase =
    { kind = Udon
    , name = "うどん入り"
    , basePrice = 1200
    , includedNoodleKind = Just NoodleKindUdon
    }


{-| 全部入りのベースドメイン情報（イカ・エビ込み、麺は別途追加）。 -}
baseZenbuIriBase : OkonomiyakiBase
baseZenbuIriBase =
    { kind = ZenbuIri
    , name = "全部入り"
    , basePrice = 600
    , includedNoodleKind = Nothing
    }


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


{-| `noodle` が `base` の込み麺として内包されているか判定する。

    isDefaultNoodleOf noodleSoba baseSobaBase  == True
    isDefaultNoodleOf noodleUdon baseSobaBase  == False
    isDefaultNoodleOf noodleSoba baseYasaiBase == False

-}
isDefaultNoodleOf : MenuItem -> OkonomiyakiBase -> Bool
isDefaultNoodleOf noodle base =
    case noodleKindOf noodle of
        Just kind ->
            base.includedNoodleKind == Just kind

        Nothing ->
            False


{-| `NoodleItem` から `NoodleKind` を取得する。

`OkonomiyakiBase.includedNoodleKind` との比較に使用する。
`NoodleItem` 以外は `Nothing` を返す。

-}
noodleKindOf : MenuItem -> Maybe NoodleKind
noodleKindOf menuItem =
    case menuItem of
        NoodleItem r ->
            case r.id of
                "noodle-soba" ->
                    Just NoodleKindSoba

                "noodle-udon" ->
                    Just NoodleKindUdon

                _ ->
                    Nothing

        _ ->
            Nothing


{-| `MenuItem`（`OkonomiyakiItem`）から `OkonomiyakiBase` へ変換する。

`MenuItem` 世界と `OkonomiyakiBase` 世界の境界変換関数。
ID 文字列参照をこの関数1箇所に局所化する。
`OkonomiyakiItem` 以外は `Nothing` を返す。

-}
menuItemToOkonomiyakiBase : MenuItem -> Maybe OkonomiyakiBase
menuItemToOkonomiyakiBase menuItem =
    case menuItem of
        OkonomiyakiItem r ->
            case r.id of
                "base-yasai" ->
                    Just baseYasaiBase

                "base-soba" ->
                    Just baseSobaBase

                "base-udon" ->
                    Just baseUdonBase

                "base-zenbu-iri" ->
                    Just baseZenbuIriBase

                _ ->
                    Nothing

        _ ->
            Nothing


-- 構築・正規化・計算


-- 全部入りの初期トッピング（循環依存を避けるため Okonomiyaki.elm 内でローカル定義）
zenbuIriSquid : MenuItem
zenbuIriSquid =
    StandardItem { id = "topping-squid", name = "イカ", price = 400, category = Topping }


zenbuIriShrimp : MenuItem
zenbuIriShrimp =
    StandardItem { id = "topping-shrimp", name = "エビ", price = 400, category = Topping }


{-| `OkonomiyakiBase` から初期 `BaseOrderItem` を生成する。

`includedNoodleKind` を持つベースの場合、`noodles` に1玉（`quantity = 2`）をセットする。
`includedNoodleKind` を持たない `Yasai` の場合、`noodles` は空になる。
`ZenbuIri` の場合、`noodles` にそば1玉をセットし、`toppings` にイカ・エビをセットする。

-}
initialBaseOrderItem : OkonomiyakiBase -> BaseOrderItem
initialBaseOrderItem base =
    let
        initialNoodles =
            case base.includedNoodleKind of
                Just NoodleKindSoba ->
                    [ { menuItem = noodleSoba, quantity = 2 } ]

                Just NoodleKindUdon ->
                    [ { menuItem = noodleUdon, quantity = 2 } ]

                Nothing ->
                    case base.kind of
                        ZenbuIri ->
                            [ { menuItem = noodleSoba, quantity = 2 } ]

                        _ ->
                            []

        initialToppings =
            case base.kind of
                ZenbuIri ->
                    [ { menuItem = zenbuIriSquid, quantity = 1 }
                    , { menuItem = zenbuIriShrimp, quantity = 1 }
                    ]

                _ ->
                    []
    in
    { base = base
    , quantity = 1
    , noodles = initialNoodles
    , toppings = initialToppings
    }


{-| 麺の「＋」操作後に呼び、ベースと麺の整合性を保つ。

`Yasai`（`includedNoodleKind` なし）に麺を追加した場合、
追加した麺に対応するベース（`Soba` または `Udon`）へ切り替える。
`ZenbuIri` はトッピングによって決まるベースであるため麺の追加によるベース切替の対象外とする。
すでに `includedNoodleKind` を持つベースの場合は何もしない。

-}
normalizeBaseOnNoodleAdd : MenuItem -> BaseOrderItem -> BaseOrderItem
normalizeBaseOnNoodleAdd noodleItem baseItem =
    let
        newBase =
            case baseItem.base.kind of
                Yasai ->
                    case noodleKindOf noodleItem of
                        Just NoodleKindSoba ->
                            baseSobaBase

                        Just NoodleKindUdon ->
                            baseUdonBase

                        Nothing ->
                            baseItem.base

                _ ->
                    baseItem.base
    in
    { baseItem | base = newBase }


{-| 麺の「−」操作後に呼び、ベースと麺の整合性を保つ。

`includedNoodleKind` を持つベース（`Soba`・`Udon`）で、
その込み麺の合計 quantity が 0 になった場合、ベースを `Yasai` へ切り替える。

`ZenbuIri`（`includedNoodleKind` なし）は麺量に関わらず切り替えない。
全部入りはイカ・エビが両方ある限り成立し、麺なし全部入り（1400円）も有効な状態とする。
全部入りのベース切替はトッピング操作（`normalizeBaseOnToppingChange`）が担う。

それ以外の `includedNoodleKind` を持たないベースには影響しない。

-}
normalizeBaseOnNoodleChange : BaseOrderItem -> BaseOrderItem
normalizeBaseOnNoodleChange baseItem =
    let
        newBase =
            case baseItem.base.includedNoodleKind of
                Just includedKind ->
                    let
                        includedNoodleQty =
                            baseItem.noodles
                                |> List.filter (\n -> noodleKindOf n.menuItem == Just includedKind)
                                |> List.map .quantity
                                |> List.sum
                    in
                    if includedNoodleQty == 0 then
                        baseYasaiBase

                    else
                        baseItem.base

                Nothing ->
                    baseItem.base
    in
    { baseItem | base = newBase }


{-| トッピングの追加・削除操作後に呼び、ベースと全部入り条件の整合性を保つ。

イカ（`topping-squid`）とエビ（`topping-shrimp`）が両方 `toppings` に存在する場合、
`Soba`・`Udon`・`Yasai` を `ZenbuIri` へ切り替える。
このとき `noodles` はそのまま引き継がれる。

どちらかが欠けている場合、`ZenbuIri` だったベースを麺の状態に応じて切り替える：
  - そばが残っていれば `Soba`
  - うどんが残っていれば `Udon`
  - 麺なしなら `Yasai`

-}
normalizeBaseOnToppingChange : BaseOrderItem -> BaseOrderItem
normalizeBaseOnToppingChange baseItem =
    let
        hasSquid =
            List.any (\t -> menuItemId t.menuItem == "topping-squid") baseItem.toppings

        hasShrimp =
            List.any (\t -> menuItemId t.menuItem == "topping-shrimp") baseItem.toppings

        hasNoodleKind kind =
            List.any (\n -> noodleKindOf n.menuItem == Just kind) baseItem.noodles

        newBase =
            if hasSquid && hasShrimp then
                case baseItem.base.kind of
                    ZenbuIri ->
                        baseItem.base

                    _ ->
                        baseZenbuIriBase

            else
                case baseItem.base.kind of
                    ZenbuIri ->
                        if hasNoodleKind NoodleKindSoba then
                            baseSobaBase

                        else if hasNoodleKind NoodleKindUdon then
                            baseUdonBase

                        else
                            baseYasaiBase

                    _ ->
                        baseItem.base
    in
    { baseItem | base = newBase }


{-| `BaseOrderItem` の小計を計算する。

計算式：`(ベース価格 + 麺追加料金 + トッピング料金) × 枚数`

**麺の料金ルール：**

  - `includedNoodleKind` を持つベースは、その麺1玉分（`quantity = 2`）の料金が
    すでに `basePrice` に含まれているため、超過分のみを加算する。
  - `includedNoodleKind` を持たないベース（`Yasai`・`ZenbuIri`）に麺を追加した場合は、
    入場料（100円）と半玉単価（100円）× quantity を加算する。

-}
calculateBaseItemTotal : BaseOrderItem -> Int
calculateBaseItemTotal baseItem =
    let
        basePrice =
            baseItem.base.basePrice

        noodlePrice =
            let
                includedNoodleKind =
                    baseItem.base.includedNoodleKind

                extraNoodleQuantity =
                    baseItem.noodles
                        |> List.map
                            (\n ->
                                case includedNoodleKind of
                                    Just includedKind ->
                                        if noodleKindOf n.menuItem == Just includedKind then
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
            case includedNoodleKind of
                Nothing ->
                    if extraNoodleQuantity == 0 then
                        0

                    else
                        -- includedNoodle なし：入場料（100円）+ 半玉単価 × quantity
                        100 + 100 * extraNoodleQuantity

                Just _ ->
                    -- includedNoodle の基本料金はベース価格に含まれるため、差分のみ加算（負なら割引）
                    100 * extraNoodleQuantity

        toppingsPrice =
            baseItem.toppings
                |> List.map (\t -> menuItemPrice t.menuItem t.quantity)
                |> List.sum
    in
    (basePrice + noodlePrice + toppingsPrice) * baseItem.quantity
