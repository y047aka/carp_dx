module Okonomiyaki exposing
    ( BaseEntry
    , BaseOrderItem
    , Noodle
    , NoodleAddition
    , NoodleKind(..)
    , OkonomiyakiBase
    , OkonomiyakiBaseKind(..)
    , Topping
    , ToppingAddition
    , ToppingKind(..)
    , allBases
    , allNoodles
    , allToppings
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
    , toppingIkaten
    , toppingMochi
    , toppingNegi
    , toppingGarlic
    , toppingCheese
    , toppingSquid
    , toppingShrimp
    , noodleQuantityDisplay
    , isDefaultNoodleOf
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

  - **責務分離**: `MenuItem` は UI のメニュー選択専用。
    注文状態の表現には `OkonomiyakiBase` を使用する。
  - **麺の独立**: 麺は `MenuItem` ではなく `Noodle` 型で表現する。
    麺の識別・価格計算・注文操作はすべてこのモジュール内で完結する。
  - **ベース** (`OkonomiyakiBase`) は `includedNoodleKind` を持つ場合があり、
    その麺1玉分の価格はベースの `basePrice` に含まれている。
  - **麺の数量** は内部的に「半玉単位」で管理する（quantity 2 = 1玉）。
    表示変換には `noodleQuantityDisplay` を使用する。
  - ベースと麺の整合性を保つ正規化関数は、麺の増減操作の直後に呼び出す。
  - **境界変換**: `MenuItem` → `OkonomiyakiBase` への変換は
    `menuItemToOkonomiyakiBase` の1箇所に局所化している。

-}

import Menu exposing (MenuCategory(..), MenuItem)


-- 型定義


{-| 麺の種類。`OkonomiyakiBase.includedNoodleKind` での比較に使用する。 -}
type NoodleKind
    = NoodleKindSoba
    | NoodleKindUdon


{-| トッピングの種類。トッピングの同一性識別に使用する。 -}
type ToppingKind
    = ToppingKindIkaten
    | ToppingKindMochi
    | ToppingKindNegi
    | ToppingKindGarlic
    | ToppingKindCheese
    | ToppingKindSquid
    | ToppingKindShrimp


{-| お好み焼きベースの種類。IDハードコードの代替として使用する。 -}
type OkonomiyakiBaseKind
    = Yasai
    | Soba
    | Udon
    | ZenbuIri


{-| お好み焼きベースのドメイン情報。注文状態として使用する。

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


{-| お好み焼きに追加できるトッピングのドメイン情報。

  - `kind` はトッピングの種類（同一性の識別に使用）
  - `name` は表示名
  - `price` は1個あたりの料金

-}
type alias Topping =
    { kind : ToppingKind
    , name : String
    , price : Int
    }


{-| `MenuItem` と `OkonomiyakiBase` のペア。`allBases` リストの要素。 -}
type alias BaseEntry =
    { menuItem : MenuItem
    , base : OkonomiyakiBase
    }


{-| 麺のドメイン情報。`MenuItem` から独立したお好み焼きドメイン専用の型。

  - `kind` は麺の種類（同一性の識別に使用）
  - `basePrice` は麺の入場料（最初の1玉に加算される固定料金）
  - `pricePerHalfBall` は0.5玉あたりの追加料金

-}
type alias Noodle =
    { kind : NoodleKind
    , name : String
    , basePrice : Int
    , pricePerHalfBall : Int
    }


{-| お好み焼きに付属するトッピングの追加オプション。

`quantity` はトッピングの個数単位。

-}
type alias ToppingAddition =
    { topping : Topping
    , quantity : Int
    }


{-| お好み焼きに付属する麺の追加オプション。

`quantity` は半玉単位（`2` = 1玉）。

-}
type alias NoodleAddition =
    { noodle : Noodle
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
    , noodles : List NoodleAddition
    , toppings : List ToppingAddition
    }


-- マスタデータ：ベース


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


{-| 全ベースエントリの一覧。`MenuData.allMenuItems` のベース部分と `menuItemToOkonomiyakiBase` の両方に使用する。 -}
allBases : List BaseEntry
allBases =
    [ { menuItem = baseYasai,    base = baseYasaiBase    }
    , { menuItem = baseSoba,     base = baseSobaBase     }
    , { menuItem = baseUdon,     base = baseUdonBase     }
    , { menuItem = baseZenbuIri, base = baseZenbuIriBase }
    ]


-- マスタデータ：トッピング


toppingIkaten : Topping
toppingIkaten =
    { kind = ToppingKindIkaten, name = "イカ天", price = 200 }


toppingMochi : Topping
toppingMochi =
    { kind = ToppingKindMochi, name = "もち", price = 200 }


toppingNegi : Topping
toppingNegi =
    { kind = ToppingKindNegi, name = "ねぎかけ", price = 250 }


toppingGarlic : Topping
toppingGarlic =
    { kind = ToppingKindGarlic, name = "ニンニク", price = 250 }


toppingCheese : Topping
toppingCheese =
    { kind = ToppingKindCheese, name = "チーズ", price = 300 }


toppingSquid : Topping
toppingSquid =
    { kind = ToppingKindSquid, name = "イカ", price = 400 }


toppingShrimp : Topping
toppingShrimp =
    { kind = ToppingKindShrimp, name = "エビ", price = 400 }


{-| 全トッピング一覧。UI のトッピング選択画面に使用する。 -}
allToppings : List Topping
allToppings =
    [ toppingIkaten
    , toppingMochi
    , toppingNegi
    , toppingGarlic
    , toppingCheese
    , toppingSquid
    , toppingShrimp
    ]


-- マスタデータ：麺


{-| そば（basePrice: 100円、pricePerHalfBall: 100円）。 -}
noodleSoba : Noodle
noodleSoba =
    { kind = NoodleKindSoba
    , name = "そば"
    , basePrice = 100
    , pricePerHalfBall = 100
    }


{-| うどん（basePrice: 100円、pricePerHalfBall: 100円）。 -}
noodleUdon : Noodle
noodleUdon =
    { kind = NoodleKindUdon
    , name = "うどん"
    , basePrice = 100
    , pricePerHalfBall = 100
    }


{-| 全麺一覧。UI の麺選択画面に使用する。 -}
allNoodles : List Noodle
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
isDefaultNoodleOf : Noodle -> OkonomiyakiBase -> Bool
isDefaultNoodleOf noodle base =
    base.includedNoodleKind == Just noodle.kind


{-| `MenuItem` から `OkonomiyakiBase` へ変換する。

`allBases` リストを検索し、`id` が一致するエントリの `base` を返す。
`allBases` に存在しない `MenuItem` は `Nothing` を返す。

-}
menuItemToOkonomiyakiBase : MenuItem -> Maybe OkonomiyakiBase
menuItemToOkonomiyakiBase menuItem =
    allBases
        |> List.filter (\e -> e.menuItem.id == menuItem.id)
        |> List.head
        |> Maybe.map .base


-- 構築・正規化・計算


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
                    [ { noodle = noodleSoba, quantity = 2 } ]

                Just NoodleKindUdon ->
                    [ { noodle = noodleUdon, quantity = 2 } ]

                Nothing ->
                    case base.kind of
                        ZenbuIri ->
                            [ { noodle = noodleSoba, quantity = 2 } ]

                        _ ->
                            []

        initialToppings =
            case base.kind of
                ZenbuIri ->
                    [ { topping = toppingSquid, quantity = 1 }
                    , { topping = toppingShrimp, quantity = 1 }
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
normalizeBaseOnNoodleAdd : Noodle -> BaseOrderItem -> BaseOrderItem
normalizeBaseOnNoodleAdd noodle baseItem =
    let
        newBase =
            case baseItem.base.kind of
                Yasai ->
                    case noodle.kind of
                        NoodleKindSoba ->
                            baseSobaBase

                        NoodleKindUdon ->
                            baseUdonBase

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
                                |> List.filter (\n -> n.noodle.kind == includedKind)
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
            List.any (\t -> t.topping.kind == ToppingKindSquid) baseItem.toppings

        hasShrimp =
            List.any (\t -> t.topping.kind == ToppingKindShrimp) baseItem.toppings

        hasNoodleKind kind =
            List.any (\n -> n.noodle.kind == kind) baseItem.noodles

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
                                        if n.noodle.kind == includedKind then
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
                |> List.map (\t -> t.topping.price * t.quantity)
                |> List.sum
    in
    (basePrice + noodlePrice + toppingsPrice) * baseItem.quantity
