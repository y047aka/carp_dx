module Okonomiyaki exposing
    ( Noodle
    , NoodleAddition
    , NoodleKind(..)
    , Okonomiyaki
    , OkonomiyakiBase
    , OkonomiyakiBaseKind(..)
    , Topping
    , ToppingAddition
    , ToppingKind(..)
    , addNoodle
    , addTopping
    , allNoodles
    , allToppings
    , baseYasaiBase
    , baseSobaBase
    , baseUdonBase
    , baseZenbuIriBase
    , calculateTotal
    , decrementNoodle
    , decrementQuantity
    , incrementQuantity
    , init
    , isDefaultNoodleOf
    , kindToBase
    , noodleQuantityDisplay
    , noodleSoba
    , noodleUdon
    , normalizeBase
    , removeTopping
    , toggleTopping
    , toppingCheese
    , toppingGarlic
    , toppingIkaten
    , toppingMochi
    , toppingNegi
    , toppingShrimp
    , toppingSquid
    )

{-| お好み焼きドメインのモジュール。

お好み焼きのベース・麺・トッピングに関する型定義、マスタデータ、および
`Okonomiyaki` の構築・計算・正規化ロジックを提供する。

## 設計方針

  - **責務分離**: このモジュールは `MenuItem` に依存しない。
    `MenuItem` → `OkonomiyakiBase` の変換は `MenuData` モジュールが担う。
  - **麺の独立**: 麺は `Noodle` 型で表現する。
    麺の識別・価格計算・注文操作はすべてこのモジュール内で完結する。
  - **ベース** (`OkonomiyakiBase`) は `includedNoodleKind` を持つ場合があり、
    その麺1玉分の価格はベースの `basePrice` に含まれている。
  - **麺の数量** は内部的に「半玉単位」で管理する（quantity 2 = 1玉）。
    表示変換には `noodleQuantityDisplay` を使用する。
  - `normalizeBase` は麺・トッピングの状態からベースを一意に決定する。
    麺・トッピングの操作後にこの関数を1つ呼ぶだけで整合性が保たれる。

-}


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


{-| 麺のドメイン情報。お好み焼きドメイン専用の型。

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


{-| お好み焼き1枚の完全な構成。

`base` にベースのドメイン情報（`OkonomiyakiBase`）を持ち、
`noodles` と `toppings` にそれぞれの追加オプションを格納する。
`quantity` はこのお好み焼き自体の枚数を表す。

-}
type alias Okonomiyaki =
    { base : OkonomiyakiBase
    , quantity : Int
    , noodles : List NoodleAddition
    , toppings : List ToppingAddition
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


{-| `OkonomiyakiBaseKind` から `OkonomiyakiBase` を取得する。 -}
kindToBase : OkonomiyakiBaseKind -> OkonomiyakiBase
kindToBase kind =
    case kind of
        Yasai ->
            baseYasaiBase

        Soba ->
            baseSobaBase

        Udon ->
            baseUdonBase

        ZenbuIri ->
            baseZenbuIriBase


-- 構築・正規化・計算


{-| `OkonomiyakiBase` から初期 `Okonomiyaki` を生成する。

`includedNoodleKind` を持つベースの場合、`noodles` に1玉（`quantity = 2`）をセットする。
`includedNoodleKind` を持たない `Yasai` の場合、`noodles` は空になる。
`ZenbuIri` の場合、`noodles` にそば1玉をセットし、`toppings` にイカ・エビをセットする。

-}
init : OkonomiyakiBase -> Okonomiyaki
init base =
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


{-| 麺・トッピングの状態からベースを一意に決定し、整合性を保つ。

ベース決定ルール（優先順位順）:

  1. イカとエビが両方 toppings にあれば → ZenbuIri
  2. そばが noodles にあれば → Soba
  3. うどんが noodles にあれば → Udon
  4. いずれでもなければ → Yasai

麺・トッピングのいずれの操作後にも、この関数を1つ呼ぶだけでベースの整合性が保たれる。

-}
normalizeBase : Okonomiyaki -> Okonomiyaki
normalizeBase baseItem =
    let
        hasSquid =
            List.any (\t -> t.topping.kind == ToppingKindSquid) baseItem.toppings

        hasShrimp =
            List.any (\t -> t.topping.kind == ToppingKindShrimp) baseItem.toppings

        hasNoodleKind kind =
            List.any (\n -> n.noodle.kind == kind) baseItem.noodles

        newBase =
            if hasSquid && hasShrimp then
                baseZenbuIriBase

            else if hasNoodleKind NoodleKindSoba then
                baseSobaBase

            else if hasNoodleKind NoodleKindUdon then
                baseUdonBase

            else
                baseYasaiBase
    in
    { baseItem | base = newBase }


{-| お好み焼きの小計を計算する。

計算式：`(ベース価格 + 麺追加料金 + トッピング料金) × 枚数`

**麺の料金ルール：**

  - `includedNoodleKind` を持つベースは、その麺1玉分（`quantity = 2`）の料金が
    すでに `basePrice` に含まれているため、超過分のみを加算する。
  - `includedNoodleKind` を持たないベース（`Yasai`・`ZenbuIri`）に麺を追加した場合は、
    入場料（100円）と半玉単価（100円）× quantity を加算する。

-}
calculateTotal : Okonomiyaki -> Int
calculateTotal baseItem =
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


-- 操作


{-| 麺を追加する。既存の同種麺があれば半玉（quantity+1）追加、なければ1玉（quantity=2）で新規追加する。 -}
addNoodle : Noodle -> Okonomiyaki -> Okonomiyaki
addNoodle noodle item =
    let
        hasExisting =
            List.any (\n -> n.noodle.kind == noodle.kind) item.noodles
    in
    if hasExisting then
        { item
            | noodles =
                List.map
                    (\n ->
                        if n.noodle.kind == noodle.kind then
                            { n | quantity = n.quantity + 1 }

                        else
                            n
                    )
                    item.noodles
        }

    else
        { item | noodles = item.noodles ++ [ { noodle = noodle, quantity = 2 } ] }


{-| 麺を半玉（quantity-1）減らす。0以下になったら削除する。 -}
decrementNoodle : Noodle -> Okonomiyaki -> Okonomiyaki
decrementNoodle noodle item =
    { item
        | noodles =
            List.map
                (\n ->
                    if n.noodle.kind == noodle.kind then
                        { n | quantity = max 0 (n.quantity - 1) }

                    else
                        n
                )
                item.noodles
                |> List.filter (\n -> n.quantity > 0)
    }


{-| トッピングを追加する。既存の同種があれば quantity+1、なければ quantity=1 で新規追加する。 -}
addTopping : Topping -> Okonomiyaki -> Okonomiyaki
addTopping toppingItem item =
    let
        hasExisting =
            List.any (\t -> t.topping.kind == toppingItem.kind) item.toppings
    in
    if hasExisting then
        { item
            | toppings =
                List.map
                    (\t ->
                        if t.topping.kind == toppingItem.kind then
                            { t | quantity = t.quantity + 1 }

                        else
                            t
                    )
                    item.toppings
        }

    else
        { item | toppings = item.toppings ++ [ { topping = toppingItem, quantity = 1 } ] }


{-| 指定種類のトッピングを削除する。 -}
removeTopping : ToppingKind -> Okonomiyaki -> Okonomiyaki
removeTopping toppingKind item =
    { item | toppings = List.filter (\t -> t.topping.kind /= toppingKind) item.toppings }


{-| トッピングをトグルする。存在すれば削除、なければ追加する。 -}
toggleTopping : Topping -> Okonomiyaki -> Okonomiyaki
toggleTopping toppingItem item =
    if List.any (\t -> t.topping.kind == toppingItem.kind) item.toppings then
        removeTopping toppingItem.kind item

    else
        addTopping toppingItem item


{-| 枚数を1増やす。 -}
incrementQuantity : Okonomiyaki -> Okonomiyaki
incrementQuantity item =
    { item | quantity = item.quantity + 1 }


{-| 枚数を1減らす。0以下になる場合は Nothing を返す。 -}
decrementQuantity : Okonomiyaki -> Maybe Okonomiyaki
decrementQuantity item =
    let
        newQuantity =
            item.quantity - 1
    in
    if newQuantity <= 0 then
        Nothing

    else
        Just { item | quantity = newQuantity }
