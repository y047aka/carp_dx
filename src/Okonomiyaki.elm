module Okonomiyaki exposing
    ( BaseKind(..)
    , Noodle
    , NoodleAddition
    , NoodleKind(..)
    , Okonomiyaki
    , Topping
    , ToppingAddition
    , ToppingKind(..)
    , addNoodle
    , addTopping
    , allNoodles
    , allToppings
    , baseName
    , calculateTotal
    , decrementNoodle
    , init
    , isDefaultNoodleOf
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
    `MenuItem` ↔ `BaseKind` の変換は `MenuData` モジュールが担う。
  - **麺の独立**: 麺は `Noodle` 型で表現する。
    麺の識別・価格計算・注文操作はすべてこのモジュール内で完結する。
  - **ベース**: 各ベースは込み麺を持つ場合がある。
    ベース情報へのアクセスには `baseName` ヘルパー関数を使用する。
  - **麺の数量** は内部的に「半玉単位」で管理する（quantity 2 = 1玉）。
    表示変換には `noodleQuantityDisplay` を使用する。
  - `normalizeBase` は麺・トッピングの状態からベースを一意に決定する。
    麺・トッピングの操作後にこの関数を1つ呼ぶだけで整合性が保たれる。

-}


-- 型定義


{-| 麺の種類。麺の同一性識別に使用する。 -}
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
type BaseKind
    = Yasai
    | Soba
    | Udon
    | ZenbuIri


type alias BaseInfo =
    { name : String
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

`base` にベースの種類（`BaseKind`）を持ち、
`noodles` と `toppings` にそれぞれの追加オプションを格納する。
ベース名は `baseName` で取得できる。

-}
type alias Okonomiyaki =
    { base : BaseKind
    , noodles : List NoodleAddition
    , toppings : List ToppingAddition
    }


-- マスタデータ：BaseInfo


{-| 野菜入りのベースドメイン情報。 -}
baseYasaiBase : BaseInfo
baseYasaiBase =
    { name = "野菜入り"
    , includedNoodleKind = Nothing
    }


{-| そば入りのベースドメイン情報（そば1玉分込み）。 -}
baseSobaBase : BaseInfo
baseSobaBase =
    { name = "そば入り"
    , includedNoodleKind = Just NoodleKindSoba
    }


{-| うどん入りのベースドメイン情報（うどん1玉分込み）。 -}
baseUdonBase : BaseInfo
baseUdonBase =
    { name = "うどん入り"
    , includedNoodleKind = Just NoodleKindUdon
    }


{-| 全部入りのベースドメイン情報（イカ・エビ込み、麺は別途追加）。 -}
baseZenbuIriBase : BaseInfo
baseZenbuIriBase =
    { name = "全部入り"
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


{-| `noodle` が `kind` の込み麺として内包されているか判定する。

    isDefaultNoodleOf noodleSoba Soba  == True
    isDefaultNoodleOf noodleUdon Soba  == False
    isDefaultNoodleOf noodleSoba Yasai == False

-}
isDefaultNoodleOf : Noodle -> BaseKind -> Bool
isDefaultNoodleOf noodle kind =
    (kindToBase kind).includedNoodleKind == Just noodle.kind


{-| お好み焼きのベース名を取得する。 -}
baseName : Okonomiyaki -> String
baseName okonomiyaki =
    (kindToBase okonomiyaki.base).name


-- `BaseKind` からマスタデータ `BaseInfo` を取得する内部関数。
kindToBase : BaseKind -> BaseInfo
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


{-| `BaseKind` から初期 `Okonomiyaki` を生成する。

`includedNoodleKind` を持つベースの場合、`noodles` に1玉（`quantity = 2`）をセットする。
`includedNoodleKind` を持たない `Yasai` の場合、`noodles` は空になる。
`ZenbuIri` の場合、`noodles` にそば1玉をセットし、`toppings` にイカ・エビをセットする。

-}
init : BaseKind -> Okonomiyaki
init kind =
    let
        base =
            kindToBase kind

        initialNoodles =
            case base.includedNoodleKind of
                Just NoodleKindSoba ->
                    [ { noodle = noodleSoba, quantity = 2 } ]

                Just NoodleKindUdon ->
                    [ { noodle = noodleUdon, quantity = 2 } ]

                Nothing ->
                    case kind of
                        ZenbuIri ->
                            [ { noodle = noodleSoba, quantity = 2 } ]

                        _ ->
                            []

        initialToppings =
            case kind of
                ZenbuIri ->
                    [ { topping = toppingSquid, quantity = 1 }
                    , { topping = toppingShrimp, quantity = 1 }
                    ]

                _ ->
                    []
    in
    { base = kind
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

        hasNoodleKind noodleKind =
            List.any (\n -> n.noodle.kind == noodleKind) baseItem.noodles

        newBaseKind =
            if hasSquid && hasShrimp then
                ZenbuIri

            else if hasNoodleKind NoodleKindSoba then
                Soba

            else if hasNoodleKind NoodleKindUdon then
                Udon

            else
                Yasai
    in
    { baseItem | base = newBaseKind }


{-| お好み焼き1枚あたりの金額を計算する。

計算式：`基本料金（900円） + 麺料金 + トッピング料金 - 割引`

**麺の料金ルール：**

  - 麺がなければ 0円。
  - 麺がある場合：入場料（100円）は全麺合計で1回だけ加算、半玉単価（100円）× 合計quantity。
    例：そば1玉（quantity=2）→ 100 + 100×2 = 300円。
    例：そば1玉 + うどん1玉（合計quantity=4）→ 100 + 100×4 = 500円。

**割引ルール：**

  - イカとエビが同時にトッピングされている場合、300円割引を適用する。

-}
calculateTotal : Okonomiyaki -> Int
calculateTotal baseItem =
    let
        baseOkonomiyakiPrice =
            900

        totalNoodleQuantity =
            baseItem.noodles
                |> List.map .quantity
                |> List.sum

        noodlePrice =
            if totalNoodleQuantity == 0 then
                0

            else
                -- 入場料（100円）は麺全体で1回だけ加算、半玉単価（100円）× 合計quantity
                100 + 100 * totalNoodleQuantity

        toppingsPrice =
            baseItem.toppings
                |> List.map (\t -> t.topping.price * t.quantity)
                |> List.sum

        hasSquid =
            List.any (\t -> t.topping.kind == ToppingKindSquid) baseItem.toppings

        hasShrimp =
            List.any (\t -> t.topping.kind == ToppingKindShrimp) baseItem.toppings

        zenbuIriDiscount =
            if hasSquid && hasShrimp then
                300

            else
                0
    in
    baseOkonomiyakiPrice + noodlePrice + toppingsPrice - zenbuIriDiscount


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
