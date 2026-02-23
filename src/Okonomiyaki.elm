module Okonomiyaki exposing
    ( BaseKind(..)
    , Noodle
    , NoodleAddition
    , NoodleKind(..)
    , NoodleQuantity(..)
    , Okonomiyaki
    , Topping
    , ToppingAddition
    , ToppingKind(..)
    , addTopping
    , allNoodles
    , allToppings
    , baseKind
    , baseName
    , calculateTotal
    , decrementNoodle
    , incrementNoodle
    , init
    , noodleAdditionPrice
    , noodleQuantityDisplay
    , noodleSoba
    , noodleUdon
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
  - **ベース**: `Okonomiyaki` 型は `base` フィールドを持たない。
    ベースは麺・トッピングの状態から `baseKind` で算出される**導出値**である。
    これにより、不整合なデータ状態が構造的に不可能になる。
    ベース情報へのアクセスには `baseName` や `baseKind` ヘルパー関数を使用する。
  - **麺の数量** は `NoodleQuantity` 型で表現する。
    `HalfBall`（0.5玉）、`Whole n`（n玉）、`WholeAndHalf n`（n.5玉）の3パターンで、
    不正な数量状態が型レベルで不可能になる。
    表示変換には `noodleQuantityDisplay` を使用する。
  - **麺の価格** はモジュール内部定数で一元管理する。
    表示用の個別価格は `noodleAdditionPrice` で取得できる。

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


{-| 麺の数量。0.5玉単位で増減できる。

  - `HalfBall` は 0.5玉
  - `Whole n` は n玉（n >= 1）
  - `WholeAndHalf n` は n.5玉（n >= 1）

`NoodleAddition.quantity` フィールドで使用する。
`incrementNoodleQuantity` / `decrementNoodleQuantity` で増減できる。

-}
type NoodleQuantity
    = HalfBall
    | Whole Int
    | WholeAndHalf Int


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
  - `name` は表示名

麺の価格はモジュール内定数（入場料・半玉単価）で管理する。
個別の麺追加分の価格は `noodleAdditionPrice` で取得できる。

-}
type alias Noodle =
    { kind : NoodleKind
    , name : String
    }


{-| お好み焼きに付属するトッピングの追加オプション。

`quantity` はトッピングの個数単位。

-}
type alias ToppingAddition =
    { topping : Topping
    , quantity : Int
    }


{-| お好み焼きに付属する麺の追加オプション。

`quantity` は `NoodleQuantity` 型で表現する。

-}
type alias NoodleAddition =
    { noodle : Noodle
    , quantity : NoodleQuantity
    }


{-| お好み焼き1枚の完全な構成。

`noodles` と `toppings` にそれぞれの追加オプションを格納する。
ベース種類は `baseKind` で算出される導出値である。
ベース名は `baseName` で取得できる。

-}
type alias Okonomiyaki =
    { noodles : List NoodleAddition
    , toppings : List ToppingAddition
    }


-- 価格定数（モジュール内部）


{-| 麺の入場料。麺がある場合、全麺合計で1回だけ加算される。 -}
noodleEntryPrice : Int
noodleEntryPrice =
    100


{-| 半玉あたりの追加料金。 -}
noodleHalfBallPrice : Int
noodleHalfBallPrice =
    100


-- マスタデータ：BasePreset


type alias BasePreset =
    { name : String
    , defaultNoodles : List NoodleAddition
    , defaultToppings : List ToppingAddition
    }


yasaiPreset : BasePreset
yasaiPreset =
    { name = "野菜入り"
    , defaultNoodles = []
    , defaultToppings = []
    }


sobaPreset : BasePreset
sobaPreset =
    { name = "そば入り"
    , defaultNoodles = [ { noodle = noodleSoba, quantity = Whole 1 } ]
    , defaultToppings = []
    }


udonPreset : BasePreset
udonPreset =
    { name = "うどん入り"
    , defaultNoodles = [ { noodle = noodleUdon, quantity = Whole 1 } ]
    , defaultToppings = []
    }


zenbuIriPreset : BasePreset
zenbuIriPreset =
    { name = "全部入り"
    , defaultNoodles = [ { noodle = noodleSoba, quantity = Whole 1 } ]
    , defaultToppings =
        [ { topping = toppingSquid, quantity = 1 }
        , { topping = toppingShrimp, quantity = 1 }
        ]
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


{-| そば。 -}
noodleSoba : Noodle
noodleSoba =
    { kind = NoodleKindSoba
    , name = "そば"
    }


{-| うどん。 -}
noodleUdon : Noodle
noodleUdon =
    { kind = NoodleKindUdon
    , name = "うどん"
    }


{-| 全麺一覧。UI の麺選択画面に使用する。 -}
allNoodles : List Noodle
allNoodles =
    [ noodleSoba, noodleUdon ]


-- 表示・クエリ


{-| `NoodleQuantity` を表示用文字列に変換する。

    noodleQuantityDisplay HalfBall         == "0.5"
    noodleQuantityDisplay (Whole 1)        == "1"
    noodleQuantityDisplay (WholeAndHalf 1) == "1.5"
    noodleQuantityDisplay (Whole 2)        == "2"

-}
noodleQuantityDisplay : NoodleQuantity -> String
noodleQuantityDisplay q =
    case q of
        HalfBall ->
            "0.5"

        Whole n ->
            String.fromInt n

        WholeAndHalf n ->
            String.fromInt n ++ ".5"


{-| 麺追加1件分の価格を計算する（表示用）。

計算式：入場料（100円）＋ 半玉単価（100円）× 半玉数

複数麺がある場合の「入場料は全麺合計で1回だけ」というルールは
`calculateTotal` が担保する。この関数は個別の表示目的に使用する。

-}
noodleAdditionPrice : NoodleAddition -> Int
noodleAdditionPrice na =
    noodleEntryPrice + noodleHalfBallPrice * toHalfBallCount na.quantity


{-| `NoodleQuantity` を半玉単位の整数に変換する（内部計算用）。 -}
toHalfBallCount : NoodleQuantity -> Int
toHalfBallCount q =
    case q of
        HalfBall ->
            1

        Whole n ->
            2 * n

        WholeAndHalf n ->
            2 * n + 1


{-| `NoodleQuantity` を0.5玉増やす。 -}
incrementNoodleQuantity : NoodleQuantity -> NoodleQuantity
incrementNoodleQuantity q =
    case q of
        HalfBall ->
            Whole 1

        Whole n ->
            WholeAndHalf n

        WholeAndHalf n ->
            Whole (n + 1)


{-| `NoodleQuantity` を0.5玉減らす。0.5玉より少なくなる場合は `Nothing` を返す。 -}
decrementNoodleQuantity : NoodleQuantity -> Maybe NoodleQuantity
decrementNoodleQuantity q =
    case q of
        HalfBall ->
            Nothing

        Whole 1 ->
            Just HalfBall

        Whole n ->
            Just (WholeAndHalf (n - 1))

        WholeAndHalf n ->
            Just (Whole n)


{-| お好み焼きの麺・トッピング状態からベース種類を一意に決定する。

ベース決定ルール（優先順位順）:

  1. イカとエビが両方 toppings にあれば → ZenbuIri
  2. そばが noodles にあれば → Soba
  3. うどんが noodles にあれば → Udon
  4. いずれでもなければ → Yasai

-}
baseKind : Okonomiyaki -> BaseKind
baseKind okonomiyaki =
    let
        hasSquid =
            List.any (\t -> t.topping.kind == ToppingKindSquid) okonomiyaki.toppings

        hasShrimp =
            List.any (\t -> t.topping.kind == ToppingKindShrimp) okonomiyaki.toppings

        hasNoodleKind noodleKind =
            List.any (\n -> n.noodle.kind == noodleKind) okonomiyaki.noodles
    in
    if hasSquid && hasShrimp then
        ZenbuIri

    else if hasNoodleKind NoodleKindSoba then
        Soba

    else if hasNoodleKind NoodleKindUdon then
        Udon

    else
        Yasai


{-| お好み焼きのベース名を取得する。 -}
baseName : Okonomiyaki -> String
baseName okonomiyaki =
    case baseKind okonomiyaki of
        Yasai ->
            yasaiPreset.name

        Soba ->
            sobaPreset.name

        Udon ->
            udonPreset.name

        ZenbuIri ->
            zenbuIriPreset.name


-- 構築・正規化・計算


{-| `BaseKind` から初期 `Okonomiyaki` を生成する。

初期の麺・トッピング構成は `BasePreset` マスタデータから取得する。

-}
init : BaseKind -> Okonomiyaki
init kind =
    let
        preset =
            case kind of
                Yasai ->
                    yasaiPreset

                Soba ->
                    sobaPreset

                Udon ->
                    udonPreset

                ZenbuIri ->
                    zenbuIriPreset
    in
    { noodles = preset.defaultNoodles
    , toppings = preset.defaultToppings
    }


{-| お好み焼き1枚あたりの金額を計算する。

計算式：`基本料金（900円） + 麺料金 + トッピング料金 - 割引`

**麺の料金ルール：**

  - 麺がなければ 0円。
  - 麺がある場合：入場料（100円）は全麺合計で1回だけ加算、半玉単価（100円）× 合計半玉数。
    例：そば1玉 → 100 + 100×2 = 300円。
    例：そば1玉 + うどん1玉（合計4半玉）→ 100 + 100×4 = 500円。

**割引ルール：**

  - 全部入り（ZenbuIri）の場合、300円割引を適用する。

-}
calculateTotal : Okonomiyaki -> Int
calculateTotal okonomiyaki =
    let
        baseOkonomiyakiPrice =
            900

        totalHalfBalls =
            okonomiyaki.noodles
                |> List.map (.quantity >> toHalfBallCount)
                |> List.sum

        noodlePrice =
            if totalHalfBalls == 0 then
                0

            else
                -- 入場料は麺全体で1回だけ加算、半玉単価 × 合計半玉数
                noodleEntryPrice + noodleHalfBallPrice * totalHalfBalls

        toppingsPrice =
            okonomiyaki.toppings
                |> List.map (\t -> t.topping.price * t.quantity)
                |> List.sum

        zenbuIriDiscount =
            if baseKind okonomiyaki == ZenbuIri then
                300

            else
                0
    in
    baseOkonomiyakiPrice + noodlePrice + toppingsPrice - zenbuIriDiscount


-- 操作


{-| 麺を0.5玉増やす。同種の麺があれば `incrementNoodleQuantity` で増量、なければ1玉（`Whole 1`）で新規追加する。 -}
incrementNoodle : Noodle -> Okonomiyaki -> Okonomiyaki
incrementNoodle noodle item =
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
                            { n | quantity = incrementNoodleQuantity n.quantity }

                        else
                            n
                    )
                    item.noodles
        }

    else
        { item | noodles = item.noodles ++ [ { noodle = noodle, quantity = Whole 1 } ] }


{-| 麺を0.5玉減らす。`HalfBall`（0.5玉）から減らすと削除される。 -}
decrementNoodle : Noodle -> Okonomiyaki -> Okonomiyaki
decrementNoodle noodle item =
    { item
        | noodles =
            List.filterMap
                (\n ->
                    if n.noodle.kind == noodle.kind then
                        decrementNoodleQuantity n.quantity
                            |> Maybe.map (\q -> { n | quantity = q })

                    else
                        Just n
                )
                item.noodles
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
