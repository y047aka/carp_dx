module Okonomiyaki exposing
    ( BaseKind(..)
    , Msg(..)
    , NoodleBadge
    , NoodleKind(..)
    , NoodleQuantity(..)
    , NoodleSelection(..)
    , Okonomiyaki
    , Topping
    , ToppingAddition
    , ToppingKind(..)
    , allToppings
    , baseKind
    , baseName
    , calculateTotal
    , init
    , noodleBadges
    , noodleSelectionDisplay
    , noodleSelectionName
    , toNoodleKind
    , toppingCheese
    , toppingGarlic
    , toppingIkaten
    , toppingMochi
    , toppingNegi
    , toppingShrimp
    , toppingSquid
    , update
    )

{-| お好み焼きドメインのモジュール。

お好み焼きのベース・麺・トッピングに関する型定義、マスタデータ、および
`Okonomiyaki` の構築・計算・正規化ロジックを提供する。

## 設計方針

  - **責務分離**: このモジュールは `MenuItem` に依存しない。
    `MenuItem` ↔ `BaseKind` の変換は `MenuData` モジュールが担う。
  - **麺の選択**: 麺は `NoodleSelection` 型で表現する。
    「なし / そば / うどん / ちゃんぽん」の4択から1つを選び、数量を指定する。
    ちゃんぽんはそばとうどんを同量ずつ含む。
  - **ベース**: `Okonomiyaki` 型は `base` フィールドを持たない。
    ベースは麺・トッピングの状態から `baseKind` で算出される**導出値**である。
    これにより、不整合なデータ状態が構造的に不可能になる。
    ベース情報へのアクセスには `baseName` や `baseKind` ヘルパー関数を使用する。
  - **麺の数量** は `NoodleQuantity` 型で表現する。
    `HalfBall`（0.5玉）、`Whole n`（n玉）、`WholeAndHalf n`（n.5玉）の3パターンで、
    不正な数量状態が型レベルで不可能になる。
  - **麺の価格** はモジュール内部定数で一元管理する。

-}


-- 型定義


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
    = YasaiIri
    | SobaIri
    | UdonIri
    | ZenbuIri


{-| 麺の数量。0.5玉単位で増減できる。

  - `HalfBall` は 0.5玉
  - `Whole n` は n玉（n >= 1）
  - `WholeAndHalf n` は n.5玉（n >= 1）

-}
type NoodleQuantity
    = HalfBall
    | Whole Int
    | WholeAndHalf Int


{-| 麺の選択状態。種類と数量を一体で管理する。

  - `WithoutNoodle` は麺なし
  - `WithSoba q` はそば単体（数量 q）
  - `WithUdon q` はうどん単体（数量 q）
  - `WithChampon n` はちゃんぽん（n玉、そばとうどんを同量ずつ含む）

-}
type NoodleSelection
    = WithoutNoodle
    | WithSoba NoodleQuantity
    | WithUdon NoodleQuantity
    | WithChampon Int


{-| 麺の種類選択（数量なし）。UI の種類切替ボタンで使用する。

種類切替時に既存の数量を可能な限り維持するために、
`SelectNoodleKind` メッセージの引数として使用する。

-}
type NoodleKind
    = NoNoodle
    | Soba
    | Udon
    | Champon


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


{-| お好み焼きに付属するトッピングの追加オプション。

`quantity` はトッピングの個数単位。

-}
type alias ToppingAddition =
    { topping : Topping
    , quantity : Int
    }


{-| バッジ表示用の麺情報。内部型を漏らさず、表示に必要な情報のみ提供する。 -}
type alias NoodleBadge =
    { name : String
    , quantityDisplay : String
    }


{-| お好み焼き1枚の完全な構成。

`noodleSelection` で麺の種類と数量を管理し、`toppings` にトッピングの追加オプションを格納する。
ベース種類は `baseKind` で算出される導出値である。
ベース名は `baseName` で取得できる。

-}
type alias Okonomiyaki =
    { noodleSelection : NoodleSelection
    , toppings : List ToppingAddition
    , selectedBase : BaseKind
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
    , defaultNoodleSelection : NoodleSelection
    , defaultToppings : List ToppingAddition
    }


yasaiPreset : BasePreset
yasaiPreset =
    { name = "野菜入り"
    , defaultNoodleSelection = WithoutNoodle
    , defaultToppings = []
    }


sobaPreset : BasePreset
sobaPreset =
    { name = "そば入り"
    , defaultNoodleSelection = WithSoba (Whole 1)
    , defaultToppings = []
    }


udonPreset : BasePreset
udonPreset =
    { name = "うどん入り"
    , defaultNoodleSelection = WithUdon (Whole 1)
    , defaultToppings = []
    }


zenbuIriPreset : BasePreset
zenbuIriPreset =
    { name = "全部入り"
    , defaultNoodleSelection = WithSoba (Whole 1)
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


-- 表示・クエリ


{-| `NoodleQuantity` を表示用文字列に変換する（内部用）。 -}
noodleQuantityDisplay : NoodleQuantity -> String
noodleQuantityDisplay q =
    case q of
        HalfBall ->
            "0.5"

        Whole n ->
            String.fromInt n

        WholeAndHalf n ->
            String.fromInt n ++ ".5"


{-| `NoodleSelection` の数量を表示用文字列に変換する。 -}
noodleSelectionDisplay : NoodleSelection -> String
noodleSelectionDisplay selection =
    case selection of
        WithoutNoodle ->
            "0"

        WithSoba q ->
            noodleQuantityDisplay q

        WithUdon q ->
            noodleQuantityDisplay q

        WithChampon n ->
            String.fromInt n


{-| `NoodleSelection` の麺名を返す。`WithoutNoodle` の場合は空文字列。 -}
noodleSelectionName : NoodleSelection -> String
noodleSelectionName selection =
    case selection of
        WithoutNoodle ->
            ""

        WithSoba _ ->
            "そば"

        WithUdon _ ->
            "うどん"

        WithChampon _ ->
            "ちゃんぽん"


{-| `NoodleSelection` から対応する `NoodleKind` を導出する。 -}
toNoodleKind : NoodleSelection -> NoodleKind
toNoodleKind selection =
    case selection of
        WithoutNoodle ->
            NoNoodle

        WithSoba _ ->
            Soba

        WithUdon _ ->
            Udon

        WithChampon _ ->
            Champon


{-| `NoodleSelection` をバッジ表示用のリストに変換する。

ちゃんぽんの場合はそばとうどんの2件に展開される。

-}
noodleBadges : NoodleSelection -> List NoodleBadge
noodleBadges selection =
    case selection of
        WithoutNoodle ->
            []

        WithSoba q ->
            [ { name = "そば", quantityDisplay = noodleQuantityDisplay q } ]

        WithUdon q ->
            [ { name = "うどん", quantityDisplay = noodleQuantityDisplay q } ]

        WithChampon n ->
            let
                q =
                    champonPerNoodleQuantity n

                display =
                    noodleQuantityDisplay q
            in
            [ { name = "そば", quantityDisplay = display }
            , { name = "うどん", quantityDisplay = display }
            ]


{-| ちゃんぽん n 玉における各麺の NoodleQuantity を計算する。

ちゃんぽん 1玉 = そば 0.5玉 + うどん 0.5玉
ちゃんぽん 2玉 = そば 1玉 + うどん 1玉
ちゃんぽん 3玉 = そば 1.5玉 + うどん 1.5玉

-}
champonPerNoodleQuantity : Int -> NoodleQuantity
champonPerNoodleQuantity n =
    let
        safeN =
            max 1 n
    in
    if safeN == 1 then
        HalfBall

    else if modBy 2 safeN == 0 then
        Whole (safeN // 2)

    else
        WholeAndHalf (safeN // 2)


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


{-| `NoodleSelection` の合計半玉数を計算する（内部計算用）。 -}
selectionTotalHalfBalls : NoodleSelection -> Int
selectionTotalHalfBalls selection =
    case selection of
        WithoutNoodle ->
            0

        WithSoba q ->
            toHalfBallCount q

        WithUdon q ->
            toHalfBallCount q

        WithChampon n ->
            -- そば n半玉 + うどん n半玉 = 2n半玉
            2 * n


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

  1. メニューで全部入りを選択済み かつ イカとエビが両方 toppings にあれば → ZenbuIri
  2. そばが含まれていれば → SobaIri
  3. うどんが含まれていれば → UdonIri
  4. いずれでもなければ → YasaiIri

-}
baseKind : Okonomiyaki -> BaseKind
baseKind okonomiyaki =
    let
        hasSquid =
            List.any (\t -> t.topping.kind == ToppingKindSquid) okonomiyaki.toppings

        hasShrimp =
            List.any (\t -> t.topping.kind == ToppingKindShrimp) okonomiyaki.toppings

    in
    if okonomiyaki.selectedBase == ZenbuIri && hasSquid && hasShrimp then
        ZenbuIri

    else
        case okonomiyaki.noodleSelection of
            WithSoba _ ->
                SobaIri

            WithChampon _ ->
                SobaIri

            WithUdon _ ->
                UdonIri

            WithoutNoodle ->
                YasaiIri


{-| お好み焼きのベース名を取得する。 -}
baseName : Okonomiyaki -> String
baseName okonomiyaki =
    case baseKind okonomiyaki of
        YasaiIri ->
            yasaiPreset.name

        SobaIri ->
            sobaPreset.name

        UdonIri ->
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
                YasaiIri ->
                    yasaiPreset

                SobaIri ->
                    sobaPreset

                UdonIri ->
                    udonPreset

                ZenbuIri ->
                    zenbuIriPreset
    in
    { noodleSelection = preset.defaultNoodleSelection
    , toppings = preset.defaultToppings
    , selectedBase = kind
    }


{-| お好み焼き1枚あたりの金額を計算する。

計算式：`基本料金（900円） + 麺料金 + トッピング料金 - 割引`

**麺の料金ルール：**

  - 麺がなければ 0円。
  - 麺がある場合：入場料（100円）は全麺合計で1回だけ加算、半玉単価（100円）× 合計半玉数。
    例：そば1玉 → 100 + 100×2 = 300円。
    例：ちゃんぽん1玉（そば0.5+うどん0.5）→ 100 + 100×2 = 300円。

**割引ルール：**

  - 全部入り（ZenbuIri）の場合、300円割引を適用する。

-}
calculateTotal : Okonomiyaki -> Int
calculateTotal okonomiyaki =
    let
        baseOkonomiyakiPrice =
            900

        totalHalfBalls =
            selectionTotalHalfBalls okonomiyaki.noodleSelection

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


-- Msg / update


{-| 麺の種類切替を適用する。数量は可能な限り維持する。

  - WithoutNoodle → 任意: デフォルト数量
  - そば/うどん → そば/うどん: NoodleQuantity をそのまま維持
  - そば/うどん → ちゃんぽん: 半玉数を切り上げてちゃんぽん玉数に変換
  - ちゃんぽん → そば/うどん: ちゃんぽん玉数を Whole n に変換
  - 任意 → NoNoodle（NoodleKind）: WithoutNoodle に遷移

-}
applyNoodleKind : NoodleKind -> NoodleSelection -> NoodleSelection
applyNoodleKind choice current =
    case choice of
        NoNoodle ->
            WithoutNoodle

        Soba ->
            case current of
                WithoutNoodle ->
                    WithSoba (Whole 1)

                WithSoba _ ->
                    current

                WithUdon q ->
                    WithSoba q

                WithChampon n ->
                    WithSoba (Whole n)

        Udon ->
            case current of
                WithoutNoodle ->
                    WithUdon (Whole 1)

                WithUdon _ ->
                    current

                WithSoba q ->
                    WithUdon q

                WithChampon n ->
                    WithUdon (Whole n)

        Champon ->
            case current of
                WithoutNoodle ->
                    WithChampon 1

                WithChampon _ ->
                    current

                WithSoba q ->
                    WithChampon (halfBallsToChampon (toHalfBallCount q))

                WithUdon q ->
                    WithChampon (halfBallsToChampon (toHalfBallCount q))


{-| 半玉数をちゃんぽん玉数に変換する（切り上げ）。 -}
halfBallsToChampon : Int -> Int
halfBallsToChampon halfBalls =
    max 1 ((halfBalls + 1) // 2)


{-| お好み焼き編集操作のメッセージ型。 -}
type Msg
    = SelectNoodleKind NoodleKind
    | IncrementNoodleQuantity
    | DecrementNoodleQuantity
    | ToggleTopping Topping


{-| `Msg` を `Okonomiyaki` に適用する。 -}
update : Msg -> Okonomiyaki -> Okonomiyaki
update msg okonomiyaki =
    case msg of
        SelectNoodleKind choice ->
            { okonomiyaki | noodleSelection = applyNoodleKind choice okonomiyaki.noodleSelection }

        IncrementNoodleQuantity ->
            { okonomiyaki
                | noodleSelection =
                    case okonomiyaki.noodleSelection of
                        WithoutNoodle ->
                            WithoutNoodle

                        WithSoba q ->
                            WithSoba (incrementNoodleQuantity q)

                        WithUdon q ->
                            WithUdon (incrementNoodleQuantity q)

                        WithChampon n ->
                            WithChampon (n + 1)
            }

        DecrementNoodleQuantity ->
            { okonomiyaki
                | noodleSelection =
                    case okonomiyaki.noodleSelection of
                        WithoutNoodle ->
                            WithoutNoodle

                        WithSoba q ->
                            case decrementNoodleQuantity q of
                                Just newQ ->
                                    WithSoba newQ

                                Nothing ->
                                    WithoutNoodle

                        WithUdon q ->
                            case decrementNoodleQuantity q of
                                Just newQ ->
                                    WithUdon newQ

                                Nothing ->
                                    WithoutNoodle

                        WithChampon n ->
                            if n <= 1 then
                                WithoutNoodle

                            else
                                WithChampon (n - 1)
            }

        ToggleTopping topping ->
            if List.any (\t -> t.topping.kind == topping.kind) okonomiyaki.toppings then
                { okonomiyaki | toppings = List.filter (\t -> t.topping.kind /= topping.kind) okonomiyaki.toppings }

            else
                { okonomiyaki | toppings = okonomiyaki.toppings ++ [ { topping = topping, quantity = 1 } ] }
