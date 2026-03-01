module OkonomiyakiTest exposing (suite)

import Expect
import Okonomiyaki
import Test exposing (..)


suite : Test
suite =
    describe "Okonomiyaki"
        [ describe "noodleSelectionDisplay"
            [ test "WithoutNoodle → \"0\"" <|
                \_ -> Okonomiyaki.noodleSelectionDisplay Okonomiyaki.WithoutNoodle |> Expect.equal "0"
            , test "WithSoba HalfBall → \"0.5\"" <|
                \_ -> Okonomiyaki.noodleSelectionDisplay (Okonomiyaki.WithSoba Okonomiyaki.HalfBall) |> Expect.equal "0.5"
            , test "WithSoba (Whole 1) → \"1\"" <|
                \_ -> Okonomiyaki.noodleSelectionDisplay (Okonomiyaki.WithSoba (Okonomiyaki.Whole 1)) |> Expect.equal "1"
            , test "WithUdon (WholeAndHalf 1) → \"1.5\"" <|
                \_ -> Okonomiyaki.noodleSelectionDisplay (Okonomiyaki.WithUdon (Okonomiyaki.WholeAndHalf 1)) |> Expect.equal "1.5"
            , test "WithChampon 2 → \"2\"" <|
                \_ -> Okonomiyaki.noodleSelectionDisplay (Okonomiyaki.WithChampon 2) |> Expect.equal "2"
            ]
        , describe "init"
            [ describe "Yasai（込み麺なし）"
                [ test "noodleSelection が WithoutNoodle で生成される" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.YasaiIri
                            |> .noodleSelection
                            |> Expect.equal Okonomiyaki.WithoutNoodle
                ]
            , describe "Soba（込み麺: そば）"
                [ test "noodleSelection が WithSoba (Whole 1) でセットされる" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.SobaIri
                            |> .noodleSelection
                            |> Expect.equal (Okonomiyaki.WithSoba (Okonomiyaki.Whole 1))
                ]
            , describe "Udon（込み麺: うどん）"
                [ test "noodleSelection が WithUdon (Whole 1) でセットされる" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.UdonIri
                            |> .noodleSelection
                            |> Expect.equal (Okonomiyaki.WithUdon (Okonomiyaki.Whole 1))
                ]
            , describe "ZenbuIri（初期: そば1玉 + イカ・エビ）"
                [ test "noodleSelection が WithSoba (Whole 1) でセットされる" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.ZenbuIri
                            |> .noodleSelection
                            |> Expect.equal (Okonomiyaki.WithSoba (Okonomiyaki.Whole 1))
                , test "toppings にイカとエビが各1個でセットされる" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.ZenbuIri
                            |> .toppings
                            |> List.map (\t -> ( t.topping.kind, t.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.ToppingKindSquid, 1 ), ( Okonomiyaki.ToppingKindShrimp, 1 ) ]
                ]
            , describe "共通の初期値"
                [ test "toppings は空で初期化される（込みトッピングなしのベース）" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.YasaiIri
                            |> .toppings
                            |> Expect.equal []
                ]
            ]
        , describe "baseKind"
            [ describe "麺の状態に応じたベース決定"
                [ test "そばがあれば Soba になる" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 1)
                        , toppings = []
                        , selectedBase = Okonomiyaki.YasaiIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.SobaIri
                , test "うどんがあれば Udon になる" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithUdon (Okonomiyaki.Whole 1)
                        , toppings = []
                        , selectedBase = Okonomiyaki.YasaiIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.UdonIri
                , test "麺がなければ Yasai になる" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithoutNoodle
                        , toppings = []
                        , selectedBase = Okonomiyaki.YasaiIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.YasaiIri
                , test "そば0.5玉でも Soba になる" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba Okonomiyaki.HalfBall
                        , toppings = []
                        , selectedBase = Okonomiyaki.YasaiIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.SobaIri
                , test "ちゃんぽんは Soba になる（そば優先）" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithChampon 1
                        , toppings = []
                        , selectedBase = Okonomiyaki.YasaiIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.SobaIri
                ]
            , describe "全部入り（ZenbuIri）の判定：selectedBase == ZenbuIri かつ イカ+エビ"
                [ test "selectedBase=ZenbuIri + イカ + エビ（そば有り）→ ZenbuIri" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 1)
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "selectedBase=ZenbuIri + イカ + エビ（うどん有り）→ ZenbuIri" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithUdon (Okonomiyaki.Whole 1)
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "selectedBase=ZenbuIri + イカ + エビ（麺なし）→ ZenbuIri" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithoutNoodle
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "selectedBase が ZenbuIri でなければイカ+エビがあっても ZenbuIri にならない（Soba）" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 1)
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        , selectedBase = Okonomiyaki.SobaIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.SobaIri
                , test "selectedBase が ZenbuIri でなければイカ+エビがあっても ZenbuIri にならない（Yasai）" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithoutNoodle
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        , selectedBase = Okonomiyaki.YasaiIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.YasaiIri
                ]
            , describe "全部入り条件が崩れると麺に応じたベースに戻る"
                [ test "selectedBase=ZenbuIri でもエビのみ（そば残り）→ Soba" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 1)
                        , toppings = [ { topping = Okonomiyaki.toppingShrimp, quantity = 1 } ]
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.SobaIri
                , test "selectedBase=ZenbuIri でもイカのみ（うどん残り）→ Udon" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithUdon (Okonomiyaki.Whole 1)
                        , toppings = [ { topping = Okonomiyaki.toppingSquid, quantity = 1 } ]
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.UdonIri
                , test "selectedBase=ZenbuIri でもイカのみ（麺なし）→ Yasai" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithoutNoodle
                        , toppings = [ { topping = Okonomiyaki.toppingSquid, quantity = 1 } ]
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.YasaiIri
                , test "selectedBase=ZenbuIri でもトッピングを全て外す（そば残り）→ Soba" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 1)
                        , toppings = []
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.SobaIri
                , test "selectedBase=ZenbuIri でもトッピングを全て外す（麺なし）→ Yasai" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithoutNoodle
                        , toppings = []
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.YasaiIri
                ]
            , describe "イカのみ・エビのみでは ZenbuIri にならない"
                [ test "Soba + イカのみ → Soba" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 1)
                        , toppings = [ { topping = Okonomiyaki.toppingSquid, quantity = 1 } ]
                        , selectedBase = Okonomiyaki.YasaiIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.SobaIri
                , test "Soba + エビのみ → Soba" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 1)
                        , toppings = [ { topping = Okonomiyaki.toppingShrimp, quantity = 1 } ]
                        , selectedBase = Okonomiyaki.YasaiIri
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.SobaIri
                ]
            ]
        , describe "calculateTotal"
            [ describe "麺なし"
                [ test "麺なし = 900円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithoutNoodle, toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 900
                ]
            , describe "麺あり: 900 + 入場料(100) + 半玉単価(100)×半玉数"
                [ test "そば0.5玉（HalfBall）: 900 + (100 + 100×1) = 1100円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba Okonomiyaki.HalfBall, toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1100
                , test "そば1玉（Whole 1）: 900 + (100 + 100×2) = 1200円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 1), toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1200
                , test "そば1.5玉（WholeAndHalf 1）: 900 + (100 + 100×3) = 1300円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.WholeAndHalf 1), toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1300
                , test "そば2玉（Whole 2）: 900 + (100 + 100×4) = 1400円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 2), toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1400
                ]
            , describe "ちゃんぽん: 900 + 入場料(100) + 半玉単価(100)×(2n)"
                [ test "ちゃんぽん1玉: 900 + (100 + 100×2) = 1200円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithChampon 1, toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1200
                , test "ちゃんぽん2玉: 900 + (100 + 100×4) = 1400円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithChampon 2, toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1400
                , test "ちゃんぽん3玉: 900 + (100 + 100×6) = 1600円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithChampon 3, toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1600
                ]
            , describe "トッピングあり"
                [ test "イカ天×1: 900 + 200 = 1100円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithoutNoodle, toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 1 } ], selectedBase = Okonomiyaki.YasaiIri }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1100
                , test "イカ天×1 + ねぎかけ×2: 900 + 200 + (250×2) = 1600円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithoutNoodle, toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 1 }, { topping = Okonomiyaki.toppingNegi, quantity = 2 } ], selectedBase = Okonomiyaki.YasaiIri }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1600
                ]
            , describe "麺 + トッピングの組み合わせ"
                [ test "そば0.5玉 + イカ天 + ねぎかけ: 900 + (100+100) + 200 + 250 = 1550円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba Okonomiyaki.HalfBall
                        , toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 1 }, { topping = Okonomiyaki.toppingNegi, quantity = 1 } ]
                        , selectedBase = Okonomiyaki.YasaiIri
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1550
                , test "そば0.5玉 + イカ天×2: 900 + (100+100) + 200×2 = 1500円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba Okonomiyaki.HalfBall
                        , toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 2 } ]
                        , selectedBase = Okonomiyaki.YasaiIri
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1500
                ]
            , describe "全部入り（ZenbuIri）の価格計算"
                [ test "イカ + エビ（麺なし）= 1400円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithoutNoodle
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1400
                , test "そば1玉 + イカ + エビ = 1700円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 1)
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1700
                , test "うどん1玉 + イカ + エビ = 1700円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithUdon (Okonomiyaki.Whole 1)
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1700
                , test "そば0.5玉 + イカ + エビ = 1600円" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba Okonomiyaki.HalfBall
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        , selectedBase = Okonomiyaki.ZenbuIri
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1600
                , test "selectedBase が ZenbuIri でなければイカ+エビがあっても割引なし（そば1玉+イカ+エビ = 2000円）" <|
                    \_ ->
                        { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 1)
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        , selectedBase = Okonomiyaki.SobaIri
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 2000
                ]
            ]
        , describe "SelectNoodleKind（数量維持）"
            [ test "WithoutNoodle → ChooseSoba: デフォルト Whole 1" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.YasaiIri
                        |> Okonomiyaki.update (Okonomiyaki.SelectNoodleKind Okonomiyaki.Soba)
                        |> .noodleSelection
                        |> Expect.equal (Okonomiyaki.WithSoba (Okonomiyaki.Whole 1))
            , test "WithSoba (Whole 1) → ChooseChampon: 半玉2 → ちゃんぽん1玉" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.SobaIri
                        |> Okonomiyaki.update (Okonomiyaki.SelectNoodleKind Okonomiyaki.Champon)
                        |> .noodleSelection
                        |> Expect.equal (Okonomiyaki.WithChampon 1)
            , test "WithSoba (WholeAndHalf 1) → ChooseChampon: 半玉3 → ちゃんぽん2玉（切り上げ）" <|
                \_ ->
                    { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.WholeAndHalf 1), toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                        |> Okonomiyaki.update (Okonomiyaki.SelectNoodleKind Okonomiyaki.Champon)
                        |> .noodleSelection
                        |> Expect.equal (Okonomiyaki.WithChampon 2)
            , test "WithSoba (Whole 2) → ChooseUdon: 数量維持" <|
                \_ ->
                    { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.Whole 2), toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                        |> Okonomiyaki.update (Okonomiyaki.SelectNoodleKind Okonomiyaki.Udon)
                        |> .noodleSelection
                        |> Expect.equal (Okonomiyaki.WithUdon (Okonomiyaki.Whole 2))
            , test "WithChampon 2 → ChooseSoba: Whole 2" <|
                \_ ->
                    { noodleSelection = Okonomiyaki.WithChampon 2, toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                        |> Okonomiyaki.update (Okonomiyaki.SelectNoodleKind Okonomiyaki.Soba)
                        |> .noodleSelection
                        |> Expect.equal (Okonomiyaki.WithSoba (Okonomiyaki.Whole 2))
            , test "同じ種類を再選択しても変わらない" <|
                \_ ->
                    { noodleSelection = Okonomiyaki.WithSoba (Okonomiyaki.WholeAndHalf 2), toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                        |> Okonomiyaki.update (Okonomiyaki.SelectNoodleKind Okonomiyaki.Soba)
                        |> .noodleSelection
                        |> Expect.equal (Okonomiyaki.WithSoba (Okonomiyaki.WholeAndHalf 2))
            , test "ChooseNoNoodle で麺なしになる" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.SobaIri
                        |> Okonomiyaki.update (Okonomiyaki.SelectNoodleKind Okonomiyaki.NoNoodle)
                        |> .noodleSelection
                        |> Expect.equal Okonomiyaki.WithoutNoodle
            ]
        , describe "IncrementNoodleQuantity"
            [ test "そば: Whole 1 → WholeAndHalf 1" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.SobaIri
                        |> Okonomiyaki.update Okonomiyaki.IncrementNoodleQuantity
                        |> .noodleSelection
                        |> Expect.equal (Okonomiyaki.WithSoba (Okonomiyaki.WholeAndHalf 1))
            , test "うどん: Whole 1 → WholeAndHalf 1" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.UdonIri
                        |> Okonomiyaki.update Okonomiyaki.IncrementNoodleQuantity
                        |> .noodleSelection
                        |> Expect.equal (Okonomiyaki.WithUdon (Okonomiyaki.WholeAndHalf 1))
            , test "ちゃんぽん: 1 → 2" <|
                \_ ->
                    { noodleSelection = Okonomiyaki.WithChampon 1, toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                        |> Okonomiyaki.update Okonomiyaki.IncrementNoodleQuantity
                        |> .noodleSelection
                        |> Expect.equal (Okonomiyaki.WithChampon 2)
            , test "WithoutNoodle に対しては何もしない" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.YasaiIri
                        |> Okonomiyaki.update Okonomiyaki.IncrementNoodleQuantity
                        |> .noodleSelection
                        |> Expect.equal Okonomiyaki.WithoutNoodle
            ]
        , describe "DecrementNoodleQuantity"
            [ test "そば: Whole 1 → HalfBall" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.SobaIri
                        |> Okonomiyaki.update Okonomiyaki.DecrementNoodleQuantity
                        |> .noodleSelection
                        |> Expect.equal (Okonomiyaki.WithSoba Okonomiyaki.HalfBall)
            , test "そば: HalfBall → WithoutNoodle" <|
                \_ ->
                    { noodleSelection = Okonomiyaki.WithSoba Okonomiyaki.HalfBall, toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                        |> Okonomiyaki.update Okonomiyaki.DecrementNoodleQuantity
                        |> .noodleSelection
                        |> Expect.equal Okonomiyaki.WithoutNoodle
            , test "ちゃんぽん: 2 → 1" <|
                \_ ->
                    { noodleSelection = Okonomiyaki.WithChampon 2, toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                        |> Okonomiyaki.update Okonomiyaki.DecrementNoodleQuantity
                        |> .noodleSelection
                        |> Expect.equal (Okonomiyaki.WithChampon 1)
            , test "ちゃんぽん: 1 → WithoutNoodle" <|
                \_ ->
                    { noodleSelection = Okonomiyaki.WithChampon 1, toppings = [], selectedBase = Okonomiyaki.YasaiIri }
                        |> Okonomiyaki.update Okonomiyaki.DecrementNoodleQuantity
                        |> .noodleSelection
                        |> Expect.equal Okonomiyaki.WithoutNoodle
            ]
        , describe "noodleBadges"
            [ test "WithoutNoodle → 空リスト" <|
                \_ ->
                    Okonomiyaki.noodleBadges Okonomiyaki.WithoutNoodle
                        |> Expect.equal []
            , test "WithSoba (Whole 1) → そば 1玉" <|
                \_ ->
                    Okonomiyaki.noodleBadges (Okonomiyaki.WithSoba (Okonomiyaki.Whole 1))
                        |> List.map (\b -> ( b.name, b.quantityDisplay ))
                        |> Expect.equal [ ( "そば", "1" ) ]
            , test "WithUdon (WholeAndHalf 1) → うどん 1.5玉" <|
                \_ ->
                    Okonomiyaki.noodleBadges (Okonomiyaki.WithUdon (Okonomiyaki.WholeAndHalf 1))
                        |> List.map (\b -> ( b.name, b.quantityDisplay ))
                        |> Expect.equal [ ( "うどん", "1.5" ) ]
            , test "WithChampon 1 → そば0.5玉 + うどん0.5玉" <|
                \_ ->
                    Okonomiyaki.noodleBadges (Okonomiyaki.WithChampon 1)
                        |> List.map (\b -> ( b.name, b.quantityDisplay ))
                        |> Expect.equal
                            [ ( "そば", "0.5" )
                            , ( "うどん", "0.5" )
                            ]
            , test "WithChampon 2 → そば1玉 + うどん1玉" <|
                \_ ->
                    Okonomiyaki.noodleBadges (Okonomiyaki.WithChampon 2)
                        |> List.map (\b -> ( b.name, b.quantityDisplay ))
                        |> Expect.equal
                            [ ( "そば", "1" )
                            , ( "うどん", "1" )
                            ]
            , test "WithChampon 3 → そば1.5玉 + うどん1.5玉" <|
                \_ ->
                    Okonomiyaki.noodleBadges (Okonomiyaki.WithChampon 3)
                        |> List.map (\b -> ( b.name, b.quantityDisplay ))
                        |> Expect.equal
                            [ ( "そば", "1.5" )
                            , ( "うどん", "1.5" )
                            ]
            ]
        , describe "ToggleTopping"
            [ test "未選択なら quantity=1 で追加される" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.YasaiIri
                        |> Okonomiyaki.update (Okonomiyaki.ToggleTopping Okonomiyaki.toppingIkaten)
                        |> .toppings
                        |> List.map (\t -> ( t.topping.kind, t.quantity ))
                        |> Expect.equal [ ( Okonomiyaki.ToppingKindIkaten, 1 ) ]
            , test "選択済みなら削除される" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.YasaiIri
                        |> Okonomiyaki.update (Okonomiyaki.ToggleTopping Okonomiyaki.toppingIkaten)
                        |> Okonomiyaki.update (Okonomiyaki.ToggleTopping Okonomiyaki.toppingIkaten)
                        |> .toppings
                        |> Expect.equal []
            ]
        ]
