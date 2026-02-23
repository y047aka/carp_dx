module OkonomiyakiTest exposing (suite)

import Expect
import Okonomiyaki
import Test exposing (..)


suite : Test
suite =
    describe "Okonomiyaki"
        [ describe "noodleQuantityDisplay"
            [ test "HalfBall → \"0.5\"" <|
                \_ -> Okonomiyaki.noodleQuantityDisplay Okonomiyaki.HalfBall |> Expect.equal "0.5"
            , test "Whole 1 → \"1\"" <|
                \_ -> Okonomiyaki.noodleQuantityDisplay (Okonomiyaki.Whole 1) |> Expect.equal "1"
            , test "WholeAndHalf 1 → \"1.5\"" <|
                \_ -> Okonomiyaki.noodleQuantityDisplay (Okonomiyaki.WholeAndHalf 1) |> Expect.equal "1.5"
            , test "Whole 2 → \"2\"" <|
                \_ -> Okonomiyaki.noodleQuantityDisplay (Okonomiyaki.Whole 2) |> Expect.equal "2"
            ]
        , describe "init"
            [ describe "baseYasaiBase（込み麺なし）"
                [ test "noodles が空で生成される" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.Yasai
                            |> .noodles
                            |> Expect.equal []
                ]
            , describe "baseSobaBase（込み麺: そば）"
                [ test "noodles に noodleSoba が1玉（Whole 1）でセットされる" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.Soba
                            |> .noodles
                            |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, Okonomiyaki.Whole 1 ) ]
                ]
            , describe "baseUdonBase（込み麺: うどん）"
                [ test "noodles に noodleUdon が1玉（Whole 1）でセットされる" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.Udon
                            |> .noodles
                            |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.NoodleKindUdon, Okonomiyaki.Whole 1 ) ]
                ]
            , describe "baseZenbuIriBase（込み麺なし、初期トッピング: イカ・エビ）"
                [ test "noodles にそば1玉（Whole 1）がデフォルトでセットされる" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.ZenbuIri
                            |> .noodles
                            |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, Okonomiyaki.Whole 1 ) ]
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
                        Okonomiyaki.init Okonomiyaki.Yasai
                            |> .toppings
                            |> Expect.equal []
                ]
            ]
        , describe "baseKind"
            [ describe "麺の状態に応じたベース決定"
                [ test "そばがあれば Soba になる" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.Whole 1 } ]
                        , toppings = []
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.Soba
                , test "うどんがあれば Udon になる" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = Okonomiyaki.Whole 1 } ]
                        , toppings = []
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.Udon
                , test "麺がなければ Yasai になる" <|
                    \_ ->
                        { noodles = []
                        , toppings = []
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.Yasai
                , test "そばが残っていれば Soba になる" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.HalfBall } ]
                        , toppings = []
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.Soba
                , test "そばとうどんが両方あればそば優先で Soba になる" <|
                    \_ ->
                        { noodles =
                            [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.Whole 1 }
                            , { noodle = Okonomiyaki.noodleUdon, quantity = Okonomiyaki.Whole 1 }
                            ]
                        , toppings = []
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.Soba
                ]
            , describe "イカとエビが両方あると ZenbuIri になる"
                [ test "Soba + イカ + エビ → ZenbuIri" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.Whole 1 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "Udon + イカ + エビ → ZenbuIri" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = Okonomiyaki.Whole 1 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "Yasai + イカ + エビ → ZenbuIri" <|
                    \_ ->
                        { noodles = []
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "イカ + エビのみ（麺なし）→ ZenbuIri" <|
                    \_ ->
                        { noodles = []
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "イカ + エビ（そば有り）→ ZenbuIri" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.Whole 1 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                ]
            , describe "全部入り条件が崩れると麺に応じたベースに戻る"
                [ test "エビのみ（そば残り）→ Soba" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.Whole 1 } ]
                        , toppings = [ { topping = Okonomiyaki.toppingShrimp, quantity = 1 } ]
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.Soba
                , test "イカのみ（うどん残り）→ Udon" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = Okonomiyaki.Whole 1 } ]
                        , toppings = [ { topping = Okonomiyaki.toppingSquid, quantity = 1 } ]
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.Udon
                , test "イカのみ（麺なし）→ Yasai" <|
                    \_ ->
                        { noodles = []
                        , toppings = [ { topping = Okonomiyaki.toppingSquid, quantity = 1 } ]
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.Yasai
                ]
            , describe "イカのみ・エビのみでは ZenbuIri にならない"
                [ test "Soba + イカのみ → Soba" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.Whole 1 } ]
                        , toppings = [ { topping = Okonomiyaki.toppingSquid, quantity = 1 } ]
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.Soba
                , test "Soba + エビのみ → Soba" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.Whole 1 } ]
                        , toppings = [ { topping = Okonomiyaki.toppingShrimp, quantity = 1 } ]
                        }
                            |> Okonomiyaki.baseKind
                            |> Expect.equal Okonomiyaki.Soba
                ]
            ]
        , describe "calculateTotal"
            [ describe "麺なし"
                [ test "麺なし = 900円" <|
                    \_ ->
                        { noodles = [], toppings = [] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 900
                ]
            , describe "麺あり: 900 + 入場料(100) + 半玉単価(100)×半玉数"
                [ test "そば0.5玉（HalfBall）: 900 + (100 + 100×1) = 1100円" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.HalfBall } ], toppings = [] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1100
                , test "そば1玉（Whole 1）: 900 + (100 + 100×2) = 1200円" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.Whole 1 } ], toppings = [] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1200
                , test "そば1.5玉（WholeAndHalf 1）: 900 + (100 + 100×3) = 1300円" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.WholeAndHalf 1 } ], toppings = [] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1300
                , test "そば2玉（Whole 2）: 900 + (100 + 100×4) = 1400円" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.Whole 2 } ], toppings = [] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1400
                ]
            , describe "トッピングあり"
                [ test "イカ天×1: 900 + 200 = 1100円" <|
                    \_ ->
                        { noodles = [], toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 1 } ] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1100
                , test "イカ天×1 + ねぎかけ×2: 900 + 200 + (250×2) = 1600円" <|
                    \_ ->
                        { noodles = [], toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 1 }, { topping = Okonomiyaki.toppingNegi, quantity = 2 } ] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1600
                ]
            , describe "麺 + トッピングの組み合わせ"
                [ test "そば0.5玉 + イカ天 + ねぎかけ: 900 + (100+100) + 200 + 250 = 1550円" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.HalfBall } ]
                        , toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 1 }, { topping = Okonomiyaki.toppingNegi, quantity = 1 } ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1550
                , test "そば0.5玉 + イカ天×2: 900 + (100+100) + 200×2 = 1500円" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.HalfBall } ]
                        , toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 2 } ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1500
                ]
            , describe "全部入り（ZenbuIri）の価格計算"
                [ test "イカ + エビ（麺なし）= 1400円" <|
                    \_ ->
                        { noodles = []
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1400
                , test "そば1玉 + イカ + エビ = 1700円" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.Whole 1 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1700
                , test "うどん1玉 + イカ + エビ = 1700円" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = Okonomiyaki.Whole 1 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1700
                , test "そば0.5玉 + イカ + エビ = 1600円" <|
                    \_ ->
                        { noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = Okonomiyaki.HalfBall } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1600
                ]
            ]
        , describe "incrementNoodle"
            [ test "新規麺は1玉（Whole 1）で追加される" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Yasai
                        |> Okonomiyaki.incrementNoodle Okonomiyaki.noodleSoba
                        |> .noodles
                        |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                        |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, Okonomiyaki.Whole 1 ) ]
            , test "既存麺は半玉増える（Whole 1 → WholeAndHalf 1）" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Soba
                        |> Okonomiyaki.incrementNoodle Okonomiyaki.noodleSoba
                        |> .noodles
                        |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                        |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, Okonomiyaki.WholeAndHalf 1 ) ]
            ]
        , describe "decrementNoodle"
            [ test "半玉減らす（Whole 1 → HalfBall）" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Soba
                        |> Okonomiyaki.decrementNoodle Okonomiyaki.noodleSoba
                        |> .noodles
                        |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                        |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, Okonomiyaki.HalfBall ) ]
            , test "0になったら削除される" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Soba
                        |> Okonomiyaki.decrementNoodle Okonomiyaki.noodleSoba
                        |> Okonomiyaki.decrementNoodle Okonomiyaki.noodleSoba
                        |> .noodles
                        |> Expect.equal []
            ]
        , describe "addTopping"
            [ test "新規トッピングは quantity=1 で追加される" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Yasai
                        |> Okonomiyaki.addTopping Okonomiyaki.toppingIkaten
                        |> .toppings
                        |> List.map (\t -> ( t.topping.kind, t.quantity ))
                        |> Expect.equal [ ( Okonomiyaki.ToppingKindIkaten, 1 ) ]
            , test "既存トッピングは quantity+1 される" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Yasai
                        |> Okonomiyaki.addTopping Okonomiyaki.toppingIkaten
                        |> Okonomiyaki.addTopping Okonomiyaki.toppingIkaten
                        |> .toppings
                        |> List.map (\t -> ( t.topping.kind, t.quantity ))
                        |> Expect.equal [ ( Okonomiyaki.ToppingKindIkaten, 2 ) ]
            ]
        , describe "removeTopping"
            [ test "指定種類のトッピングが削除される" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Yasai
                        |> Okonomiyaki.addTopping Okonomiyaki.toppingIkaten
                        |> Okonomiyaki.removeTopping Okonomiyaki.ToppingKindIkaten
                        |> .toppings
                        |> Expect.equal []
            ]
        , describe "toggleTopping"
            [ test "未選択なら追加される" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Yasai
                        |> Okonomiyaki.toggleTopping Okonomiyaki.toppingIkaten
                        |> .toppings
                        |> List.map (\t -> t.topping.kind)
                        |> Expect.equal [ Okonomiyaki.ToppingKindIkaten ]
            , test "選択済みなら削除される" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Yasai
                        |> Okonomiyaki.toggleTopping Okonomiyaki.toppingIkaten
                        |> Okonomiyaki.toggleTopping Okonomiyaki.toppingIkaten
                        |> .toppings
                        |> Expect.equal []
            ]
        ]
