module OkonomiyakiTest exposing (suite)

import Expect
import Okonomiyaki
import Test exposing (..)


suite : Test
suite =
    describe "Okonomiyaki"
        [ describe "isDefaultNoodleOf"
            [ describe "対応する麺は True"
                [ test "noodleSoba は Soba の込み麺" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleSoba Okonomiyaki.Soba
                            |> Expect.equal True
                , test "noodleUdon は Udon の込み麺" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleUdon Okonomiyaki.Udon
                            |> Expect.equal True
                ]
            , describe "対応しない麺は False"
                [ test "noodleSoba は Udon の込み麺ではない" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleSoba Okonomiyaki.Udon
                            |> Expect.equal False
                , test "noodleUdon は Soba の込み麺ではない" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleUdon Okonomiyaki.Soba
                            |> Expect.equal False
                , test "Yasai は込み麺を持たないので常に False" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleSoba Okonomiyaki.Yasai
                            |> Expect.equal False
                ]
            ]
        , describe "noodleQuantityDisplay"
            [ test "1 → \"0.5\"" <|
                \_ -> Okonomiyaki.noodleQuantityDisplay 1 |> Expect.equal "0.5"
            , test "2 → \"1\"" <|
                \_ -> Okonomiyaki.noodleQuantityDisplay 2 |> Expect.equal "1"
            , test "3 → \"1.5\"" <|
                \_ -> Okonomiyaki.noodleQuantityDisplay 3 |> Expect.equal "1.5"
            , test "4 → \"2\"" <|
                \_ -> Okonomiyaki.noodleQuantityDisplay 4 |> Expect.equal "2"
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
                [ test "noodles に noodleSoba が1玉（quantity=2）でセットされる" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.Soba
                            |> .noodles
                            |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, 2 ) ]
                ]
            , describe "baseUdonBase（込み麺: うどん）"
                [ test "noodles に noodleUdon が1玉（quantity=2）でセットされる" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.Udon
                            |> .noodles
                            |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.NoodleKindUdon, 2 ) ]
                ]
            , describe "baseZenbuIriBase（込み麺なし、初期トッピング: イカ・エビ）"
                [ test "noodles にそば1玉（quantity=2）がデフォルトでセットされる" <|
                    \_ ->
                        Okonomiyaki.init Okonomiyaki.ZenbuIri
                            |> .noodles
                            |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, 2 ) ]
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
        , describe "normalizeBase"
            [ describe "麺の状態に応じたベース決定"
                [ test "そばがあれば Soba になる" <|
                    \_ ->
                        { base = Okonomiyaki.Yasai
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings = []
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.Soba
                , test "うどんがあれば Udon になる" <|
                    \_ ->
                        { base = Okonomiyaki.Yasai
                        , noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = 2 } ]
                        , toppings = []
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.Udon
                , test "麺がなければ Yasai になる" <|
                    \_ ->
                        { base = Okonomiyaki.Soba
                        , noodles = []
                        , toppings = []
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.Yasai
                , test "そばが残っていれば Soba のまま" <|
                    \_ ->
                        { base = Okonomiyaki.Soba
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ]
                        , toppings = []
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.Soba
                , test "そばとうどんが両方あればそば優先で Soba になる" <|
                    \_ ->
                        { base = Okonomiyaki.Yasai
                        , noodles =
                            [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 }
                            , { noodle = Okonomiyaki.noodleUdon, quantity = 2 }
                            ]
                        , toppings = []
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.Soba
                ]
            , describe "イカとエビが両方あると ZenbuIri に切り替わる"
                [ test "Soba + イカ + エビ → ZenbuIri" <|
                    \_ ->
                        { base = Okonomiyaki.Soba
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "Udon + イカ + エビ → ZenbuIri" <|
                    \_ ->
                        { base = Okonomiyaki.Udon
                        , noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = 2 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "Yasai + イカ + エビ → ZenbuIri" <|
                    \_ ->
                        { base = Okonomiyaki.Yasai
                        , noodles = []
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "ZenbuIri + イカ + エビ（麺なし）→ ZenbuIri のまま" <|
                    \_ ->
                        { base = Okonomiyaki.ZenbuIri
                        , noodles = []
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "ZenbuIri + イカ + エビ（そば有り）→ ZenbuIri のまま" <|
                    \_ ->
                        { base = Okonomiyaki.ZenbuIri
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.ZenbuIri
                ]
            , describe "全部入り条件が崩れると麺に応じたベースに戻る"
                [ test "ZenbuIri + エビのみ（そば残り）→ Soba" <|
                    \_ ->
                        { base = Okonomiyaki.ZenbuIri
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings = [ { topping = Okonomiyaki.toppingShrimp, quantity = 1 } ]
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.Soba
                , test "ZenbuIri + イカのみ（うどん残り）→ Udon" <|
                    \_ ->
                        { base = Okonomiyaki.ZenbuIri
                        , noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = 2 } ]
                        , toppings = [ { topping = Okonomiyaki.toppingSquid, quantity = 1 } ]
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.Udon
                , test "ZenbuIri + イカのみ（麺なし）→ Yasai" <|
                    \_ ->
                        { base = Okonomiyaki.ZenbuIri
                        , noodles = []
                        , toppings = [ { topping = Okonomiyaki.toppingSquid, quantity = 1 } ]
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.Yasai
                ]
            , describe "麺はそのまま引き継がれる"
                [ test "Udon + イカ + エビ → ZenbuIri だが noodles はうどんのまま" <|
                    \_ ->
                        { base = Okonomiyaki.Udon
                        , noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = 2 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .noodles
                            |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.NoodleKindUdon, 2 ) ]
                ]
            , describe "イカのみ・エビのみでは ZenbuIri にならない"
                [ test "Soba + イカのみ → Soba のまま" <|
                    \_ ->
                        { base = Okonomiyaki.Soba
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings = [ { topping = Okonomiyaki.toppingSquid, quantity = 1 } ]
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.Soba
                , test "Soba + エビのみ → Soba のまま" <|
                    \_ ->
                        { base = Okonomiyaki.Soba
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings = [ { topping = Okonomiyaki.toppingShrimp, quantity = 1 } ]
                        }
                            |> Okonomiyaki.normalizeBase
                            |> .base
                            |> Expect.equal Okonomiyaki.Soba
                ]
            ]
        , describe "calculateTotal"
            [ describe "麺なし"
                [ test "Yasai（麺なし）= 900円" <|
                    \_ ->
                        { base = Okonomiyaki.Yasai, noodles = [], toppings = [] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 900
                ]
            , describe "麺あり: 900 + 入場料(100) + 半玉単価(100)×quantity"
                [ test "そば0.5玉（qty=1）: 900 + (100 + 100×1) = 1100円" <|
                    \_ ->
                        { base = Okonomiyaki.Yasai, noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ], toppings = [] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1100
                , test "そば1玉（qty=2）: 900 + (100 + 100×2) = 1200円" <|
                    \_ ->
                        { base = Okonomiyaki.Yasai, noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ], toppings = [] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1200
                , test "そば1.5玉（qty=3）: 900 + (100 + 100×3) = 1300円" <|
                    \_ ->
                        { base = Okonomiyaki.Soba, noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 3 } ], toppings = [] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1300
                , test "そば2玉（qty=4）: 900 + (100 + 100×4) = 1400円" <|
                    \_ ->
                        { base = Okonomiyaki.Soba, noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 4 } ], toppings = [] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1400
                ]
            , describe "トッピングあり"
                [ test "Yasai + イカ天×1: 900 + 200 = 1100円" <|
                    \_ ->
                        { base = Okonomiyaki.Yasai, noodles = [], toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 1 } ] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1100
                , test "Yasai + イカ天×1 + ねぎかけ×2: 900 + 200 + (250×2) = 1600円" <|
                    \_ ->
                        { base = Okonomiyaki.Yasai, noodles = [], toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 1 }, { topping = Okonomiyaki.toppingNegi, quantity = 2 } ] }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1600
                ]
            , describe "麺 + トッピングの組み合わせ"
                [ test "Yasai + そば0.5玉 + イカ天 + ねぎかけ: 900 + (100+100) + 200 + 250 = 1550円" <|
                    \_ ->
                        { base = Okonomiyaki.Yasai
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ]
                        , toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 1 }, { topping = Okonomiyaki.toppingNegi, quantity = 1 } ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1550
                , test "Yasai + そば0.5玉 + イカ天×2: 900 + (100+100) + 200×2 = 1500円" <|
                    \_ ->
                        { base = Okonomiyaki.Yasai
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ]
                        , toppings = [ { topping = Okonomiyaki.toppingIkaten, quantity = 2 } ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1500
                ]
            , describe "全部入り（ZenbuIri）の価格計算"
                [ test "ZenbuIri（麺なし・イカ・エビ込み）= 1400円" <|
                    \_ ->
                        { base = Okonomiyaki.ZenbuIri
                        , noodles = []
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1400
                , test "ZenbuIri（そば1玉・イカ・エビ込み）= 1700円" <|
                    \_ ->
                        { base = Okonomiyaki.ZenbuIri
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1700
                , test "ZenbuIri（うどん1玉・イカ・エビ込み）= 1700円" <|
                    \_ ->
                        { base = Okonomiyaki.ZenbuIri
                        , noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = 2 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1700
                , test "ZenbuIri（そば0.5玉・イカ・エビ込み）= 1600円" <|
                    \_ ->
                        { base = Okonomiyaki.ZenbuIri
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ]
                        , toppings =
                            [ { topping = Okonomiyaki.toppingSquid, quantity = 1 }
                            , { topping = Okonomiyaki.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateTotal
                            |> Expect.equal 1600
                ]
            ]
        , describe "addNoodle"
            [ test "新規麺は1玉（quantity=2）で追加される" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Yasai
                        |> Okonomiyaki.addNoodle Okonomiyaki.noodleSoba
                        |> .noodles
                        |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                        |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, 2 ) ]
            , test "既存麺は半玉（quantity+1）追加される" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Soba
                        |> Okonomiyaki.addNoodle Okonomiyaki.noodleSoba
                        |> .noodles
                        |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                        |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, 3 ) ]
            ]
        , describe "decrementNoodle"
            [ test "半玉減らす（quantity-1）" <|
                \_ ->
                    Okonomiyaki.init Okonomiyaki.Soba
                        |> Okonomiyaki.decrementNoodle Okonomiyaki.noodleSoba
                        |> .noodles
                        |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                        |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, 1 ) ]
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
