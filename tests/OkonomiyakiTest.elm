module OkonomiyakiTest exposing (suite)

import Expect
import Menu exposing (menuItemId)
import MenuData
import Okonomiyaki
import Test exposing (..)


suite : Test
suite =
    describe "Okonomiyaki"
        [ describe "isDefaultNoodleOf"
            [ describe "対応する麺は True"
                [ test "noodleSoba は baseSobaBase の込み麺" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleSoba Okonomiyaki.baseSobaBase
                            |> Expect.equal True
                , test "noodleUdon は baseUdonBase の込み麺" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleUdon Okonomiyaki.baseUdonBase
                            |> Expect.equal True
                ]
            , describe "対応しない麺は False"
                [ test "noodleSoba は baseUdonBase の込み麺ではない" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleSoba Okonomiyaki.baseUdonBase
                            |> Expect.equal False
                , test "noodleUdon は baseSobaBase の込み麺ではない" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleUdon Okonomiyaki.baseSobaBase
                            |> Expect.equal False
                , test "baseYasaiBase は込み麺を持たないので常に False" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleSoba Okonomiyaki.baseYasaiBase
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
        , describe "initialBaseOrderItem"
            [ describe "baseYasaiBase（込み麺なし）"
                [ test "noodles が空で生成される" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseYasaiBase
                            |> .noodles
                            |> Expect.equal []
                ]
            , describe "baseSobaBase（込み麺: そば）"
                [ test "noodles に noodleSoba が1玉（quantity=2）でセットされる" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseSobaBase
                            |> .noodles
                            |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, 2 ) ]
                ]
            , describe "baseUdonBase（込み麺: うどん）"
                [ test "noodles に noodleUdon が1玉（quantity=2）でセットされる" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseUdonBase
                            |> .noodles
                            |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.NoodleKindUdon, 2 ) ]
                ]
            , describe "baseZenbuIriBase（込み麺なし、初期トッピング: イカ・エビ）"
                [ test "noodles にそば1玉（quantity=2）がデフォルトでセットされる" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseZenbuIriBase
                            |> .noodles
                            |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.NoodleKindSoba, 2 ) ]
                , test "toppings にイカとエビが各1個でセットされる" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseZenbuIriBase
                            |> .toppings
                            |> List.map (\t -> ( menuItemId t.menuItem, t.quantity ))
                            |> Expect.equal [ ( "topping-squid", 1 ), ( "topping-shrimp", 1 ) ]
                ]
            , describe "共通の初期値"
                [ test "quantity は 1 で初期化される" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseYasaiBase
                            |> .quantity
                            |> Expect.equal 1
                , test "toppings は空で初期化される（込みトッピングなしのベース）" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseYasaiBase
                            |> .toppings
                            |> Expect.equal []
                ]
            ]
        , describe "normalizeBaseOnNoodleAdd"
            [ describe "Yasai に麺を追加するとベースが切り替わる"
                [ test "そばを追加すると Soba になる" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseYasaiBase
                            |> Okonomiyaki.normalizeBaseOnNoodleAdd Okonomiyaki.noodleSoba
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Soba
                , test "うどんを追加すると Udon になる" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseYasaiBase
                            |> Okonomiyaki.normalizeBaseOnNoodleAdd Okonomiyaki.noodleUdon
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Udon
                ]
            , describe "込み麺を持つベースは切り替わらない"
                [ test "Soba にそばを追加してもベースは変わらない" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseSobaBase
                            |> Okonomiyaki.normalizeBaseOnNoodleAdd Okonomiyaki.noodleSoba
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Soba
                , test "Udon にうどんを追加してもベースは変わらない" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseUdonBase
                            |> Okonomiyaki.normalizeBaseOnNoodleAdd Okonomiyaki.noodleUdon
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Udon
                ]
            ]
        , describe "normalizeBaseOnNoodleChange"
            [ describe "込み麺が0玉になると Yasai に切り替わる"
                [ test "Soba でそばが0玉になると Yasai になる" <|
                    \_ ->
                        { base = Okonomiyaki.baseSobaBase
                        , quantity = 1
                        , noodles = []
                        , toppings = []
                        }
                            |> Okonomiyaki.normalizeBaseOnNoodleChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Yasai
                , test "Udon でうどんが0玉になると Yasai になる" <|
                    \_ ->
                        { base = Okonomiyaki.baseUdonBase
                        , quantity = 1
                        , noodles = []
                        , toppings = []
                        }
                            |> Okonomiyaki.normalizeBaseOnNoodleChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Yasai
                ]
            , describe "込み麺が残っていればベースは変わらない"
                [ test "Soba でそばが残っていれば変わらない" <|
                    \_ ->
                        { base = Okonomiyaki.baseSobaBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ]
                        , toppings = []
                        }
                            |> Okonomiyaki.normalizeBaseOnNoodleChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Soba
                ]
            , describe "込み麺を持たないベースは変化しない"
                [ test "Yasai は変化しない" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseYasaiBase
                            |> Okonomiyaki.normalizeBaseOnNoodleChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Yasai
                ]
            ]
        , describe "calculateBaseItemTotal"
            [ describe "ベースのみ（麺・トッピングなし）"
                [ test "Yasai × 1 = 900円" <|
                    \_ ->
                        { base = Okonomiyaki.baseYasaiBase, quantity = 1, noodles = [], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 900
                , test "Yasai × 2 = 1800円" <|
                    \_ ->
                        { base = Okonomiyaki.baseYasaiBase, quantity = 2, noodles = [], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1800
                , test "Soba × 1 = 1200円（そば1玉分込み）" <|
                    \_ ->
                        { base = Okonomiyaki.baseSobaBase, quantity = 1, noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1200
                , test "Udon × 1 = 1200円（うどん1玉分込み）" <|
                    \_ ->
                        { base = Okonomiyaki.baseUdonBase, quantity = 1, noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = 2 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1200
                ]
            , describe "込み麺なしのベース（Yasai）に麺を追加"
                [ test "そば0.5玉追加（qty=1）: 900 + (100 + 100×1) = 1100円" <|
                    \_ ->
                        { base = Okonomiyaki.baseYasaiBase, quantity = 1, noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1100
                , test "そば1玉追加（qty=2）: 900 + (100 + 100×2) = 1200円" <|
                    \_ ->
                        { base = Okonomiyaki.baseYasaiBase, quantity = 1, noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1200
                , test "お好み焼き2枚 × そば0.5玉: (900 + (100 + 100×1)) × 2 = 2200円" <|
                    \_ ->
                        { base = Okonomiyaki.baseYasaiBase, quantity = 2, noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 2200
                ]
            , describe "込み麺ありのベース（Soba）に麺を追加・減量"
                [ test "そば0.5玉（qty=1）: 1200 - 100×1 = 1100円（1玉分は price 込みなので差引）" <|
                    \_ ->
                        { base = Okonomiyaki.baseSobaBase, quantity = 1, noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1100
                , test "そば1.5玉（qty=3）: 1200 + 100×1 = 1300円（1玉分は price 込み）" <|
                    \_ ->
                        { base = Okonomiyaki.baseSobaBase, quantity = 1, noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 3 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1300
                , test "そば2玉（qty=4）: 1200 + 100×2 = 1400円" <|
                    \_ ->
                        { base = Okonomiyaki.baseSobaBase, quantity = 1, noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 4 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1400
                ]
            , describe "トッピングあり"
                [ test "Yasai + イカ天×1: 900 + 200 = 1100円" <|
                    \_ ->
                        { base = Okonomiyaki.baseYasaiBase, quantity = 1, noodles = [], toppings = [ { menuItem = MenuData.toppingIkaten, quantity = 1 } ] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1100
                , test "Yasai + イカ天×1 + ねぎかけ×2: 900 + 200 + (250×2) = 1600円" <|
                    \_ ->
                        { base = Okonomiyaki.baseYasaiBase, quantity = 1, noodles = [], toppings = [ { menuItem = MenuData.toppingIkaten, quantity = 1 }, { menuItem = MenuData.toppingNegi, quantity = 2 } ] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1600
                ]
            , describe "麺 + トッピングの組み合わせ"
                [ test "Yasai + そば0.5玉 + イカ天 + ねぎかけ: 900 + (100+100) + 200 + 250 = 1550円" <|
                    \_ ->
                        { base = Okonomiyaki.baseYasaiBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ]
                        , toppings = [ { menuItem = MenuData.toppingIkaten, quantity = 1 }, { menuItem = MenuData.toppingNegi, quantity = 1 } ]
                        }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1550
                , test "Yasai×3 + そば0.5玉 + イカ天×2: (900 + (100+100) + 200×2) × 3 = 4500円" <|
                    \_ ->
                        { base = Okonomiyaki.baseYasaiBase
                        , quantity = 3
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ]
                        , toppings = [ { menuItem = MenuData.toppingIkaten, quantity = 2 } ]
                        }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 4500
                ]
            , describe "全部入り（ZenbuIri）の価格計算"
                [ test "ZenbuIri × 1（麺なし・イカ・エビ込み）= 1400円" <|
                    \_ ->
                        { base = Okonomiyaki.baseZenbuIriBase
                        , quantity = 1
                        , noodles = []
                        , toppings =
                            [ { menuItem = MenuData.toppingSquid, quantity = 1 }
                            , { menuItem = MenuData.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1400
                , test "ZenbuIri × 1（そば1玉・イカ・エビ込み）= 1700円" <|
                    \_ ->
                        { base = Okonomiyaki.baseZenbuIriBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings =
                            [ { menuItem = MenuData.toppingSquid, quantity = 1 }
                            , { menuItem = MenuData.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1700
                , test "ZenbuIri × 1（うどん1玉・イカ・エビ込み）= 1700円" <|
                    \_ ->
                        { base = Okonomiyaki.baseZenbuIriBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = 2 } ]
                        , toppings =
                            [ { menuItem = MenuData.toppingSquid, quantity = 1 }
                            , { menuItem = MenuData.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1700
                , test "ZenbuIri × 1（そば0.5玉・イカ・エビ込み）= 1600円" <|
                    \_ ->
                        { base = Okonomiyaki.baseZenbuIriBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 1 } ]
                        , toppings =
                            [ { menuItem = MenuData.toppingSquid, quantity = 1 }
                            , { menuItem = MenuData.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1600
                , test "ZenbuIri × 2（そば1玉・イカ・エビ込み）= 3400円" <|
                    \_ ->
                        { base = Okonomiyaki.baseZenbuIriBase
                        , quantity = 2
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings =
                            [ { menuItem = MenuData.toppingSquid, quantity = 1 }
                            , { menuItem = MenuData.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 3400
                ]
            ]
        , describe "normalizeBaseOnNoodleChange（ZenbuIri）"
            [ test "ZenbuIri は麺が0玉になっても ZenbuIri のまま変化しない" <|
                \_ ->
                    { base = Okonomiyaki.baseZenbuIriBase
                    , quantity = 1
                    , noodles = []
                    , toppings =
                        [ { menuItem = MenuData.toppingSquid, quantity = 1 }
                        , { menuItem = MenuData.toppingShrimp, quantity = 1 }
                        ]
                    }
                        |> Okonomiyaki.normalizeBaseOnNoodleChange
                        |> .base
                        |> .kind
                        |> Expect.equal Okonomiyaki.ZenbuIri
            ]
        , describe "normalizeBaseOnToppingChange"
            [ describe "イカとエビが両方あると全部入りに切り替わる"
                [ test "Soba + イカ + エビ → ZenbuIri" <|
                    \_ ->
                        { base = Okonomiyaki.baseSobaBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings =
                            [ { menuItem = MenuData.toppingSquid, quantity = 1 }
                            , { menuItem = MenuData.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.normalizeBaseOnToppingChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "Udon + イカ + エビ → ZenbuIri" <|
                    \_ ->
                        { base = Okonomiyaki.baseUdonBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = 2 } ]
                        , toppings =
                            [ { menuItem = MenuData.toppingSquid, quantity = 1 }
                            , { menuItem = MenuData.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.normalizeBaseOnToppingChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                , test "Yasai + イカ + エビ → ZenbuIri" <|
                    \_ ->
                        { base = Okonomiyaki.baseYasaiBase
                        , quantity = 1
                        , noodles = []
                        , toppings =
                            [ { menuItem = MenuData.toppingSquid, quantity = 1 }
                            , { menuItem = MenuData.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.normalizeBaseOnToppingChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                ]
            , describe "全部入りからイカまたはエビを削除すると元のベースに戻る"
                [ test "ZenbuIri + エビのみ（そば残り）→ Soba" <|
                    \_ ->
                        { base = Okonomiyaki.baseZenbuIriBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings = [ { menuItem = MenuData.toppingShrimp, quantity = 1 } ]
                        }
                            |> Okonomiyaki.normalizeBaseOnToppingChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Soba
                , test "ZenbuIri + イカのみ（うどん残り）→ Udon" <|
                    \_ ->
                        { base = Okonomiyaki.baseZenbuIriBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = 2 } ]
                        , toppings = [ { menuItem = MenuData.toppingSquid, quantity = 1 } ]
                        }
                            |> Okonomiyaki.normalizeBaseOnToppingChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Udon
                , test "ZenbuIri + イカのみ（麺なし）→ Yasai" <|
                    \_ ->
                        { base = Okonomiyaki.baseZenbuIriBase
                        , quantity = 1
                        , noodles = []
                        , toppings = [ { menuItem = MenuData.toppingSquid, quantity = 1 } ]
                        }
                            |> Okonomiyaki.normalizeBaseOnToppingChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Yasai
                ]
            , describe "全部入りのまま変化しないケース"
                [ test "ZenbuIri + イカ + エビ両方あれば変化しない" <|
                    \_ ->
                        { base = Okonomiyaki.baseZenbuIriBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings =
                            [ { menuItem = MenuData.toppingSquid, quantity = 1 }
                            , { menuItem = MenuData.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.normalizeBaseOnToppingChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.ZenbuIri
                ]
            , describe "麺はそのまま引き継がれる"
                [ test "Udon + イカ + エビ → ZenbuIri だが noodles はうどんのまま" <|
                    \_ ->
                        { base = Okonomiyaki.baseUdonBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleUdon, quantity = 2 } ]
                        , toppings =
                            [ { menuItem = MenuData.toppingSquid, quantity = 1 }
                            , { menuItem = MenuData.toppingShrimp, quantity = 1 }
                            ]
                        }
                            |> Okonomiyaki.normalizeBaseOnToppingChange
                            |> .noodles
                            |> List.map (\n -> ( n.noodle.kind, n.quantity ))
                            |> Expect.equal [ ( Okonomiyaki.NoodleKindUdon, 2 ) ]
                ]
            , describe "イカのみ・エビのみでは切り替わらない"
                [ test "Soba + イカのみ → 変化なし" <|
                    \_ ->
                        { base = Okonomiyaki.baseSobaBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings = [ { menuItem = MenuData.toppingSquid, quantity = 1 } ]
                        }
                            |> Okonomiyaki.normalizeBaseOnToppingChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Soba
                , test "Soba + エビのみ → 変化なし" <|
                    \_ ->
                        { base = Okonomiyaki.baseSobaBase
                        , quantity = 1
                        , noodles = [ { noodle = Okonomiyaki.noodleSoba, quantity = 2 } ]
                        , toppings = [ { menuItem = MenuData.toppingShrimp, quantity = 1 } ]
                        }
                            |> Okonomiyaki.normalizeBaseOnToppingChange
                            |> .base
                            |> .kind
                            |> Expect.equal Okonomiyaki.Soba
                ]
            ]
        ]
