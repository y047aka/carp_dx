module OkonomiyakiTest exposing (suite)

import Expect
import Menu exposing (menuItemId)
import MenuData
import Okonomiyaki
import Test exposing (..)


suite : Test
suite =
    describe "Okonomiyaki"
        [ describe "baseForNoodle"
            [ describe "麺IDに対応するベースを返す"
                [ test "noodle-soba → baseSoba" <|
                    \_ ->
                        Okonomiyaki.baseForNoodle "noodle-soba"
                            |> Maybe.map menuItemId
                            |> Expect.equal (Just "base-soba")
                , test "noodle-udon → baseUdon" <|
                    \_ ->
                        Okonomiyaki.baseForNoodle "noodle-udon"
                            |> Maybe.map menuItemId
                            |> Expect.equal (Just "base-udon")
                ]
            , describe "対応するベースがない場合は Nothing"
                [ test "不明なIDは Nothing" <|
                    \_ ->
                        Okonomiyaki.baseForNoodle "unknown"
                            |> Expect.equal Nothing
                , test "ベースのIDを渡しても Nothing" <|
                    \_ ->
                        Okonomiyaki.baseForNoodle "base-yasai"
                            |> Expect.equal Nothing
                ]
            ]
        , describe "isDefaultNoodleOf"
            [ describe "対応する麺は True"
                [ test "noodleSoba は baseSoba の defaultNoodle" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleSoba Okonomiyaki.baseSoba
                            |> Expect.equal True
                , test "noodleUdon は baseUdon の defaultNoodle" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleUdon Okonomiyaki.baseUdon
                            |> Expect.equal True
                ]
            , describe "対応しない麺は False"
                [ test "noodleSoba は baseUdon の defaultNoodle ではない" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleSoba Okonomiyaki.baseUdon
                            |> Expect.equal False
                , test "noodleUdon は baseSoba の defaultNoodle ではない" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleUdon Okonomiyaki.baseSoba
                            |> Expect.equal False
                , test "baseYasai は defaultNoodle を持たないので常に False" <|
                    \_ ->
                        Okonomiyaki.isDefaultNoodleOf Okonomiyaki.noodleSoba Okonomiyaki.baseYasai
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
            [ describe "baseYasai（defaultNoodle なし）"
                [ test "noodles が空で生成される" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseYasai
                            |> .noodles
                            |> Expect.equal []
                ]
            , describe "baseSoba（defaultNoodle: noodleSoba）"
                [ test "noodles に noodleSoba が1玉（quantity=2）でセットされる" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseSoba
                            |> .noodles
                            |> List.map (\n -> ( menuItemId n.menuItem, n.quantity ))
                            |> Expect.equal [ ( "noodle-soba", 2 ) ]
                ]
            , describe "baseUdon（defaultNoodle: noodleUdon）"
                [ test "noodles に noodleUdon が1玉（quantity=2）でセットされる" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseUdon
                            |> .noodles
                            |> List.map (\n -> ( menuItemId n.menuItem, n.quantity ))
                            |> Expect.equal [ ( "noodle-udon", 2 ) ]
                ]
            , describe "共通の初期値"
                [ test "quantity は 1 で初期化される" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseYasai
                            |> .quantity
                            |> Expect.equal 1
                , test "toppings は空で初期化される" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseSoba
                            |> .toppings
                            |> Expect.equal []
                ]
            ]
        , describe "normalizeBaseOnNoodleAdd"
            [ describe "baseYasai に麺を追加するとベースが切り替わる"
                [ test "そばを追加すると baseSoba になる" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseYasai
                            |> Okonomiyaki.normalizeBaseOnNoodleAdd Okonomiyaki.noodleSoba
                            |> .baseItem
                            |> menuItemId
                            |> Expect.equal "base-soba"
                , test "うどんを追加すると baseUdon になる" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseYasai
                            |> Okonomiyaki.normalizeBaseOnNoodleAdd Okonomiyaki.noodleUdon
                            |> .baseItem
                            |> menuItemId
                            |> Expect.equal "base-udon"
                ]
            , describe "defaultNoodle を持つベースは切り替わらない"
                [ test "baseSoba にそばを追加してもベースは変わらない" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseSoba
                            |> Okonomiyaki.normalizeBaseOnNoodleAdd Okonomiyaki.noodleSoba
                            |> .baseItem
                            |> menuItemId
                            |> Expect.equal "base-soba"
                , test "baseUdon にうどんを追加してもベースは変わらない" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseUdon
                            |> Okonomiyaki.normalizeBaseOnNoodleAdd Okonomiyaki.noodleUdon
                            |> .baseItem
                            |> menuItemId
                            |> Expect.equal "base-udon"
                ]
            ]
        , describe "normalizeBaseOnNoodleChange"
            [ describe "defaultNoodle が0玉になるとbaseYasaiに切り替わる"
                [ test "baseSoba でそばが0玉になると baseYasai になる" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseSoba
                        , quantity = 1
                        , noodles = []
                        , toppings = []
                        }
                            |> Okonomiyaki.normalizeBaseOnNoodleChange
                            |> .baseItem
                            |> menuItemId
                            |> Expect.equal "base-yasai"
                , test "baseUdon でうどんが0玉になると baseYasai になる" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseUdon
                        , quantity = 1
                        , noodles = []
                        , toppings = []
                        }
                            |> Okonomiyaki.normalizeBaseOnNoodleChange
                            |> .baseItem
                            |> menuItemId
                            |> Expect.equal "base-yasai"
                ]
            , describe "defaultNoodle が残っていればベースは変わらない"
                [ test "baseSoba でそばが残っていれば変わらない" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseSoba
                        , quantity = 1
                        , noodles = [ { menuItem = Okonomiyaki.noodleSoba, quantity = 1 } ]
                        , toppings = []
                        }
                            |> Okonomiyaki.normalizeBaseOnNoodleChange
                            |> .baseItem
                            |> menuItemId
                            |> Expect.equal "base-soba"
                ]
            , describe "defaultNoodle を持たないベースは変化しない"
                [ test "baseYasai は変化しない" <|
                    \_ ->
                        Okonomiyaki.initialBaseOrderItem Okonomiyaki.baseYasai
                            |> Okonomiyaki.normalizeBaseOnNoodleChange
                            |> .baseItem
                            |> menuItemId
                            |> Expect.equal "base-yasai"
                ]
            ]
        , describe "calculateBaseItemTotal"
            [ describe "ベースのみ（麺・トッピングなし）"
                [ test "baseYasai × 1 = 900円" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseYasai, quantity = 1, noodles = [], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 900
                , test "baseYasai × 2 = 1800円" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseYasai, quantity = 2, noodles = [], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1800
                , test "baseSoba × 1 = 1200円（そば1玉分込み）" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseSoba, quantity = 1, noodles = [ { menuItem = Okonomiyaki.noodleSoba, quantity = 2 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1200
                , test "baseUdon × 1 = 1200円（うどん1玉分込み）" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseUdon, quantity = 1, noodles = [ { menuItem = Okonomiyaki.noodleUdon, quantity = 2 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1200
                ]
            , describe "defaultNoodle なしのベース（baseYasai）に麺を追加"
                [ test "そば0.5玉追加（qty=1）: 900 + (100 + 100×1) = 1100円" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseYasai, quantity = 1, noodles = [ { menuItem = Okonomiyaki.noodleSoba, quantity = 1 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1100
                , test "そば1玉追加（qty=2）: 900 + (100 + 100×2) = 1200円" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseYasai, quantity = 1, noodles = [ { menuItem = Okonomiyaki.noodleSoba, quantity = 2 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1200
                , test "お好み焼き2枚 × そば0.5玉: (900 + (100 + 100×1)) × 2 = 2200円" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseYasai, quantity = 2, noodles = [ { menuItem = Okonomiyaki.noodleSoba, quantity = 1 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 2200
                ]
            , describe "defaultNoodle ありのベース（baseSoba）に麺を追加"
                [ test "そば1.5玉（qty=3）: 1200 + 100×1 = 1300円（1玉分は price 込み）" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseSoba, quantity = 1, noodles = [ { menuItem = Okonomiyaki.noodleSoba, quantity = 3 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1300
                , test "そば2玉（qty=4）: 1200 + 100×2 = 1400円" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseSoba, quantity = 1, noodles = [ { menuItem = Okonomiyaki.noodleSoba, quantity = 4 } ], toppings = [] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1400
                ]
            , describe "トッピングあり"
                [ test "baseYasai + イカ天×1: 900 + 200 = 1100円" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseYasai, quantity = 1, noodles = [], toppings = [ { menuItem = MenuData.toppingIkaten, quantity = 1 } ] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1100
                , test "baseYasai + イカ天×1 + ねぎかけ×2: 900 + 200 + (250×2) = 1600円" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseYasai, quantity = 1, noodles = [], toppings = [ { menuItem = MenuData.toppingIkaten, quantity = 1 }, { menuItem = MenuData.toppingNegi, quantity = 2 } ] }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1600
                ]
            , describe "麺 + トッピングの組み合わせ"
                [ test "baseYasai + そば0.5玉 + イカ天 + ねぎかけ: 900 + (100+100) + 200 + 250 = 1550円" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseYasai
                        , quantity = 1
                        , noodles = [ { menuItem = Okonomiyaki.noodleSoba, quantity = 1 } ]
                        , toppings = [ { menuItem = MenuData.toppingIkaten, quantity = 1 }, { menuItem = MenuData.toppingNegi, quantity = 1 } ]
                        }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 1550
                , test "baseYasai×3 + そば0.5玉 + イカ天×2: (900 + (100+100) + 200×2) × 3 = 4500円" <|
                    \_ ->
                        { baseItem = Okonomiyaki.baseYasai
                        , quantity = 3
                        , noodles = [ { menuItem = Okonomiyaki.noodleSoba, quantity = 1 } ]
                        , toppings = [ { menuItem = MenuData.toppingIkaten, quantity = 2 } ]
                        }
                            |> Okonomiyaki.calculateBaseItemTotal
                            |> Expect.equal 4500
                ]
            ]
        ]
