module OrderTest exposing (suite)

import Expect
import MenuData
import Order exposing (Addition, BaseOrderItem, Order, OrderItemType(..), StandaloneOrderItem)
import Test exposing (..)


suite : Test
suite =
    describe "Order"
        [ describe "お好み焼きベースの計算"
            [ test "お好み焼き1つの価格" <|
                \_ ->
                    let
                        baseItem =
                            { baseItem = MenuData.baseYasai
                            , quantity = 1
                            , noodles = []
                            , toppings = []
                            }
                    in
                    Order.calculateBaseItemTotal baseItem
                        |> Expect.equal 900
            , test "お好み焼き2つの価格" <|
                \_ ->
                    let
                        baseItem =
                            { baseItem = MenuData.baseYasai
                            , quantity = 2
                            , noodles = []
                            , toppings = []
                            }
                    in
                    Order.calculateBaseItemTotal baseItem
                        |> Expect.equal 1800
            , test "お好み焼き + 麺（そば 0.5玉）の価格" <|
                \_ ->
                    let
                        baseItem =
                            { baseItem = MenuData.baseYasai
                            , quantity = 1
                            , noodles = [ { menuItem = MenuData.noodleSoba, quantity = 1 } ]
                            , toppings = []
                            }
                    in
                    -- 900 + (100 + 100×1) = 1100
                    Order.calculateBaseItemTotal baseItem
                        |> Expect.equal 1100
            , test "お好み焼き + 麺 × お好み焼きの数量の価格" <|
                \_ ->
                    let
                        baseItem =
                            { baseItem = MenuData.baseYasai
                            , quantity = 2
                            , noodles = [ { menuItem = MenuData.noodleSoba, quantity = 1 } ]
                            , toppings = []
                            }
                    in
                    -- (900 + (100 + 100×1)) × 2 = 2200
                    Order.calculateBaseItemTotal baseItem
                        |> Expect.equal 2200
            , test "お好み焼き + トッピング1つの価格" <|
                \_ ->
                    let
                        baseItem =
                            { baseItem = MenuData.baseYasai
                            , quantity = 1
                            , noodles = []
                            , toppings =
                                [ { menuItem = MenuData.toppingIkaten, quantity = 1 }
                                ]
                            }
                    in
                    Order.calculateBaseItemTotal baseItem
                        |> Expect.equal 1100
            , test "お好み焼き + トッピング複数の価格" <|
                \_ ->
                    let
                        baseItem =
                            { baseItem = MenuData.baseYasai
                            , quantity = 1
                            , noodles = []
                            , toppings =
                                [ { menuItem = MenuData.toppingIkaten, quantity = 1 }
                                , { menuItem = MenuData.toppingNegi, quantity = 2 }
                                ]
                            }
                    in
                    -- 900 + 200 + (250 × 2) = 1600
                    Order.calculateBaseItemTotal baseItem
                        |> Expect.equal 1600
            , test "お好み焼き + 麺 + トッピングの価格" <|
                \_ ->
                    let
                        baseItem =
                            { baseItem = MenuData.baseYasai
                            , quantity = 1
                            , noodles = [ { menuItem = MenuData.noodleSoba, quantity = 1 } ]
                            , toppings =
                                [ { menuItem = MenuData.toppingIkaten, quantity = 1 }
                                , { menuItem = MenuData.toppingNegi, quantity = 1 }
                                ]
                            }
                    in
                    -- 900 + (100 + 100×1) + 200 + 250 = 1550
                    Order.calculateBaseItemTotal baseItem
                        |> Expect.equal 1550
            , test "お好み焼き複数 + 麺 + トッピングの価格" <|
                \_ ->
                    let
                        baseItem =
                            { baseItem = MenuData.baseYasai
                            , quantity = 3
                            , noodles = [ { menuItem = MenuData.noodleSoba, quantity = 1 } ]
                            , toppings =
                                [ { menuItem = MenuData.toppingIkaten, quantity = 2 }
                                ]
                            }
                    in
                    -- (900 + (100 + 100×1) + (200 × 2)) × 3 = 1500 × 3 = 4500
                    Order.calculateBaseItemTotal baseItem
                        |> Expect.equal 4500
            ]
        , describe "注文全体の合計計算"
            [ test "空の注文は0円" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.calculateTotal
                        |> Expect.equal 0
            , test "お好み焼き1つの注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.calculateTotal
                        |> Expect.equal 900
            , test "独立商品（焼き物）1つの注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addStandaloneItem MenuData.grilledKaki
                        |> Order.calculateTotal
                        |> Expect.equal 1500
            , test "独立商品（飲み物）2つの注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addStandaloneItem MenuData.drinkBeer
                        |> Order.addStandaloneItem MenuData.drinkBeer
                        |> Order.calculateTotal
                        |> Expect.equal 1500
            , test "お好み焼き + 麺の注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.addNoodleToLastBase MenuData.noodleSoba
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2)（1玉） = 1200
                        |> Expect.equal 1200
            , test "お好み焼き + トッピングの注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.addToppingToLastBase MenuData.toppingIkaten
                        |> Order.calculateTotal
                        |> Expect.equal 1100
            , test "複雑な注文の合計" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.addNoodleToLastBase MenuData.noodleSoba
                        |> Order.addToppingToLastBase MenuData.toppingIkaten
                        |> Order.addToppingToLastBase MenuData.toppingNegi
                        |> Order.addStandaloneItem MenuData.grilledKaki
                        |> Order.addStandaloneItem MenuData.drinkBeer
                        |> Order.calculateTotal
                        -- お好み焼き: 900 + (100 + 100×2) + 200 + 250 = 1650
                        -- カキ焼き: 1500
                        -- ビール: 750
                        -- 合計: 3900
                        |> Expect.equal 3900
            , test "お好み焼き複数個 + 独立商品の注文" <|
                \_ ->
                    let
                        order =
                            Order.emptyOrder
                                |> Order.addBaseItem MenuData.baseYasai
                                |> Order.addNoodleToLastBase MenuData.noodleSoba

                        orderWithQuantity =
                            { order
                                | items =
                                    List.map
                                        (\item ->
                                            case item of
                                                BaseOrder baseItem ->
                                                    BaseOrder { baseItem | quantity = 2 }

                                                _ ->
                                                    item
                                        )
                                        order.items
                            }
                    in
                    orderWithQuantity
                        |> Order.addStandaloneItem MenuData.drinkBeer
                        |> Order.calculateTotal
                        -- お好み焼き2つ: (900 + (100 + 100×2)) × 2 = 2400
                        -- ビール: 750
                        -- 合計: 3150
                        |> Expect.equal 3150
            ]
        , describe "注文操作"
            [ test "お好み焼きを追加する" <|
                \_ ->
                    let
                        order =
                            Order.emptyOrder
                                |> Order.addBaseItem MenuData.baseYasai
                    in
                    List.length order.items
                        |> Expect.equal 1
            , test "独立商品を追加する" <|
                \_ ->
                    let
                        order =
                            Order.emptyOrder
                                |> Order.addStandaloneItem MenuData.grilledKaki
                    in
                    List.length order.items
                        |> Expect.equal 1
            , test "同じ独立商品を複数回追加すると数量が増える" <|
                \_ ->
                    let
                        order =
                            Order.emptyOrder
                                |> Order.addStandaloneItem MenuData.drinkBeer
                                |> Order.addStandaloneItem MenuData.drinkBeer

                        firstItem =
                            List.head order.items
                    in
                    case firstItem of
                        Just (StandaloneOrder item) ->
                            item.quantity |> Expect.equal 2

                        _ ->
                            Expect.fail "Expected a StandaloneOrder"
            , test "お好み焼きの数量を増やす" <|
                \_ ->
                    let
                        order =
                            Order.emptyOrder
                                |> Order.addBaseItem MenuData.baseYasai
                                |> Order.incrementBaseQuantity 0

                        firstItem =
                            List.head order.items
                    in
                    case firstItem of
                        Just (BaseOrder item) ->
                            item.quantity |> Expect.equal 2

                        _ ->
                            Expect.fail "Expected a BaseOrder"
            , test "お好み焼きの数量を減らす" <|
                \_ ->
                    let
                        order =
                            Order.emptyOrder
                                |> Order.addBaseItem MenuData.baseYasai
                                |> Order.incrementBaseQuantity 0
                                |> Order.decrementBaseQuantity 0

                        firstItem =
                            List.head order.items
                    in
                    case firstItem of
                        Just (BaseOrder item) ->
                            item.quantity |> Expect.equal 1

                        _ ->
                            Expect.fail "Expected a BaseOrder"
            , test "お好み焼きの数量を1から減らすと削除される" <|
                \_ ->
                    let
                        order =
                            Order.emptyOrder
                                |> Order.addBaseItem MenuData.baseYasai
                                |> Order.decrementBaseQuantity 0
                    in
                    List.length order.items
                        |> Expect.equal 0
            ]
        , describe "麺の数量操作"
            [ test "麺追加時は1玉（内部quantity=2）" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.addNoodleToLastBase MenuData.noodleSoba
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2) = 1200
                        |> Expect.equal 1200
            , test "麺の数量を増やす（1玉→1.5玉）" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.addNoodleToLastBase MenuData.noodleSoba
                        |> Order.incrementNoodleQuantity 0 MenuData.noodleSoba
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×3) = 1300
                        |> Expect.equal 1300
            , test "麺の数量を減らす（1玉→0.5玉）" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.addNoodleToLastBase MenuData.noodleSoba
                        |> Order.decrementNoodleQuantity 0 "noodle-soba"
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×1) = 1100
                        |> Expect.equal 1100
            , test "0.5玉から減らすと麺が削除される" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.addNoodleToLastBase MenuData.noodleSoba
                        |> Order.decrementNoodleQuantity 0 "noodle-soba"
                        |> Order.decrementNoodleQuantity 0 "noodle-soba"
                        |> Order.calculateTotal
                        -- 麺なし: 900
                        |> Expect.equal 900
            , test "そばとうどんを同時に追加できる" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.addNoodleToLastBase MenuData.noodleSoba
                        |> Order.addNoodleToLastBase MenuData.noodleUdon
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×4) = 1400
                        |> Expect.equal 1400
            , test "複数麺のうち片方を削除しても他方が残る" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.addNoodleToLastBase MenuData.noodleSoba
                        |> Order.addNoodleToLastBase MenuData.noodleUdon
                        |> Order.decrementNoodleQuantity 0 "noodle-soba"
                        |> Order.decrementNoodleQuantity 0 "noodle-soba"
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2) = 1200（うどんのみ残る）
                        |> Expect.equal 1200
            , test "ちゃんぽん（そば0.5玉 + うどん0.5玉 = 1玉）" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.addNoodleToLastBase MenuData.noodleSoba
                        |> Order.addNoodleToLastBase MenuData.noodleUdon
                        |> Order.decrementNoodleQuantity 0 "noodle-soba"
                        |> Order.decrementNoodleQuantity 0 "noodle-udon"
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2) = 1200
                        |> Expect.equal 1200
            , test "ちゃんぽん（そば1.5玉 + うどん0.5玉 = 2玉）" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem MenuData.baseYasai
                        |> Order.addNoodleToLastBase MenuData.noodleSoba
                        |> Order.incrementNoodleQuantity 0 MenuData.noodleSoba
                        |> Order.addNoodleToLastBase MenuData.noodleUdon
                        |> Order.decrementNoodleQuantity 0 "noodle-udon"
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×4) = 1400
                        |> Expect.equal 1400
            ]
        , describe "noodleQuantityDisplay"
            [ test "1 → 0.5" <|
                \_ -> Order.noodleQuantityDisplay 1 |> Expect.equal "0.5"
            , test "2 → 1" <|
                \_ -> Order.noodleQuantityDisplay 2 |> Expect.equal "1"
            , test "3 → 1.5" <|
                \_ -> Order.noodleQuantityDisplay 3 |> Expect.equal "1.5"
            , test "4 → 2" <|
                \_ -> Order.noodleQuantityDisplay 4 |> Expect.equal "2"
            ]
        ]
