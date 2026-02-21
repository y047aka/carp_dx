module OrderTest exposing (suite)

import Expect
import MenuData
import Okonomiyaki
import Order exposing (OrderItemType(..))
import Test exposing (..)


suite : Test
suite =
    describe "Order"
        [ describe "注文全体の合計計算"
            [ test "空の注文は0円" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.calculateTotal
                        |> Expect.equal 0
            , test "お好み焼き1つの注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
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
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleSoba
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2)（1玉） = 1200
                        |> Expect.equal 1200
            , test "お好み焼き + トッピングの注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                        |> Order.addToppingToLastBase Okonomiyaki.toppingIkaten
                        |> Order.calculateTotal
                        |> Expect.equal 1100
            , test "複雑な注文の合計" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleSoba
                        |> Order.addToppingToLastBase Okonomiyaki.toppingIkaten
                        |> Order.addToppingToLastBase Okonomiyaki.toppingNegi
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
                                |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                                |> Order.addNoodleToLastBase Okonomiyaki.noodleSoba

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
                                |> Order.addBaseItem Okonomiyaki.baseYasaiBase
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
                                |> Order.addBaseItem Okonomiyaki.baseYasaiBase
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
                                |> Order.addBaseItem Okonomiyaki.baseYasaiBase
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
                                |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                                |> Order.decrementBaseQuantity 0
                    in
                    List.length order.items
                        |> Expect.equal 0
            ]
        , describe "麺の数量操作"
            [ test "麺追加時は1玉（内部quantity=2）" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleSoba
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2) = 1200
                        |> Expect.equal 1200
            , test "麺の数量を増やす（1玉→1.5玉）" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleSoba
                        |> Order.incrementNoodleQuantity 0 Okonomiyaki.noodleSoba
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×3) = 1300
                        |> Expect.equal 1300
            , test "麺の数量を減らす（1玉→0.5玉）" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleSoba
                        |> Order.decrementNoodleQuantity 0 Okonomiyaki.noodleSoba
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×1) = 1100
                        |> Expect.equal 1100
            , test "0.5玉から減らすと麺が削除される" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleSoba
                        |> Order.decrementNoodleQuantity 0 Okonomiyaki.noodleSoba
                        |> Order.decrementNoodleQuantity 0 Okonomiyaki.noodleSoba
                        |> Order.calculateTotal
                        -- 麺なし: 900
                        |> Expect.equal 900
            , test "そばとうどんを同時に追加できる" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleSoba
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleUdon
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×4) = 1400
                        |> Expect.equal 1400
            , test "複数麺のうち片方を削除しても他方が残る" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleSoba
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleUdon
                        |> Order.decrementNoodleQuantity 0 Okonomiyaki.noodleSoba
                        |> Order.decrementNoodleQuantity 0 Okonomiyaki.noodleSoba
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2) = 1200（うどんのみ残る）
                        |> Expect.equal 1200
            , test "ちゃんぽん（そば0.5玉 + うどん0.5玉 = 1玉）" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleSoba
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleUdon
                        |> Order.decrementNoodleQuantity 0 Okonomiyaki.noodleSoba
                        |> Order.decrementNoodleQuantity 0 Okonomiyaki.noodleUdon
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2) = 1200
                        |> Expect.equal 1200
            , test "ちゃんぽん（そば1.5玉 + うどん0.5玉 = 2玉）" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseYasaiBase
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleSoba
                        |> Order.incrementNoodleQuantity 0 Okonomiyaki.noodleSoba
                        |> Order.addNoodleToLastBase Okonomiyaki.noodleUdon
                        |> Order.decrementNoodleQuantity 0 Okonomiyaki.noodleUdon
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×4) = 1400
                        |> Expect.equal 1400
            ]
        , describe "そば入り・うどん入りの価格計算"
            [ test "そば入り単体は1200円" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseSobaBase
                        |> Order.calculateTotal
                        -- baseSoba.price=1200（defaultNoodle=soba 1玉分込み）
                        |> Expect.equal 1200
            , test "うどん入り単体は1200円" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseUdonBase
                        |> Order.calculateTotal
                        |> Expect.equal 1200
            , test "そば入り＋そば0.5玉追加は1300円（そば1.5玉）" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseSobaBase
                        |> Order.incrementNoodleQuantity 0 Okonomiyaki.noodleSoba
                        -- noodles の soba qty: 2 → 3（1.5玉）、defaultNoodle 1玉分除外 → extra=1
                        -- 1200 + 100×1 = 1300
                        |> Order.calculateTotal
                        |> Expect.equal 1300
            , test "そば入り＋イカ天は1400円" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseSobaBase
                        |> Order.addToppingToLastBase Okonomiyaki.toppingIkaten
                        -- 1200 + 200 = 1400
                        |> Order.calculateTotal
                        |> Expect.equal 1400
            , test "そば入りのそばを全部減らすとbaseYasaiに切り替わる" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.baseSobaBase
                        |> Order.decrementNoodleQuantity 0 Okonomiyaki.noodleSoba
                        |> Order.decrementNoodleQuantity 0 Okonomiyaki.noodleSoba
                        -- qty: 2 → 1 → 0（削除）
                        |> Order.normalizeBaseOnNoodleChange 0
                        |> Order.calculateTotal
                        -- baseYasai: 900
                        |> Expect.equal 900
            ]
        ]
