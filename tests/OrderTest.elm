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
                        |> Order.addBaseItem Okonomiyaki.Yasai
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
                        |> Order.addBaseItem Okonomiyaki.Yasai
                        |> Order.updateOkonomiyakiAt 0 (Okonomiyaki.IncrementNoodle Okonomiyaki.noodleSoba)
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2)（1玉） = 1200
                        |> Expect.equal 1200
            , test "お好み焼き + トッピングの注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.Yasai
                        |> Order.updateOkonomiyakiAt 0 (Okonomiyaki.ToggleTopping Okonomiyaki.toppingIkaten)
                        |> Order.calculateTotal
                        |> Expect.equal 1100
            , test "複雑な注文の合計" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.Yasai
                        |> Order.updateOkonomiyakiAt 0 (Okonomiyaki.IncrementNoodle Okonomiyaki.noodleSoba)
                        |> Order.updateOkonomiyakiAt 0 (Okonomiyaki.ToggleTopping Okonomiyaki.toppingIkaten)
                        |> Order.updateOkonomiyakiAt 0 (Okonomiyaki.ToggleTopping Okonomiyaki.toppingNegi)
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
                                |> Order.addBaseItem Okonomiyaki.Yasai
                                |> Order.updateOkonomiyakiAt 0 (Okonomiyaki.IncrementNoodle Okonomiyaki.noodleSoba)

                        orderWithQuantity =
                            { order
                                | items =
                                    List.map
                                        (\item ->
                                            case item of
                                                BaseOrder baseOrderItem ->
                                                    BaseOrder { baseOrderItem | quantity = 2 }

                                                StandaloneOrder _ ->
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
                                |> Order.addBaseItem Okonomiyaki.Yasai
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
                                |> Order.addBaseItem Okonomiyaki.Yasai
                                |> Order.incrementBaseQuantity 0

                        firstItem =
                            List.head order.items
                    in
                    case firstItem of
                        Just (BaseOrder baseOrderItem) ->
                            baseOrderItem.quantity |> Expect.equal 2

                        _ ->
                            Expect.fail "Expected a BaseOrder"
            , test "お好み焼きの数量を減らす" <|
                \_ ->
                    let
                        order =
                            Order.emptyOrder
                                |> Order.addBaseItem Okonomiyaki.Yasai
                                |> Order.incrementBaseQuantity 0
                                |> Order.decrementBaseQuantity 0

                        firstItem =
                            List.head order.items
                    in
                    case firstItem of
                        Just (BaseOrder baseOrderItem) ->
                            baseOrderItem.quantity |> Expect.equal 1

                        _ ->
                            Expect.fail "Expected a BaseOrder"
            , test "お好み焼きの数量を1から減らすと削除される" <|
                \_ ->
                    let
                        order =
                            Order.emptyOrder
                                |> Order.addBaseItem Okonomiyaki.Yasai
                                |> Order.decrementBaseQuantity 0
                    in
                    List.length order.items
                        |> Expect.equal 0
            ]
        , describe "そば入り・うどん入りの価格計算"
            [ test "そば入り単体は1200円" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.Soba
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2)（そば1玉）= 1200
                        |> Expect.equal 1200
            , test "うどん入り単体は1200円" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.addBaseItem Okonomiyaki.Udon
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2)（うどん1玉）= 1200
                        |> Expect.equal 1200
            ]
        ]
