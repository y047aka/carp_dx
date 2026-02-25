module OrderTest exposing (suite)

import Expect
import MenuData
import Okonomiyaki
import Order exposing (OrderItemContent(..))
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
                        |> Order.update (Order.AddOkonomiyaki Okonomiyaki.Yasai)
                        |> Order.calculateTotal
                        |> Expect.equal 900
            , test "独立商品（焼き物）1つの注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddStandaloneItem MenuData.grilledKaki)
                        |> Order.calculateTotal
                        |> Expect.equal 1500
            , test "独立商品（飲み物）2つの注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddStandaloneItem MenuData.drinkBeer)
                        |> Order.update (Order.AddStandaloneItem MenuData.drinkBeer)
                        |> Order.calculateTotal
                        |> Expect.equal 1500
            , test "お好み焼き + 麺の注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddOkonomiyaki Okonomiyaki.Yasai)
                        |> Order.update (Order.EditOkonomiyaki 0 (Okonomiyaki.IncrementNoodle Okonomiyaki.noodleSoba))
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2)（1玉） = 1200
                        |> Expect.equal 1200
            , test "お好み焼き + トッピングの注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddOkonomiyaki Okonomiyaki.Yasai)
                        |> Order.update (Order.EditOkonomiyaki 0 (Okonomiyaki.ToggleTopping Okonomiyaki.toppingIkaten))
                        |> Order.calculateTotal
                        |> Expect.equal 1100
            , test "複雑な注文の合計" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddOkonomiyaki Okonomiyaki.Yasai)
                        |> Order.update (Order.EditOkonomiyaki 0 (Okonomiyaki.IncrementNoodle Okonomiyaki.noodleSoba))
                        |> Order.update (Order.EditOkonomiyaki 0 (Okonomiyaki.ToggleTopping Okonomiyaki.toppingIkaten))
                        |> Order.update (Order.EditOkonomiyaki 0 (Okonomiyaki.ToggleTopping Okonomiyaki.toppingNegi))
                        |> Order.update (Order.AddStandaloneItem MenuData.grilledKaki)
                        |> Order.update (Order.AddStandaloneItem MenuData.drinkBeer)
                        |> Order.calculateTotal
                        -- お好み焼き: 900 + (100 + 100×2) + 200 + 250 = 1650
                        -- カキ焼き: 1500
                        -- ビール: 750
                        -- 合計: 3900
                        |> Expect.equal 3900
            , test "お好み焼き複数個 + 独立商品の注文" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddOkonomiyaki Okonomiyaki.Yasai)
                        |> Order.update (Order.EditOkonomiyaki 0 (Okonomiyaki.IncrementNoodle Okonomiyaki.noodleSoba))
                        |> Order.update (Order.IncrementQuantity 0)
                        |> Order.update (Order.AddStandaloneItem MenuData.drinkBeer)
                        |> Order.calculateTotal
                        -- お好み焼き2つ: (900 + (100 + 100×2)) × 2 = 2400
                        -- ビール: 750
                        -- 合計: 3150
                        |> Expect.equal 3150
            ]
        , describe "注文操作"
            [ test "お好み焼きを追加する" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddOkonomiyaki Okonomiyaki.Yasai)
                        |> .items
                        |> List.length
                        |> Expect.equal 1
            , test "独立商品を追加する" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddStandaloneItem MenuData.grilledKaki)
                        |> .items
                        |> List.length
                        |> Expect.equal 1
            , test "同じ独立商品を複数回追加すると数量が増える" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddStandaloneItem MenuData.drinkBeer)
                        |> Order.update (Order.AddStandaloneItem MenuData.drinkBeer)
                        |> .items
                        |> List.head
                        |> Maybe.map .content
                        |> Expect.equal (Just (StandaloneOrder { menuItem = MenuData.drinkBeer, quantity = 2 }))
            , test "お好み焼きの数量を増やす" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddOkonomiyaki Okonomiyaki.Yasai)
                        |> Order.update (Order.IncrementQuantity 0)
                        |> .items
                        |> List.head
                        |> Maybe.map .content
                        |> Expect.equal (Just (BaseOrder { okonomiyaki = Okonomiyaki.init Okonomiyaki.Yasai, quantity = 2 }))
            , test "お好み焼きの数量を減らす" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddOkonomiyaki Okonomiyaki.Yasai)
                        |> Order.update (Order.IncrementQuantity 0)
                        |> Order.update (Order.DecrementQuantity 0)
                        |> .items
                        |> List.head
                        |> Maybe.map .content
                        |> Expect.equal (Just (BaseOrder { okonomiyaki = Okonomiyaki.init Okonomiyaki.Yasai, quantity = 1 }))
            , test "お好み焼きの数量を1から減らすと削除される" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddOkonomiyaki Okonomiyaki.Yasai)
                        |> Order.update (Order.DecrementQuantity 0)
                        |> .items
                        |> List.length
                        |> Expect.equal 0
            ]
        , describe "そば入り・うどん入りの価格計算"
            [ test "そば入り単体は1200円" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddOkonomiyaki Okonomiyaki.Soba)
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2)（そば1玉）= 1200
                        |> Expect.equal 1200
            , test "うどん入り単体は1200円" <|
                \_ ->
                    Order.emptyOrder
                        |> Order.update (Order.AddOkonomiyaki Okonomiyaki.Udon)
                        |> Order.calculateTotal
                        -- 900 + (100 + 100×2)（うどん1玉）= 1200
                        |> Expect.equal 1200
            ]
        ]
