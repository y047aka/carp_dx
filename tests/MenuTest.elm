module MenuTest exposing (suite)

import Expect
import Menu exposing (MenuItem)
import MenuData exposing (..)
import Okonomiyaki
import Test exposing (..)


suite : Test
suite =
    describe "Menu"
        [ describe "お好み焼き（ベース）"
            [ testMenuItem "野菜入り" 900 baseYasai
            , testMenuItem "そば入り" 1200 baseSoba
            , testMenuItem "うどん入り" 1200 baseUdon
            ]
        , describe "トッピング"
            [ testTopping "イカ天" 200 Okonomiyaki.toppingIkaten
            , testTopping "もち" 200 Okonomiyaki.toppingMochi
            , testTopping "ねぎかけ" 250 Okonomiyaki.toppingNegi
            , testTopping "ニンニク" 250 Okonomiyaki.toppingGarlic
            , testTopping "チーズ" 300 Okonomiyaki.toppingCheese
            , testTopping "イカ" 400 Okonomiyaki.toppingSquid
            , testTopping "エビ" 400 Okonomiyaki.toppingShrimp
            ]
        , describe "焼き物"
            [ testMenuItem "カキ焼き" 1500 grilledKaki
            , testMenuItem "ホタテ焼き" 1000 grilledHotate
            , testMenuItem "イカ焼き" 900 grilledIka
            , testMenuItem "ネギ焼き" 1000 grilledNegiYaki
            , testMenuItem "とん平" 1000 grilledTonpei
            ]
        , describe "飲み物"
            [ testMenuItem "ビール（瓶）" 750 drinkBeer
            , testMenuItem "ソフトドリンク" 300 drinkSoft
            ]
        , describe "メニュー項目の整合性"
            [ test "すべてのメニュー項目にIDが設定されている" <|
                \_ ->
                    allMenuItems
                        |> List.all (\item -> not (String.isEmpty item.id))
                        |> Expect.equal True
            , test "すべてのメニュー項目に名前が設定されている" <|
                \_ ->
                    allMenuItems
                        |> List.all (\item -> not (String.isEmpty item.name))
                        |> Expect.equal True
            , test "すべてのメニュー項目の価格が正の数" <|
                \_ ->
                    allMenuItems
                        |> List.all (\item -> item.price > 0)
                        |> Expect.equal True
            , test "全11品目が登録されている（ベース4種・焼き物5種・飲み物2種）" <|
                \_ ->
                    allMenuItems
                        |> List.length
                        |> Expect.equal 11
            ]
        ]


testMenuItem : String -> Int -> MenuItem -> Test
testMenuItem expectedName expectedPrice item =
    describe expectedName
        [ test "名前が正しい" <|
            \_ -> Expect.equal expectedName item.name
        , test "価格が正しい" <|
            \_ -> Expect.equal expectedPrice item.price
        ]


testTopping : String -> Int -> Okonomiyaki.Topping -> Test
testTopping expectedName expectedPrice topping =
    describe expectedName
        [ test "名前が正しい" <|
            \_ -> Expect.equal expectedName topping.name
        , test "価格が正しい" <|
            \_ -> Expect.equal expectedPrice topping.price
        ]
