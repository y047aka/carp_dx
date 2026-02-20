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
            [ testMenuItem "野菜入り" 900 Okonomiyaki.baseYasai
            , testMenuItem "そば入り" 1200 Okonomiyaki.baseSoba
            , testMenuItem "うどん入り" 1200 Okonomiyaki.baseUdon
            ]
        , describe "トッピング"
            [ testMenuItem "イカ天" 200 toppingIkaten
            , testMenuItem "もち" 200 toppingMochi
            , testMenuItem "ねぎかけ" 250 toppingNegi
            , testMenuItem "ニンニク" 250 toppingGarlic
            , testMenuItem "チーズ" 300 toppingCheese
            , testMenuItem "イカ" 400 toppingSquid
            , testMenuItem "エビ" 400 toppingShrimp
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
            , test "全18品目が登録されている" <|
                \_ ->
                    allMenuItems
                        |> List.length
                        |> Expect.equal 18
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
