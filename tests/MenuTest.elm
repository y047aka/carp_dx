module MenuTest exposing (suite)

import Expect
import Menu exposing (MenuItem(..), menuItemId, menuItemName, menuItemPrice)
import MenuData exposing (..)
import Okonomiyaki
import Test exposing (..)


suite : Test
suite =
    describe "Menu"
        [ describe "お好み焼き（ベース）"
            [ testStandardMenuItem "野菜入り" 900 Okonomiyaki.baseYasai
            , testStandardMenuItem "そば入り" 1200 Okonomiyaki.baseSoba
            , testStandardMenuItem "うどん入り" 1200 Okonomiyaki.baseUdon
            ]
        , describe "麺"
            [ testNoodleMenuItem "そば" 100 100 Okonomiyaki.noodleSoba
            , testNoodleMenuItem "うどん" 100 100 Okonomiyaki.noodleUdon
            ]
        , describe "トッピング"
            [ testStandardMenuItem "イカ天" 200 toppingIkaten
            , testStandardMenuItem "もち" 200 toppingMochi
            , testStandardMenuItem "ねぎかけ" 250 toppingNegi
            , testStandardMenuItem "ニンニク" 250 toppingGarlic
            , testStandardMenuItem "チーズ" 300 toppingCheese
            , testStandardMenuItem "イカ" 400 toppingSquid
            , testStandardMenuItem "エビ" 400 toppingShrimp
            ]
        , describe "焼き物"
            [ testStandardMenuItem "カキ焼き" 1500 grilledKaki
            , testStandardMenuItem "ホタテ焼き" 1000 grilledHotate
            , testStandardMenuItem "イカ焼き" 900 grilledIka
            , testStandardMenuItem "ネギ焼き" 1000 grilledNegiYaki
            , testStandardMenuItem "とん平" 1000 grilledTonpei
            ]
        , describe "飲み物"
            [ testStandardMenuItem "ビール（瓶）" 750 drinkBeer
            , testStandardMenuItem "ソフトドリンク" 300 drinkSoft
            ]
        , describe "メニュー項目の整合性"
            [ test "すべてのメニュー項目にIDが設定されている" <|
                \_ ->
                    allMenuItems
                        |> List.all (\item -> not (String.isEmpty (menuItemId item)))
                        |> Expect.equal True
            , test "すべてのメニュー項目に名前が設定されている" <|
                \_ ->
                    allMenuItems
                        |> List.all (\item -> not (String.isEmpty (menuItemName item)))
                        |> Expect.equal True
            , test "すべてのメニュー項目の価格が正の数" <|
                \_ ->
                    allMenuItems
                        |> List.all (\item -> menuItemPrice item 1 > 0)
                        |> Expect.equal True
            , test "全20品目が登録されている" <|
                \_ ->
                    allMenuItems
                        |> List.length
                        |> Expect.equal 20
            ]
        ]


testStandardMenuItem : String -> Int -> MenuItem -> Test
testStandardMenuItem expectedName expectedPrice item =
    describe expectedName
        [ test "名前が正しい" <|
            \_ -> Expect.equal expectedName (menuItemName item)
        , test "価格が正しい" <|
            \_ ->
                case item of
                    StandardItem r ->
                        Expect.equal expectedPrice r.price

                    NoodleItem _ ->
                        Expect.fail "Expected StandardItem"
        ]


testNoodleMenuItem : String -> Int -> Int -> MenuItem -> Test
testNoodleMenuItem expectedName expectedBasePrice expectedPricePerHalfBall item =
    describe expectedName
        [ test "名前が正しい" <|
            \_ -> Expect.equal expectedName (menuItemName item)
        , test "基本料金が正しい" <|
            \_ ->
                case item of
                    NoodleItem r ->
                        Expect.equal expectedBasePrice r.basePrice

                    StandardItem _ ->
                        Expect.fail "Expected NoodleItem"
        , test "0.5玉あたりの価格が正しい" <|
            \_ ->
                case item of
                    NoodleItem r ->
                        Expect.equal expectedPricePerHalfBall r.pricePerHalfBall

                    StandardItem _ ->
                        Expect.fail "Expected NoodleItem"
        ]
