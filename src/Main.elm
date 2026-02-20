module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, input, span, text)
import Html.Attributes exposing (checked, class, classList, disabled, type_)
import Html.Events exposing (onClick)
import Menu exposing (MenuCategory(..), MenuItem)
import MenuData
import Okonomiyaki exposing (Addition, BaseOrderItem, Noodle, NoodleAddition)
import Order exposing (Order, OrderItemType(..), StandaloneOrderItem)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { currentOrder : Order
    , selectedCategory : MenuCategory
    , showCheckoutModal : Bool
    , editingBaseIndex : Maybe Int
    , isAddingNewBase : Bool
    }


init : Model
init =
    { currentOrder = Order.emptyOrder
    , selectedCategory = Base
    , showCheckoutModal = False
    , editingBaseIndex = Nothing
    , isAddingNewBase = False
    }


type Msg
    = SelectCategory MenuCategory
    | AddMenuItem MenuItem
    | AddNoodle Noodle
    | CancelAddingBase
    | IncrementBaseQuantity Int
    | DecrementBaseQuantity Int
    | IncrementNoodleQuantity Int Noodle
    | DecrementNoodleQuantity Int Noodle
    | ToggleTopping Int MenuItem
    | IncrementStandaloneQuantity String
    | DecrementStandaloneQuantity String
    | ShowCheckout
    | CloseCheckout
    | ResetOrder
    | OpenEditModal Int
    | CloseEditModal


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectCategory category ->
            { model | selectedCategory = category }

        AddMenuItem menuItem ->
            case menuItem.category of
                Base ->
                    let
                        newOrder =
                            Order.addBaseItem menuItem model.currentOrder

                        lastIndex =
                            List.length newOrder.items - 1
                    in
                    { model
                        | currentOrder = newOrder
                        , editingBaseIndex = Just lastIndex
                        , isAddingNewBase = True
                    }

                Topping ->
                    { model | currentOrder = Order.addToppingToLastBase menuItem model.currentOrder }

                Grilled ->
                    { model | currentOrder = Order.addStandaloneItem menuItem model.currentOrder }

                Drink ->
                    { model | currentOrder = Order.addStandaloneItem menuItem model.currentOrder }

        AddNoodle noodle ->
            { model | currentOrder = Order.addNoodleToLastBase noodle model.currentOrder }

        CancelAddingBase ->
            let
                newItems =
                    List.take (List.length model.currentOrder.items - 1) model.currentOrder.items

                newOrder =
                    { items = newItems }
            in
            { model
                | currentOrder = newOrder
                , editingBaseIndex = Nothing
                , isAddingNewBase = False
            }

        IncrementBaseQuantity index ->
            { model | currentOrder = Order.incrementBaseQuantity index model.currentOrder }

        DecrementBaseQuantity index ->
            { model | currentOrder = Order.decrementBaseQuantity index model.currentOrder }

        IncrementNoodleQuantity index noodle ->
            { model
                | currentOrder =
                    model.currentOrder
                        |> Order.incrementNoodleQuantity index noodle
                        |> Order.normalizeBaseOnNoodleAdd index noodle
            }

        DecrementNoodleQuantity index noodle ->
            { model
                | currentOrder =
                    model.currentOrder
                        |> Order.decrementNoodleQuantity index noodle
                        |> Order.normalizeBaseOnNoodleChange index
            }

        ToggleTopping index toppingItem ->
            { model
                | currentOrder =
                    model.currentOrder
                        |> Order.toggleTopping index toppingItem
                        |> Order.normalizeBaseOnToppingChange index
            }

        IncrementStandaloneQuantity itemId ->
            { model | currentOrder = Order.incrementStandaloneQuantity itemId model.currentOrder }

        DecrementStandaloneQuantity itemId ->
            { model | currentOrder = Order.decrementStandaloneQuantity itemId model.currentOrder }

        ShowCheckout ->
            { model | showCheckoutModal = True }

        CloseCheckout ->
            { model | showCheckoutModal = False }

        ResetOrder ->
            { model
                | currentOrder = Order.emptyOrder
                , showCheckoutModal = False
                , editingBaseIndex = Nothing
            }

        OpenEditModal index ->
            { model
                | editingBaseIndex = Just index
                , isAddingNewBase = False
            }

        CloseEditModal ->
            { model
                | editingBaseIndex = Nothing
                , isAddingNewBase = False
            }


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-base-200 pb-80" ]
        [ -- ヘッダー
          div [ class "navbar bg-primary text-primary-content sticky top-0 z-10" ]
            [ div [ class "flex-1" ]
                [ h1 [ class "text-2xl font-bold px-4" ] [ text "お好み焼き会計" ]
                ]
            ]

        -- カテゴリータブ
        , div [ class "tabs tabs-boxed bg-base-100 sticky top-16 z-10 p-2 shadow-md" ]
            [ categoryTab Base model.selectedCategory
            , categoryTab Grilled model.selectedCategory
            , categoryTab Drink model.selectedCategory
            ]

        -- メニューグリッド
        , div [ class "p-4" ]
            [ div [ class "grid grid-cols-2 gap-3" ]
                (MenuData.allMenuItems
                    |> List.filter (\item -> item.category == model.selectedCategory)
                    |> List.map menuItemCard
                )
            ]

        -- 注文サマリー（固定ボトムシート）
        , orderSummary model

        -- 会計モーダル
        , if model.showCheckoutModal then
            checkoutModal model

          else
            text ""

        -- 編集モーダル
        , if model.editingBaseIndex /= Nothing then
            editBaseModal model

          else
            text ""
        ]


categoryTab : MenuCategory -> MenuCategory -> Html Msg
categoryTab category selectedCategory =
    button
        [ class "tab"
        , classList [ ( "tab-active", category == selectedCategory ) ]
        , onClick (SelectCategory category)
        ]
        [ text (Menu.categoryName category)
        ]


menuItemCard : MenuItem -> Html Msg
menuItemCard item =
    let
        priceText =
            if item.category == Base then
                case Okonomiyaki.menuItemToOkonomiyakiBase item of
                    Just base ->
                        "¥" ++ String.fromInt (Okonomiyaki.calculateBaseItemTotal (Okonomiyaki.initialBaseOrderItem base))

                    Nothing ->
                        "¥?"

            else
                "¥" ++ String.fromInt item.price
    in
    button
        [ class "btn btn-lg h-auto min-h-20 flex-col items-start justify-center p-4 normal-case"
        , onClick (AddMenuItem item)
        ]
        [ div [ class "text-lg font-bold w-full text-left" ] [ text item.name ]
        , div [ class "text-xl text-primary w-full text-left" ] [ text priceText ]
        ]


getBaseItemByIndex : Int -> Order -> Maybe BaseOrderItem
getBaseItemByIndex index order =
    order.items
        |> List.drop index
        |> List.head
        |> Maybe.andThen
            (\item ->
                case item of
                    BaseOrder baseItem ->
                        Just baseItem

                    _ ->
                        Nothing
            )


orderSummary : Model -> Html Msg
orderSummary model =
    let
        total =
            Order.calculateTotal model.currentOrder

        hasItems =
            not (List.isEmpty model.currentOrder.items)
    in
    div [ class "fixed bottom-0 left-0 right-0 bg-base-100 shadow-2xl border-t-4 border-primary" ]
        [ div [ class "p-4" ]
            [ -- 注文リスト
              if hasItems then
                div [ class "mb-4 max-h-48 overflow-y-auto" ]
                    (List.indexedMap orderItemView model.currentOrder.items)

              else
                div [ class "text-center text-base-content/50 mb-4 py-4" ]
                    [ text "商品を選択してください" ]

            -- 合計金額
            , div [ class "flex items-center justify-between mb-4 p-4 bg-primary/10 rounded-lg" ]
                [ span [ class "text-2xl font-bold" ] [ text "合計" ]
                , span [ class "text-4xl font-bold text-primary" ] [ text ("¥" ++ String.fromInt total) ]
                ]

            -- アクションボタン
            , div [ class "grid grid-cols-2 gap-3" ]
                [ button
                    [ class "btn btn-outline btn-lg"
                    , onClick ResetOrder
                    , disabled (not hasItems)
                    ]
                    [ text "クリア" ]
                , button
                    [ class "btn btn-primary btn-lg"
                    , onClick ShowCheckout
                    , disabled (not hasItems)
                    ]
                    [ text "会計" ]
                ]
            ]
        ]


orderItemView : Int -> OrderItemType -> Html Msg
orderItemView index item =
    case item of
        BaseOrder baseItem ->
            baseOrderView index baseItem

        StandaloneOrder standaloneItem ->
            standaloneOrderView standaloneItem


noodlePrice : NoodleAddition -> Int
noodlePrice noodleAddition =
    let
        n =
            noodleAddition.noodle
    in
    n.basePrice + n.pricePerHalfBall * noodleAddition.quantity


baseOrderView : Int -> BaseOrderItem -> Html Msg
baseOrderView index baseItem =
    div [ class "mb-4 p-3 bg-base-200 rounded-lg" ]
        [ -- お好み焼き本体
          div [ class "flex flex-wrap items-center justify-between gap-2 mb-2" ]
            [ div [ class "flex-1 min-w-[120px]" ]
                [ div [ class "text-lg font-bold" ] [ text baseItem.base.name ]
                , div [ class "text-sm text-base-content/70" ]
                    [ text ("¥" ++ String.fromInt baseItem.base.basePrice ++ " × " ++ String.fromInt baseItem.quantity)
                    ]
                ]
            , button
                [ class "btn btn-sm btn-outline btn-info"
                , onClick (OpenEditModal index)
                ]
                [ text "編集" ]
            , div [ class "flex items-center gap-2" ]
                [ button
                    [ class "btn btn-sm btn-circle btn-outline"
                    , onClick (DecrementBaseQuantity index)
                    ]
                    [ text "−" ]
                , span [ class "text-xl font-bold w-8 text-center" ]
                    [ text (String.fromInt baseItem.quantity) ]
                , button
                    [ class "btn btn-sm btn-circle btn-primary"
                    , onClick (IncrementBaseQuantity index)
                    ]
                    [ text "+" ]
                ]
            ]

        -- 麺・トッピング（badge表示）
        , if List.isEmpty baseItem.noodles && List.isEmpty baseItem.toppings then
            text ""

          else
            div [ class "flex flex-wrap gap-1.5 mb-2 pl-1" ]
                (List.concat
                    [ baseItem.noodles
                        |> List.map
                            (\n ->
                                span [ class "px-2.5 py-1 bg-base-300 rounded-full text-xs font-medium" ]
                                    [ text (n.noodle.name ++ " " ++ Okonomiyaki.noodleQuantityDisplay n.quantity ++ "玉 ¥" ++ String.fromInt (noodlePrice n * baseItem.quantity)) ]
                            )
                    , baseItem.toppings
                        |> List.map
                            (\topping ->
                                span [ class "px-2.5 py-1 bg-base-300 rounded-full text-xs font-medium" ]
                                    [ text (topping.menuItem.name ++ " ¥" ++ String.fromInt (topping.menuItem.price * topping.quantity * baseItem.quantity)) ]
                            )
                    ]
                )

        -- 小計
        , div [ class "flex justify-between pt-2 mt-2 border-t border-base-300" ]
            [ span [ class "font-semibold" ] [ text "小計" ]
            , span [ class "font-bold text-primary" ]
                [ text ("¥" ++ String.fromInt (Okonomiyaki.calculateBaseItemTotal baseItem))
                ]
            ]
        ]



standaloneOrderView : StandaloneOrderItem -> Html Msg
standaloneOrderView item =
    div [ class "flex items-center justify-between py-2 border-b border-base-300" ]
        [ div [ class "flex-1" ]
            [ div [ class "text-lg font-semibold" ] [ text item.menuItem.name ]
            , div [ class "text-sm text-base-content/70" ]
                [ text ("¥" ++ String.fromInt item.menuItem.price ++ " × " ++ String.fromInt item.quantity)
                ]
            ]
        , div [ class "flex items-center gap-2" ]
            [ button
                [ class "btn btn-sm btn-circle btn-outline"
                , onClick (DecrementStandaloneQuantity item.menuItem.id)
                ]
                [ text "−" ]
            , span [ class "text-xl font-bold w-8 text-center" ]
                [ text (String.fromInt item.quantity) ]
            , button
                [ class "btn btn-sm btn-circle btn-primary"
                , onClick (IncrementStandaloneQuantity item.menuItem.id)
                ]
                [ text "+" ]
            ]
        , div [ class "text-lg font-bold text-right w-24" ]
            [ text ("¥" ++ String.fromInt (item.menuItem.price * item.quantity))
            ]
        ]


editBaseModal : Model -> Html Msg
editBaseModal model =
    case model.editingBaseIndex of
        Nothing ->
            text ""

        Just index ->
            case getBaseItemByIndex index model.currentOrder of
                Nothing ->
                    text ""

                Just baseItem ->
                    let
                        modalTitle =
                            if model.isAddingNewBase then
                                "「" ++ baseItem.base.name ++ "」をカスタマイズ"

                            else
                                "「" ++ baseItem.base.name ++ "」を編集"
                    in
                    div [ class "modal modal-open" ]
                        [ div [ class "modal-box max-w-2xl max-h-[90vh] flex flex-col p-0" ]
                            [ -- スクロール可能なコンテンツエリア
                              div [ class "flex-1 overflow-y-auto p-6" ]
                                [ -- ヘッダー
                                  div [ class "mb-6" ]
                                    [ h2 [ class "font-bold text-2xl mb-2" ]
                                        [ text modalTitle ]
                                    ]

                                -- 麺セクション
                                , div [ class "mb-6" ]
                                    [ h3 [ class "text-lg font-bold mb-3" ] [ text "麺" ]
                                    , div [ class "grid grid-cols-2 gap-3" ]
                                        (Okonomiyaki.allNoodles
                                            |> List.map (noodleMenuItem index baseItem)
                                        )
                                    ]

                                -- トッピングセクション
                                , div [ class "mb-6" ]
                                    [ h3 [ class "text-lg font-bold mb-3" ] [ text "トッピング" ]
                                    , div [ class "grid grid-cols-2 gap-3" ]
                                        (MenuData.allMenuItems
                                            |> List.filter (\item -> item.category == Topping)
                                            |> List.map (toppingMenuItem index baseItem.toppings)
                                        )
                                    ]
                                ]

                            -- 固定フッター
                            , div [ class "flex-shrink-0 border-t border-base-300 bg-base-200 p-4" ]
                                [ -- サマリーカード
                                  div [ class "bg-base-100 rounded-xl p-4 border border-base-300 mb-3" ]
                                    [ -- ベース
                                      div [ class "font-bold mb-3" ]
                                        [ text baseItem.base.name ]

                                    -- 麺・トッピング（badge表示）
                                    , if List.isEmpty baseItem.noodles && List.isEmpty baseItem.toppings then
                                        text ""

                                      else
                                        div [ class "flex flex-wrap gap-1.5 mb-3" ]
                                            (List.concat
                                                [ baseItem.noodles
                                                    |> List.map
                                                        (\n ->
                                                            span [ class "px-2.5 py-1 bg-base-200 rounded-full text-xs font-medium" ]
                                                                [ text (n.noodle.name ++ " (" ++ Okonomiyaki.noodleQuantityDisplay n.quantity ++ "玉)") ]
                                                        )
                                                , baseItem.toppings
                                                    |> List.map
                                                        (\topping ->
                                                            span [ class "px-2.5 py-1 bg-base-200 rounded-full text-xs font-medium" ]
                                                                [ text topping.menuItem.name ]
                                                        )
                                                ]
                                            )

                                    -- 小計
                                    , div [ class "flex items-center justify-between pt-3 border-t border-base-300" ]
                                        [ span [ class "text-sm font-semibold text-base-content/70" ] [ text "小計" ]
                                        , span [ class "text-2xl font-bold text-primary" ]
                                            [ text ("¥" ++ String.fromInt (Okonomiyaki.calculateBaseItemTotal baseItem)) ]
                                        ]
                                    ]

                                -- アクションボタン
                                , if model.isAddingNewBase then
                                    div [ class "grid grid-cols-2 gap-3" ]
                                        [ button
                                            [ class "btn btn-outline btn-lg"
                                            , onClick CancelAddingBase
                                            ]
                                            [ text "キャンセル" ]
                                        , button
                                            [ class "btn btn-primary btn-lg"
                                            , onClick CloseEditModal
                                            ]
                                            [ text "確定" ]
                                        ]

                                  else
                                    button
                                        [ class "btn btn-primary btn-block btn-lg"
                                        , onClick CloseEditModal
                                        ]
                                        [ text "完了" ]
                                ]
                            ]
                        ]


noodleMenuItem : Int -> BaseOrderItem -> Noodle -> Html Msg
noodleMenuItem baseIndex baseOrderItem noodle =
    let
        -- noodles リスト内の数量
        noodleQuantity =
            baseOrderItem.noodles
                |> List.filter (\n -> n.noodle.kind == noodle.kind)
                |> List.head
                |> Maybe.map .quantity
                |> Maybe.withDefault 0

        -- ベースの込み麺として内包されているか
        isEmbeddedInBase =
            Okonomiyaki.isDefaultNoodleOf noodle baseOrderItem.base

        isSelected =
            noodleQuantity > 0 || isEmbeddedInBase

        priceText =
            if noodleQuantity > 0 then
                "¥" ++ String.fromInt (noodle.basePrice + noodle.pricePerHalfBall * noodleQuantity)

            else
                "¥" ++ String.fromInt (noodle.basePrice + noodle.pricePerHalfBall * 2)
    in
    div
        [ class "flex items-center justify-between p-3 border border-base-300 rounded-lg"
        , classList [ ( "bg-success/10 border-success", isSelected ) ]
        ]
        [ -- 左側: 名前と価格
          div [ class "flex-1 text-left" ]
            [ div [ class "text-base font-bold" ] [ text noodle.name ]
            , div [ class "text-sm" ]
                [ text priceText ]
            ]

        -- 右側: 数量コントロール
        , div [ class "flex items-center gap-2" ]
            [ button
                [ class "btn btn-xs btn-circle btn-outline"
                , onClick (DecrementNoodleQuantity baseIndex noodle)
                , disabled (not isSelected)
                ]
                [ text "−" ]
            , span
                [ class "text-lg font-bold w-12 text-center"
                , classList [ ( "text-base-content/30", not isSelected ) ]
                ]
                [ text
                    (if noodleQuantity > 0 then
                        Okonomiyaki.noodleQuantityDisplay noodleQuantity ++ "玉"

                     else
                        "0玉"
                    )
                ]
            , button
                [ class "btn btn-xs btn-circle btn-primary"
                , onClick (IncrementNoodleQuantity baseIndex noodle)
                ]
                [ text "+" ]
            ]
        ]


toppingMenuItem : Int -> List Addition -> MenuItem -> Html Msg
toppingMenuItem baseIndex currentToppings item =
    let
        isSelected =
            currentToppings
                |> List.any (\t -> t.menuItem.id == item.id)
    in
    div
        [ class "flex items-center justify-between p-3 border border-base-300 rounded-lg hover:bg-base-200 cursor-pointer"
        , classList [ ( "bg-success/10 border-success", isSelected ) ]
        , onClick (ToggleTopping baseIndex item)
        ]
        [ div [ class "flex-1" ]
            [ div [ class "text-base font-bold" ] [ text item.name ]
            , div [ class "text-sm text-base-content/70" ]
                [ text ("¥" ++ String.fromInt item.price) ]
            ]
        , input
            [ type_ "checkbox"
            , class "toggle toggle-success pointer-events-none"
            , checked isSelected
            ]
            []
        ]


checkoutModal : Model -> Html Msg
checkoutModal model =
    let
        total =
            Order.calculateTotal model.currentOrder
    in
    div [ class "modal modal-open" ]
        [ div [ class "modal-box max-w-md" ]
            [ h2 [ class "font-bold text-3xl mb-6 text-center" ] [ text "お会計" ]
            , div [ class "text-center mb-8" ]
                [ div [ class "text-6xl font-bold text-primary mb-2" ]
                    [ text ("¥" ++ String.fromInt total) ]
                , div [ class "text-lg text-base-content/70" ]
                    [ text (String.fromInt (List.length model.currentOrder.items) ++ "品目")
                    ]
                ]
            , div [ class "modal-action grid grid-cols-2 gap-3" ]
                [ button [ class "btn btn-outline btn-lg", onClick CloseCheckout ]
                    [ text "戻る" ]
                , button [ class "btn btn-primary btn-lg", onClick ResetOrder ]
                    [ text "完了" ]
                ]
            ]
        ]
