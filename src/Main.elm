module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, input, span, text)
import Html.Attributes exposing (checked, class, classList, disabled, type_)
import Html.Events exposing (onClick)
import Menu exposing (MenuCategory, MenuItem)
import MenuData
import Okonomiyaki exposing (Noodle, Okonomiyaki, Topping, ToppingAddition)
import Order exposing (BaseOrderItem, Order, OrderItem, OrderItemContent(..), OrderItemId, StandaloneOrderItem)


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
    , editingBaseId : Maybe OrderItemId
    , isAddingNewBase : Bool
    }


init : Model
init =
    { currentOrder = Order.emptyOrder
    , selectedCategory = Menu.Base
    , showCheckoutModal = False
    , editingBaseId = Nothing
    , isAddingNewBase = False
    }


type Msg
    = SelectCategory MenuCategory
    | AddMenuItem MenuItem
    | CancelAddingBase
    | OrderMsg Order.Msg
    | ShowCheckout
    | CloseCheckout
    | ResetOrder
    | OpenEditModal OrderItemId
    | CloseEditModal


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectCategory category ->
            { model | selectedCategory = category }

        AddMenuItem menuItem ->
            case menuItem.category of
                Menu.Base ->
                    case MenuData.menuItemToBaseKind menuItem of
                        Just kind ->
                            let
                                newId =
                                    model.currentOrder.nextId

                                newOrder =
                                    Order.update (Order.AddOkonomiyaki kind) model.currentOrder
                            in
                            { model
                                | currentOrder = newOrder
                                , editingBaseId = Just newId
                                , isAddingNewBase = True
                            }

                        Nothing ->
                            model

                Menu.Topping ->
                    model

                Menu.Grilled ->
                    { model | currentOrder = Order.update (Order.AddStandaloneItem menuItem) model.currentOrder }

                Menu.Drink ->
                    { model | currentOrder = Order.update (Order.AddStandaloneItem menuItem) model.currentOrder }

        CancelAddingBase ->
            case model.editingBaseId of
                Just targetId ->
                    let
                        newOrder =
                            { items = List.filter (\item -> item.id /= targetId) model.currentOrder.items
                            , nextId = model.currentOrder.nextId
                            }
                    in
                    { model
                        | currentOrder = newOrder
                        , editingBaseId = Nothing
                        , isAddingNewBase = False
                    }

                Nothing ->
                    { model
                        | editingBaseId = Nothing
                        , isAddingNewBase = False
                    }

        OrderMsg orderMsg ->
            { model | currentOrder = Order.update orderMsg model.currentOrder }

        ShowCheckout ->
            { model | showCheckoutModal = True }

        CloseCheckout ->
            { model | showCheckoutModal = False }

        ResetOrder ->
            { model
                | currentOrder = Order.emptyOrder
                , showCheckoutModal = False
                , editingBaseId = Nothing
            }

        OpenEditModal itemId ->
            { model
                | editingBaseId = Just itemId
                , isAddingNewBase = False
            }

        CloseEditModal ->
            { model
                | editingBaseId = Nothing
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
            [ categoryTab Menu.Base model.selectedCategory
            , categoryTab Menu.Grilled model.selectedCategory
            , categoryTab Menu.Drink model.selectedCategory
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
        , if model.editingBaseId /= Nothing then
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
            if item.category == Menu.Base then
                case MenuData.menuItemToBaseKind item of
                    Just kind ->
                        "¥" ++ String.fromInt (Okonomiyaki.calculateTotal (Okonomiyaki.init kind))

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


getBaseItemById : OrderItemId -> Order -> Maybe BaseOrderItem
getBaseItemById targetId order =
    order.items
        |> List.filter (\item -> item.id == targetId)
        |> List.head
        |> Maybe.andThen
            (\item ->
                case item.content of
                    BaseOrder baseOrderItem ->
                        Just baseOrderItem

                    StandaloneOrder _ ->
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
                    (List.map orderItemView model.currentOrder.items)

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


orderItemView : OrderItem -> Html Msg
orderItemView item =
    case item.content of
        BaseOrder baseOrderItem ->
            baseOrderView item.id baseOrderItem

        StandaloneOrder standaloneItem ->
            standaloneOrderView item.id standaloneItem



baseOrderView : OrderItemId -> BaseOrderItem -> Html Msg
baseOrderView itemId baseOrderItem =
    let
        okonomiyaki =
            baseOrderItem.okonomiyaki

        quantity =
            baseOrderItem.quantity
    in
    div [ class "mb-4 p-3 bg-base-200 rounded-lg" ]
        [ -- お好み焼き本体
          div [ class "flex flex-wrap items-center justify-between gap-2 mb-2" ]
            [ div [ class "flex-1 min-w-[120px]" ]
                [ div [ class "text-lg font-bold" ] [ text (Okonomiyaki.baseName okonomiyaki) ]
                , div [ class "text-sm text-base-content/70" ]
                    [ text ("¥" ++ String.fromInt (Okonomiyaki.calculateTotal okonomiyaki) ++ " × " ++ String.fromInt quantity)
                    ]
                ]
            , button
                [ class "btn btn-sm btn-outline btn-info"
                , onClick (OpenEditModal itemId)
                ]
                [ text "編集" ]
            , div [ class "flex items-center gap-2" ]
                [ button
                    [ class "btn btn-sm btn-circle btn-outline"
                    , onClick (OrderMsg (Order.DecrementQuantity itemId))
                    ]
                    [ text "−" ]
                , span [ class "text-xl font-bold w-8 text-center" ]
                    [ text (String.fromInt quantity) ]
                , button
                    [ class "btn btn-sm btn-circle btn-primary"
                    , onClick (OrderMsg (Order.IncrementQuantity itemId))
                    ]
                    [ text "+" ]
                ]
            ]

        -- 麺・トッピング（badge表示）
        , if List.isEmpty okonomiyaki.noodles && List.isEmpty okonomiyaki.toppings then
            text ""

          else
            div [ class "flex flex-wrap gap-1.5 mb-2 pl-1" ]
                (List.concat
                    [ okonomiyaki.noodles
                        |> List.map
                            (\n ->
                                span [ class "px-2.5 py-1 bg-base-300 rounded-full text-xs font-medium" ]
                                    [ text (n.noodle.name ++ " " ++ Okonomiyaki.noodleQuantityDisplay n.quantity ++ "玉 ¥" ++ String.fromInt (Okonomiyaki.noodleAdditionPrice n * quantity)) ]
                            )
                    , okonomiyaki.toppings
                        |> List.map
                            (\topping ->
                                span [ class "px-2.5 py-1 bg-base-300 rounded-full text-xs font-medium" ]
                                    [ text (topping.topping.name ++ " ¥" ++ String.fromInt (topping.topping.price * topping.quantity * quantity)) ]
                            )
                    ]
                )

        -- 小計
        , div [ class "flex justify-between pt-2 mt-2 border-t border-base-300" ]
            [ span [ class "font-semibold" ] [ text "小計" ]
            , span [ class "font-bold text-primary" ]
                [ text ("¥" ++ String.fromInt (Okonomiyaki.calculateTotal okonomiyaki * quantity))
                ]
            ]
        ]



standaloneOrderView : OrderItemId -> StandaloneOrderItem -> Html Msg
standaloneOrderView itemId item =
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
                , onClick (OrderMsg (Order.DecrementQuantity itemId))
                ]
                [ text "−" ]
            , span [ class "text-xl font-bold w-8 text-center" ]
                [ text (String.fromInt item.quantity) ]
            , button
                [ class "btn btn-sm btn-circle btn-primary"
                , onClick (OrderMsg (Order.IncrementQuantity itemId))
                ]
                [ text "+" ]
            ]
        , div [ class "text-lg font-bold text-right w-24" ]
            [ text ("¥" ++ String.fromInt (item.menuItem.price * item.quantity))
            ]
        ]


editBaseModal : Model -> Html Msg
editBaseModal model =
    case model.editingBaseId of
        Nothing ->
            text ""

        Just itemId ->
            case getBaseItemById itemId model.currentOrder of
                Nothing ->
                    text ""

                Just baseOrderItem ->
                    let
                        okonomiyaki =
                            baseOrderItem.okonomiyaki

                        modalTitle =
                            if model.isAddingNewBase then
                                "「" ++ Okonomiyaki.baseName okonomiyaki ++ "」をカスタマイズ"

                            else
                                "「" ++ Okonomiyaki.baseName okonomiyaki ++ "」を編集"
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
                                            |> List.map (noodleMenuItem itemId okonomiyaki)
                                        )
                                    ]

                                -- トッピングセクション
                                , div [ class "mb-6" ]
                                    [ h3 [ class "text-lg font-bold mb-3" ] [ text "トッピング" ]
                                    , div [ class "grid grid-cols-2 gap-3" ]
                                        (Okonomiyaki.allToppings
                                            |> List.map (toppingMenuItem itemId okonomiyaki.toppings)
                                        )
                                    ]
                                ]

                            -- 固定フッター
                            , div [ class "flex-shrink-0 border-t border-base-300 bg-base-200 p-4" ]
                                [ -- サマリーカード
                                  div [ class "bg-base-100 rounded-xl p-4 border border-base-300 mb-3" ]
                                    [ -- ベース
                                      div [ class "font-bold mb-3" ]
                                        [ text (Okonomiyaki.baseName okonomiyaki) ]

                                    -- 麺・トッピング（badge表示）
                                    , if List.isEmpty okonomiyaki.noodles && List.isEmpty okonomiyaki.toppings then
                                        text ""

                                      else
                                        div [ class "flex flex-wrap gap-1.5 mb-3" ]
                                            (List.concat
                                                [ okonomiyaki.noodles
                                                    |> List.map
                                                        (\n ->
                                                            span [ class "px-2.5 py-1 bg-base-200 rounded-full text-xs font-medium" ]
                                                                [ text (n.noodle.name ++ " (" ++ Okonomiyaki.noodleQuantityDisplay n.quantity ++ "玉)") ]
                                                        )
                                                , okonomiyaki.toppings
                                                    |> List.map
                                                        (\topping ->
                                                            span [ class "px-2.5 py-1 bg-base-200 rounded-full text-xs font-medium" ]
                                                                [ text topping.topping.name ]
                                                        )
                                                ]
                                            )

                                    -- 小計
                                    , div [ class "flex items-center justify-between pt-3 border-t border-base-300" ]
                                        [ span [ class "text-sm font-semibold text-base-content/70" ] [ text "小計" ]
                                        , span [ class "text-2xl font-bold text-primary" ]
                                            [ text ("¥" ++ String.fromInt (Okonomiyaki.calculateTotal okonomiyaki * baseOrderItem.quantity)) ]
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


noodleMenuItem : OrderItemId -> Okonomiyaki -> Noodle -> Html Msg
noodleMenuItem itemId baseOrderItem noodle =
    let
        -- noodles リスト内の数量（選択されていなければ Nothing）
        maybeNoodleAddition =
            baseOrderItem.noodles
                |> List.filter (\n -> n.noodle.kind == noodle.kind)
                |> List.head

        isSelected =
            maybeNoodleAddition /= Nothing

        priceText =
            case maybeNoodleAddition of
                Just na ->
                    "¥" ++ String.fromInt (Okonomiyaki.noodleAdditionPrice na)

                Nothing ->
                    "¥" ++ String.fromInt (Okonomiyaki.noodleAdditionPrice { noodle = noodle, quantity = Okonomiyaki.Whole 1 })
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
                , onClick (OrderMsg (Order.EditOkonomiyaki itemId (Okonomiyaki.DecrementNoodle noodle)))
                , disabled (not isSelected)
                ]
                [ text "−" ]
            , span
                [ class "text-lg font-bold w-12 text-center"
                , classList [ ( "text-base-content/30", not isSelected ) ]
                ]
                [ text
                    (case maybeNoodleAddition of
                        Just na ->
                            Okonomiyaki.noodleQuantityDisplay na.quantity ++ "玉"

                        Nothing ->
                            "0玉"
                    )
                ]
            , button
                [ class "btn btn-xs btn-circle btn-primary"
                , onClick (OrderMsg (Order.EditOkonomiyaki itemId (Okonomiyaki.IncrementNoodle noodle)))
                ]
                [ text "+" ]
            ]
        ]


toppingMenuItem : OrderItemId -> List ToppingAddition -> Topping -> Html Msg
toppingMenuItem itemId currentToppings item =
    let
        isSelected =
            currentToppings
                |> List.any (\t -> t.topping.kind == item.kind)
    in
    div
        [ class "flex items-center justify-between p-3 border border-base-300 rounded-lg hover:bg-base-200 cursor-pointer"
        , classList [ ( "bg-success/10 border-success", isSelected ) ]
        , onClick (OrderMsg (Order.EditOkonomiyaki itemId (Okonomiyaki.ToggleTopping item)))
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
