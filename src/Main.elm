module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, input, option, select, span, text)
import Html.Attributes exposing (checked, class, classList, disabled, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Menu exposing (MenuCategory, MenuItem)
import MenuData
import Okonomiyaki exposing (NoodleKind(..), NoodleSelection(..), Okonomiyaki, Topping, ToppingAddition, toNoodleKind)
import Order exposing (BaseOrderItem, Order, OrderItem, OrderItemContent(..), OrderItemId, StandaloneOrderItem)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type EditModalState
    = NotEditing
    | AddingNew Okonomiyaki
    | Editing OrderItemId Okonomiyaki


type alias Model =
    { currentOrder : Order
    , selectedCategory : MenuCategory
    , showCheckoutModal : Bool
    , orderSummaryExpanded : Bool
    , editModalState : EditModalState
    }


init : Model
init =
    { currentOrder = Order.emptyOrder
    , selectedCategory = Menu.Base
    , showCheckoutModal = False
    , orderSummaryExpanded = False
    , editModalState = NotEditing
    }


type Msg
    = SelectCategory MenuCategory
    | AddMenuItem MenuItem
    | EditModalMsg Okonomiyaki.Msg
    | ConfirmEditModal
    | CancelEditModal
    | OrderMsg Order.Msg
    | ShowCheckout
    | CloseCheckout
    | ResetOrder
    | OpenEditModal OrderItemId
    | ToggleOrderSummary


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
                            { model | editModalState = AddingNew (Okonomiyaki.init kind) }

                        Nothing ->
                            model

                Menu.Topping ->
                    model

                Menu.Grilled ->
                    { model | currentOrder = Order.update (Order.AddStandaloneItem menuItem) model.currentOrder }

                Menu.Drink ->
                    { model | currentOrder = Order.update (Order.AddStandaloneItem menuItem) model.currentOrder }

        EditModalMsg okonomiyakiMsg ->
            case model.editModalState of
                AddingNew okonomiyaki ->
                    { model | editModalState = AddingNew (Okonomiyaki.update okonomiyakiMsg okonomiyaki) }

                Editing itemId okonomiyaki ->
                    { model | editModalState = Editing itemId (Okonomiyaki.update okonomiyakiMsg okonomiyaki) }

                NotEditing ->
                    model

        ConfirmEditModal ->
            case model.editModalState of
                AddingNew okonomiyaki ->
                    { model
                        | currentOrder = Order.update (Order.AddBaseOrderItem { okonomiyaki = okonomiyaki, quantity = 1 }) model.currentOrder
                        , editModalState = NotEditing
                    }

                Editing itemId okonomiyaki ->
                    { model
                        | currentOrder = Order.update (Order.UpdateOkonomiyaki itemId okonomiyaki) model.currentOrder
                        , editModalState = NotEditing
                    }

                NotEditing ->
                    model

        CancelEditModal ->
            { model | editModalState = NotEditing }

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
                , editModalState = NotEditing
            }

        OpenEditModal itemId ->
            case getBaseItemById itemId model.currentOrder of
                Just baseOrderItem ->
                    { model | editModalState = Editing itemId baseOrderItem.okonomiyaki }

                Nothing ->
                    model

        ToggleOrderSummary ->
            { model | orderSummaryExpanded = not model.orderSummaryExpanded }


view : Model -> Html Msg
view model =
    let
        total =
            Order.calculateTotal model.currentOrder
    in
    div [ class ("min-h-screen bg-base-200 " ++ (if model.orderSummaryExpanded then "pb-80" else "pb-16")) ]
        [ -- ヘッダー
          div [ class "navbar bg-primary text-primary-content sticky top-0 z-10" ]
            [ div [ class "flex-1" ]
                [ h1 [ class "text-2xl font-bold px-4" ] [ text "carp_dx" ]
                ]
            , div [ class "px-2" ]
                [ button
                    [ class "btn btn-primary-content btn-outline btn-md rounded-xl"
                    , onClick ShowCheckout
                    , disabled (not (hasItems model))
                    ]
                    [ if hasItems model then
                        text ("会計  ¥" ++ String.fromInt total)

                      else
                        text "会計"
                    ]
                ]
            ]

        -- カテゴリータブ
        , div [ class "tabs tabs-boxed bg-base-100 sticky top-16 z-10 p-2 shadow-md flex" ]
            [ categoryTab Menu.Base model.selectedCategory
            , categoryTab Menu.Grilled model.selectedCategory
            , categoryTab Menu.Drink model.selectedCategory
            ]

        -- メニューグリッド
        , div [ class "px-4 py-2 bg-base-100 divide-y divide-base-300" ]
            (MenuData.allMenuItems
                |> List.filter (\item -> item.category == model.selectedCategory)
                |> List.map (menuItemCard model.currentOrder)
            )

        -- 注文サマリー（固定ボトムシート）
        , orderSummary model

        -- 会計モーダル
        , if model.showCheckoutModal then
            checkoutModal model

          else
            text ""

        -- 編集モーダル
        , case model.editModalState of
            NotEditing ->
                text ""

            AddingNew okonomiyaki ->
                editBaseModalContent True okonomiyaki

            Editing _ okonomiyaki ->
                editBaseModalContent False okonomiyaki
        ]


categoryTab : MenuCategory -> MenuCategory -> Html Msg
categoryTab category selectedCategory =
    button
        [ class "tab flex-1"
        , classList [ ( "tab-active", category == selectedCategory ) ]
        , onClick (SelectCategory category)
        ]
        [ text (Menu.categoryName category)
        ]


standaloneQuantity : Order -> MenuItem -> Int
standaloneQuantity order menuItem =
    order.items
        |> List.filterMap
            (\item ->
                case item.content of
                    StandaloneOrder s ->
                        if s.menuItem.id == menuItem.id then
                            Just s.quantity

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.withDefault 0


menuItemCard : Order -> MenuItem -> Html Msg
menuItemCard order item =
    let
        priceText =
            if item.category == Menu.Base then
                case MenuData.menuItemToBaseKind item of
                    Just kind ->
                        "¥" ++ String.fromInt (Okonomiyaki.calculateTotal (Okonomiyaki.init kind)) ++ "〜"

                    Nothing ->
                        "¥?"

            else
                "¥" ++ String.fromInt item.price
    in
    case item.category of
        Menu.Base ->
            button
                [ class "flex items-center justify-between w-full py-3 text-left hover:bg-base-200/50 active:bg-base-200"
                , onClick (AddMenuItem item)
                ]
                [ div [ class "flex-1" ]
                    [ div [ class "text-lg font-bold" ] [ text item.name ]
                    , case MenuData.menuItemToBaseKind item of
                        Just kind ->
                            let
                                defaultOkonomiyaki =
                                    Okonomiyaki.init kind

                                noodleBadgesList =
                                    Okonomiyaki.noodleBadges defaultOkonomiyaki.noodleSelection

                                toppingBadgesList =
                                    Okonomiyaki.toppingBadges defaultOkonomiyaki
                            in
                            if List.isEmpty noodleBadgesList && List.isEmpty toppingBadgesList then
                                text ""

                            else
                                div [ class "flex flex-wrap gap-1 mt-1" ]
                                    (List.concat
                                        [ noodleBadgesList
                                            |> List.map
                                                (\badge ->
                                                    span [ class "badge badge-ghost badge-sm" ]
                                                        [ text (badge.name ++ " " ++ badge.quantityDisplay ++ "玉") ]
                                                )
                                        , toppingBadgesList
                                            |> List.map
                                                (\badge ->
                                                    span [ class "badge badge-ghost badge-sm" ]
                                                        [ text badge.name ]
                                                )
                                        ]
                                    )

                        Nothing ->
                            text ""
                    ]
                , div [ class "text-base text-primary flex-shrink-0 ml-2" ] [ text priceText ]
                ]

        _ ->
            let
                qty =
                    standaloneQuantity order item

                standaloneItemId =
                    order.items
                        |> List.filterMap
                            (\i ->
                                case i.content of
                                    StandaloneOrder s ->
                                        if s.menuItem.id == item.id then
                                            Just i.id

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
                        |> List.head
            in
            div [ class "flex items-center justify-between py-2" ]
                [ div []
                    [ div
                        [ class "text-lg font-bold"
                        , classList [ ( "text-success", qty > 0 ) ]
                        ]
                        [ text item.name ]
                    , div [ class "text-sm text-base-content/70" ] [ text priceText ]
                    ]
                , div [ class "flex items-center gap-2" ]
                    [ button
                        [ class "btn btn-xs btn-circle btn-outline"
                        , disabled (qty == 0)
                        , onClick
                            (case standaloneItemId of
                                Just id ->
                                    OrderMsg (Order.DecrementQuantity id)

                                Nothing ->
                                    OrderMsg (Order.AddStandaloneItem item)
                            )
                        ]
                        [ text "−" ]
                    , span
                        [ class "text-lg font-bold w-8 text-center"
                        , classList [ ( "text-base-content/30", qty == 0 ) ]
                        ]
                        [ text (String.fromInt qty) ]
                    , button
                        [ class "btn btn-xs btn-circle btn-primary"
                        , onClick (OrderMsg (Order.AddStandaloneItem item))
                        ]
                        [ text "+" ]
                    ]
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


hasItems : Model -> Bool
hasItems model =
    not (List.isEmpty model.currentOrder.items)


orderSummary : Model -> Html Msg
orderSummary model =
    div [ class "fixed bottom-0 left-0 right-0 bg-base-100 shadow-2xl rounded-t-3xl" ]
        [ -- ハンドル（クリックで展開/折りたたみ）
          div
            [ class "flex flex-col items-center pt-3 pb-2 cursor-pointer select-none"
            , onClick ToggleOrderSummary
            ]
            [ div [ class "w-10 h-1.5 bg-base-300 rounded-full mb-2" ] []
            , if model.orderSummaryExpanded then
                span [ class "text-xs text-base-content/40 font-medium tracking-widest uppercase" ] [ text "閉じる" ]

              else if hasItems model then
                span [ class "text-xs text-base-content/40 font-medium tracking-widest uppercase" ] [ text "注文を確認" ]

              else
                text ""
            ]

        -- 注文リスト（展開時のみ）
        , if model.orderSummaryExpanded then
            div [ class "px-4 pb-4 max-h-64 overflow-y-auto" ]
                (if hasItems model then
                    List.map orderItemView model.currentOrder.items

                 else
                    [ div [ class "text-center text-base-content/50 py-4" ]
                        [ text "商品を選択してください" ]
                    ]
                )

          else
            text ""
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
                [ class "btn btn-sm btn-outline btn-info rounded-xl"
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
        , if okonomiyaki.noodleSelection == WithoutNoodle && List.isEmpty okonomiyaki.toppings then
            text ""

          else
            div [ class "flex flex-wrap gap-1.5 mb-2 pl-1" ]
                (List.concat
                    [ Okonomiyaki.noodleBadges okonomiyaki.noodleSelection
                        |> List.map
                            (\badge ->
                                span [ class "px-2.5 py-1 bg-base-300 rounded-full text-xs font-medium" ]
                                    [ text (badge.name ++ " " ++ badge.quantityDisplay ++ "玉") ]
                            )
                    , Okonomiyaki.toppingBadges okonomiyaki
                        |> List.map
                            (\badge ->
                                span [ class "px-2.5 py-1 bg-base-300 rounded-full text-xs font-medium" ]
                                    [ text (badge.name ++ " ¥" ++ String.fromInt (badge.price * quantity)) ]
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


editBaseModalContent : Bool -> Okonomiyaki -> Html Msg
editBaseModalContent isNew okonomiyaki =
    let
        totalPrice =
            Okonomiyaki.calculateTotal okonomiyaki
    in
    div [ class "modal modal-open" ]
        [ div [ class "modal-box max-w-lg max-h-[90vh] flex flex-col p-0" ]
            [ -- 固定ヘッダー（商品名とカスタマイズ内容）
              div [ class "flex-shrink-0 border-b border-base-300 p-6" ]
                [ div [ class "flex items-start justify-between mb-4" ]
                    [ h2 [ class "font-bold text-2xl" ]
                        [ text (Okonomiyaki.baseName okonomiyaki) ]
                    , button
                        [ class "btn btn-ghost"
                        , onClick CancelEditModal
                        ]
                        [ text "キャンセル" ]
                    ]

                -- 麺・トッピング（badge表示）
                , if okonomiyaki.noodleSelection == WithoutNoodle && List.isEmpty okonomiyaki.toppings then
                    text ""

                  else
                    div [ class "flex flex-wrap gap-1.5" ]
                        (List.concat
                            [ Okonomiyaki.noodleBadges okonomiyaki.noodleSelection
                                |> List.map
                                    (\badge ->
                                        span [ class "badge badge-ghost badge-sm" ]
                                            [ text (badge.name ++ " (" ++ badge.quantityDisplay ++ "玉)") ]
                                    )
                            , Okonomiyaki.toppingBadges okonomiyaki
                                |> List.map
                                    (\badge ->
                                        span [ class "badge badge-ghost badge-sm" ]
                                            [ text badge.name ]
                                    )
                            ]
                        )
                ]

            -- スクロール可能なコンテンツエリア
            , div [ class "flex-1 overflow-y-auto p-6" ]
                [ -- 麺セクション
                  div [ class "mb-2" ]
                    [ h3 [ class "text-lg font-bold mb-3" ] [ text "麺" ]
                    , noodleSelectionView okonomiyaki
                    ]

                , div [ class "divider my-2" ] []

                -- トッピングセクション
                , div []
                    [ h3 [ class "text-lg font-bold mb-3" ] [ text "トッピング" ]
                    , div [ class "grid grid-cols-2 gap-3" ]
                        (Okonomiyaki.allToppings
                            |> List.map (toppingMenuItem okonomiyaki.toppings)
                        )
                    ]
                ]

            -- 固定フッター（確定ボタンのみ）
            , div [ class "flex-shrink-0 border-t border-base-300 p-4" ]
                [ button
                    [ class "btn btn-primary btn-block btn-lg rounded-full"
                    , onClick ConfirmEditModal
                    ]
                    [ text
                        (if isNew then
                            "追加する（¥" ++ String.fromInt totalPrice ++ "）"

                         else
                            "完了（¥" ++ String.fromInt totalPrice ++ "）"
                        )
                    ]
                ]
            ]
        ]


noodleKindFromString : String -> NoodleKind
noodleKindFromString s =
    case s of
        "soba" ->
            Soba

        "udon" ->
            Udon

        "champon" ->
            Champon

        _ ->
            NoNoodle


noodleSelectionView : Okonomiyaki -> Html Msg
noodleSelectionView okonomiyaki =
    let
        currentSelection =
            okonomiyaki.noodleSelection

        currentKind =
            toNoodleKind currentSelection

        hasNoodle =
            currentKind /= NoNoodle
    in
    div []
        [ -- 麺の種類選択（selectbox）
          select
            [ class "select select-lg w-full mb-3"
            , onInput (\s -> EditModalMsg (Okonomiyaki.SelectNoodleKind (noodleKindFromString s)))
            ]
            [ option [ value "none", selected (currentKind == NoNoodle) ] [ text "なし" ]
            , option [ value "soba", selected (currentKind == Soba) ] [ text "そば" ]
            , option [ value "udon", selected (currentKind == Udon) ] [ text "うどん" ]
            , option [ value "champon", selected (currentKind == Champon) ] [ text "ちゃんぽん" ]
            ]

        -- 数量コントロール（麺なし時はグレーアウト）
        , div
            [ class "flex items-center justify-between py-2 px-2 bg-base-200 rounded-lg"
            , classList [ ( "opacity-40 pointer-events-none", not hasNoodle ) ]
            ]
            [ div [ class "text-base font-bold" ]
                [ text "数量" ]
            , div [ class "flex items-center gap-2" ]
                [ button
                    [ class "btn btn-xs btn-circle btn-outline"
                    , disabled (not hasNoodle)
                    , onClick (EditModalMsg Okonomiyaki.DecrementNoodleQuantity)
                    ]
                    [ text "−" ]
                , span [ class "text-lg font-bold w-12 text-center" ]
                    [ text (Okonomiyaki.noodleSelectionDisplay currentSelection ++ "玉") ]
                , button
                    [ class "btn btn-xs btn-circle btn-primary"
                    , disabled (not hasNoodle)
                    , onClick (EditModalMsg Okonomiyaki.IncrementNoodleQuantity)
                    ]
                    [ text "+" ]
                ]
            ]
        ]


toppingMenuItem : List ToppingAddition -> Topping -> Html Msg
toppingMenuItem currentToppings item =
    let
        isSelected =
            currentToppings
                |> List.any (\t -> t.topping.kind == item.kind)
    in
    div
        [ class "p-3 border border-base-300 rounded-lg flex items-center justify-between cursor-pointer hover:bg-base-200/50"
        , classList [ ( "border-success bg-success/10", isSelected ) ]
        , onClick (EditModalMsg (Okonomiyaki.ToggleTopping item))
        ]
        [ div [ class "flex-1" ]
            [ div [ class "text-base font-bold" ] [ text item.name ]
            , div [ class "text-sm text-base-content/60" ]
                [ text ("¥" ++ String.fromInt item.price) ]
            ]
        , input
            [ type_ "checkbox"
            , class "checkbox checkbox-success pointer-events-none"
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
            [ h2 [ class "font-bold text-3xl mb-4 text-center" ] [ text "お会計" ]

            -- 品目内訳
            , div [ class "mb-4 divide-y divide-base-300" ]
                (List.map checkoutItemRow model.currentOrder.items)

            -- 合計金額
            , div [ class "flex items-center justify-between p-4 bg-primary/10 rounded-3xl mb-6" ]
                [ span [ class "text-2xl font-bold" ] [ text "合計" ]
                , span [ class "text-4xl font-bold text-primary" ] [ text ("¥" ++ String.fromInt total) ]
                ]

            , div [ class "modal-action grid grid-cols-2 gap-3" ]
                [ button [ class "btn btn-outline btn-lg", onClick CloseCheckout ]
                    [ text "戻る" ]
                , button [ class "btn btn-primary btn-lg", onClick ResetOrder ]
                    [ text "完了" ]
                ]
            ]
        ]


checkoutItemRow : OrderItem -> Html Msg
checkoutItemRow item =
    case item.content of
        BaseOrder baseOrderItem ->
            let
                okonomiyaki =
                    baseOrderItem.okonomiyaki

                quantity =
                    baseOrderItem.quantity
            in
            div [ class "flex items-center justify-between py-2" ]
                [ div [ class "flex-1" ]
                    [ div [ class "font-semibold" ] [ text (Okonomiyaki.baseName okonomiyaki) ]
                    , div [ class "text-sm text-base-content/60" ]
                        [ text ("¥" ++ String.fromInt (Okonomiyaki.calculateTotal okonomiyaki) ++ " × " ++ String.fromInt quantity) ]
                    ]
                , div [ class "font-bold" ]
                    [ text ("¥" ++ String.fromInt (Okonomiyaki.calculateTotal okonomiyaki * quantity)) ]
                ]

        StandaloneOrder standaloneItem ->
            div [ class "flex items-center justify-between py-2" ]
                [ div [ class "flex-1" ]
                    [ div [ class "font-semibold" ] [ text standaloneItem.menuItem.name ]
                    , div [ class "text-sm text-base-content/60" ]
                        [ text ("¥" ++ String.fromInt standaloneItem.menuItem.price ++ " × " ++ String.fromInt standaloneItem.quantity) ]
                    ]
                , div [ class "font-bold" ]
                    [ text ("¥" ++ String.fromInt (standaloneItem.menuItem.price * standaloneItem.quantity)) ]
                ]
