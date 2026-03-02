module Main exposing (main)

import Browser
import Html exposing (Html, button, details, div, h1, h2, span, summary, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Menu exposing (MenuCategory, MenuItem)
import MenuData
import Modal
import Okonomiyaki exposing (NoodleSelection(..), Okonomiyaki)
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
    , editModalState : Modal.State
    }


init : Model
init =
    { currentOrder = Order.emptyOrder
    , selectedCategory = Menu.Base
    , showCheckoutModal = False
    , editModalState = Modal.init
    }


type Msg
    = SelectCategory MenuCategory
    | AddMenuItem MenuItem
    | ModalMsg Modal.Msg
    | OrderMsg Order.Msg
    | ShowCheckout
    | CloseCheckout
    | ResetOrder
    | OpenEditModal OrderItemId
    | OpenStandaloneModal MenuItem


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
                            { model | editModalState = Modal.AddingNew (Okonomiyaki.init kind) }

                        Nothing ->
                            model

                _ ->
                    model

        ModalMsg modalMsg ->
            let
                result =
                    Modal.update modalMsg model.editModalState

                newOrder =
                    case result.orderEffect of
                        Just orderMsg ->
                            Order.update orderMsg model.currentOrder

                        Nothing ->
                            model.currentOrder
            in
            { model | editModalState = result.state, currentOrder = newOrder }

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
                , editModalState = Modal.init
            }

        OpenEditModal itemId ->
            case getBaseItemById itemId model.currentOrder of
                Just baseOrderItem ->
                    { model
                        | editModalState = Modal.Editing itemId baseOrderItem.okonomiyaki
                        , showCheckoutModal = False
                    }

                Nothing ->
                    model

        OpenStandaloneModal menuItem ->
            let
                existingItem =
                    model.currentOrder.items
                        |> List.filterMap
                            (\item ->
                                case item.content of
                                    StandaloneOrder s ->
                                        if s.menuItem.id == menuItem.id then
                                            Just ( item.id, s.quantity )

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
                        |> List.head
            in
            case existingItem of
                Just ( existingId, existingQty ) ->
                    { model | editModalState = Modal.SelectingStandaloneItem menuItem existingQty (Just existingId) }

                Nothing ->
                    { model | editModalState = Modal.SelectingStandaloneItem menuItem 1 Nothing }


view : Model -> Html Msg
view model =
    let
        total =
            Order.calculateTotal model.currentOrder
    in
    div [ class "min-h-screen bg-base-200" ]
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

        -- 会計モーダル
        , if model.showCheckoutModal then
            checkoutModal model

          else
            text ""

        -- 編集モーダル
        , Html.map ModalMsg (Modal.view model.editModalState)
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


baseQuantity : Order -> MenuItem -> Int
baseQuantity order menuItem =
    case MenuData.menuItemToBaseKind menuItem of
        Nothing ->
            0

        Just kind ->
            order.items
                |> List.filterMap
                    (\item ->
                        case item.content of
                            BaseOrder b ->
                                if Okonomiyaki.baseKind b.okonomiyaki == kind then
                                    Just b.quantity

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
                |> List.sum


menuItemCard : Order -> MenuItem -> Html Msg
menuItemCard order item =
    let
        priceText =
            case MenuData.menuItemToBaseKind item of
                Just kind ->
                    "¥" ++ String.fromInt (Okonomiyaki.calculateTotal (Okonomiyaki.init kind)) ++ "〜"

                Nothing ->
                    "¥" ++ String.fromInt item.price

        qty =
            case item.category of
                Menu.Base ->
                    baseQuantity order item

                _ ->
                    standaloneQuantity order item

        clickHandler =
            case item.category of
                Menu.Base ->
                    AddMenuItem item

                _ ->
                    OpenStandaloneModal item

        subBadges =
            case MenuData.menuItemToBaseKind item of
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
                        div [ class "flex flex-wrap gap-1.5 mt-1" ]
                            (List.concat
                                [ noodleBadgesList
                                    |> List.map
                                        (\badge ->
                                            span [ class "badge badge-sm badge-soft" ]
                                                [ text (badge.name ++ " " ++ badge.quantityDisplay ++ "玉") ]
                                        )
                                , toppingBadgesList
                                    |> List.map
                                        (\badge ->
                                            span [ class "badge badge-sm badge-soft" ]
                                                [ text badge.name ]
                                        )
                                ]
                            )

                Nothing ->
                    text ""
    in
    button
        [ class "flex items-center justify-between w-full py-2.5 text-left hover:bg-base-200/50 active:bg-base-200"
        , onClick clickHandler
        ]
        [ div [ class "flex-1 flex flex-col justify-center gap-1 min-h-[2.5rem]" ]
            [ div [ class "flex items-baseline gap-2.5" ]
                [ div [ class "text-lg font-bold" ] [ text item.name ]
                , div [ class "text-sm text-base-content/50" ] [ text priceText ]
                ]
            , subBadges
            ]
        , div [ class "flex items-center gap-2 flex-shrink-0" ]
            [ if qty > 0 then
                span [ class "badge badge-success badge-lg" ]
                    [ text (String.fromInt qty) ]

              else
                text ""
            , span [ class "text-base-content/30 text-2xl" ] [ text "›" ]
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

        unitTotal =
            Okonomiyaki.calculateTotal okonomiyaki

        noodlePriceVal =
            Okonomiyaki.noodlePrice okonomiyaki

        noodleBadgesList =
            Okonomiyaki.noodleBadges okonomiyaki.noodleSelection

        toppingBadges =
            Okonomiyaki.toppingBadges okonomiyaki

        hasBreakdown =
            noodlePriceVal > 0 || not (List.isEmpty toppingBadges)
    in
    div [ class "card bg-base-100 card-border" ]
        [ div [ class "card-body p-4 gap-2" ]
            [ -- 行1: 品名 + 単価 + 編集ボタン + 数量コントロール
              orderItemHeaderRow itemId
                (Okonomiyaki.baseName okonomiyaki)
                unitTotal
                quantity
                (Just (OpenEditModal itemId))

            -- 行2: 麺・トッピングバッジ + 合計金額
            , div [ class "flex items-center justify-between" ]
                [ div [ class "flex items-center gap-1.5 flex-wrap" ]
                    (List.map
                        (\badge ->
                            span [ class "badge badge-sm badge-soft" ]
                                [ text (badge.name ++ " " ++ badge.quantityDisplay ++ "玉") ]
                        )
                        noodleBadgesList
                        ++ List.map
                            (\badge ->
                                span [ class "badge badge-sm badge-soft" ]
                                    [ text badge.name ]
                            )
                            toppingBadges
                    )
                , span [ class "text-xl font-bold text-primary" ]
                    [ text ("¥" ++ String.fromInt (unitTotal * quantity)) ]
                ]

            -- 内訳（折りたたみ、麺またはトッピングがある場合のみ）
            , if hasBreakdown then
                details []
                    [ summary [ class "text-xs text-base-content/40 cursor-pointer hover:text-base-content/70 list-none w-fit" ]
                        [ text "内訳を表示する" ]
                    , div [ class "mt-2 pl-3 space-y-0.5 text-sm text-base-content/60" ]
                        (List.concat
                            [ [ div [ class "flex justify-between" ]
                                    [ span [] [ text "本体" ]
                                    , span [] [ text ("¥" ++ String.fromInt Okonomiyaki.baseOkonomiyakiPrice) ]
                                    ]
                              ]
                            , if noodlePriceVal > 0 then
                                [ div [ class "flex justify-between" ]
                                    [ span [] [ text "麺" ]
                                    , span [] [ text ("¥" ++ String.fromInt noodlePriceVal) ]
                                    ]
                                ]

                              else
                                []
                            , toppingBadges
                                |> List.map
                                    (\badge ->
                                        div [ class "flex justify-between" ]
                                            [ span [] [ text badge.name ]
                                            , span [] [ text ("¥" ++ String.fromInt badge.price) ]
                                            ]
                                    )
                            ]
                        )
                    ]

              else
                text ""
            ]
        ]



quantityControlView : OrderItemId -> Int -> Html Msg
quantityControlView itemId quantity =
    div [ class "flex items-center gap-2" ]
        [ button
            [ class "btn btn-sm btn-circle btn-primary"
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


orderItemHeaderRow : OrderItemId -> String -> Int -> Int -> Maybe Msg -> Html Msg
orderItemHeaderRow itemId name unitPrice quantity maybeEditMsg =
    div [ class "flex items-center gap-2" ]
        ([ div [ class "flex items-baseline gap-2 flex-1" ]
            [ span [ class "text-lg font-bold" ] [ text name ]
            , span [ class "text-sm text-base-content/50" ]
                [ text ("¥" ++ String.fromInt unitPrice) ]
            ]
         ]
            ++ (case maybeEditMsg of
                    Just editMsg ->
                        [ button
                            [ class "btn btn-xs btn-ghost rounded-xl"
                            , onClick editMsg
                            ]
                            [ text "編集" ]
                        ]

                    Nothing ->
                        []
               )
            ++ [ quantityControlView itemId quantity ]
        )


standaloneOrderView : OrderItemId -> StandaloneOrderItem -> Html Msg
standaloneOrderView itemId item =
    div [ class "card bg-base-100 card-border" ]
        [ div [ class "card-body p-4 gap-2" ]
            [ -- 行1: 品名 + 単価 + 数量コントロール（編集ボタンなし）
              orderItemHeaderRow itemId
                item.menuItem.name
                item.menuItem.price
                item.quantity
                Nothing

            -- 行2: 合計金額
            , div [ class "text-right" ]
                [ span [ class "text-xl font-bold text-primary" ]
                    [ text ("¥" ++ String.fromInt (item.menuItem.price * item.quantity)) ]
                ]
            ]
        ]


checkoutModal : Model -> Html Msg
checkoutModal model =
    let
        total =
            Order.calculateTotal model.currentOrder
    in
    div [ class "modal modal-open" ]
        [ div [ class "modal-box max-w-md max-h-[90vh] flex flex-col p-0" ]
            [ -- ヘッダー
              div [ class "flex-shrink-0 p-6 border-b border-base-300" ]
                [ div [ class "flex items-start justify-between" ]
                    [ h2 [ class "font-bold text-3xl" ] [ text "お会計" ]
                    , button [ class "btn btn-ghost", onClick CloseCheckout ] [ text "戻る" ]
                    ]
                ]

            -- 品目一覧（スクロール可能）
            , div [ class "flex-1 overflow-y-auto px-3 py-3 bg-base-300 space-y-3" ]
                (List.map orderItemView model.currentOrder.items)

            -- フッター（合計・ボタン）
            , div [ class "flex-shrink-0 p-4 border-t border-base-300" ]
                [ div [ class "flex items-center justify-between p-4 bg-primary/10 rounded-3xl mb-4" ]
                    [ span [ class "text-2xl font-bold" ] [ text "合計" ]
                    , span [ class "text-4xl font-bold text-primary" ] [ text ("¥" ++ String.fromInt total) ]
                    ]
                , button [ class "btn btn-primary btn-block btn-lg rounded-full flex justify-between px-8", onClick ResetOrder ]
                    [ text "完了"
                    , text (" ¥" ++ String.fromInt total)
                    ]
                ]
            ]
        ]
