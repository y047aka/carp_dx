module Modal exposing (Msg, State(..), UpdateResult, init, update, view)

import Html exposing (Html, button, div, h2, h3, input, option, select, span, text)
import Html.Attributes exposing (checked, class, classList, disabled, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Menu exposing (MenuItem)
import Okonomiyaki exposing (NoodleKind(..), NoodleSelection(..), Okonomiyaki, Topping, ToppingAddition, toNoodleKind)
import Order exposing (OrderItemId)


type State
    = NotEditing
    | AddingNew Okonomiyaki
    | Editing OrderItemId Okonomiyaki
    | SelectingStandaloneItem MenuItem Int (Maybe OrderItemId)


type Msg
    = EditOkonomiyaki Okonomiyaki.Msg
    | Confirm
    | Cancel
    | StandaloneIncrement
    | StandaloneDecrement


type alias UpdateResult =
    { state : State
    , orderEffect : Maybe Order.Msg
    }


init : State
init =
    NotEditing


-- UPDATE


update : Msg -> State -> UpdateResult
update msg state =
    case msg of
        EditOkonomiyaki okonomiyakiMsg ->
            case state of
                AddingNew okonomiyaki ->
                    { state = AddingNew (Okonomiyaki.update okonomiyakiMsg okonomiyaki)
                    , orderEffect = Nothing
                    }

                Editing itemId okonomiyaki ->
                    { state = Editing itemId (Okonomiyaki.update okonomiyakiMsg okonomiyaki)
                    , orderEffect = Nothing
                    }

                _ ->
                    { state = state, orderEffect = Nothing }

        Confirm ->
            case state of
                AddingNew okonomiyaki ->
                    { state = NotEditing
                    , orderEffect = Just (Order.AddBaseOrderItem { okonomiyaki = okonomiyaki, quantity = 1 })
                    }

                Editing itemId okonomiyaki ->
                    { state = NotEditing
                    , orderEffect = Just (Order.UpdateOkonomiyaki itemId okonomiyaki)
                    }

                SelectingStandaloneItem menuItem qty maybeId ->
                    let
                        effect =
                            case ( maybeId, qty ) of
                                ( Just existingId, _ ) ->
                                    Just (Order.SetStandaloneQuantity existingId qty)

                                ( Nothing, 0 ) ->
                                    Nothing

                                ( Nothing, _ ) ->
                                    Just (Order.AddStandaloneItemWithQuantity menuItem qty)
                    in
                    { state = NotEditing, orderEffect = effect }

                NotEditing ->
                    { state = NotEditing, orderEffect = Nothing }

        Cancel ->
            { state = NotEditing, orderEffect = Nothing }

        StandaloneIncrement ->
            case state of
                SelectingStandaloneItem menuItem qty maybeId ->
                    { state = SelectingStandaloneItem menuItem (qty + 1) maybeId
                    , orderEffect = Nothing
                    }

                _ ->
                    { state = state, orderEffect = Nothing }

        StandaloneDecrement ->
            case state of
                SelectingStandaloneItem menuItem qty maybeId ->
                    { state = SelectingStandaloneItem menuItem (max 0 (qty - 1)) maybeId
                    , orderEffect = Nothing
                    }

                _ ->
                    { state = state, orderEffect = Nothing }



-- VIEW


view : State -> Html Msg
view state =
    case state of
        NotEditing ->
            text ""

        AddingNew okonomiyaki ->
            editBaseModal True okonomiyaki

        Editing _ okonomiyaki ->
            editBaseModal False okonomiyaki

        SelectingStandaloneItem menuItem qty maybeId ->
            standaloneItemModal menuItem qty maybeId


editBaseModal : Bool -> Okonomiyaki -> Html Msg
editBaseModal isNew okonomiyaki =
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
                        , onClick Cancel
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
                    , onClick Confirm
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


standaloneItemModal : MenuItem -> Int -> Maybe OrderItemId -> Html Msg
standaloneItemModal menuItem qty maybeId =
    div [ class "modal modal-open" ]
        [ div [ class "modal-box max-w-sm flex flex-col p-0" ]
            [ -- 固定ヘッダー（アイテム名 + キャンセルボタン）
              div [ class "flex-shrink-0 border-b border-base-300 p-6" ]
                [ div [ class "flex items-start justify-between" ]
                    [ div []
                        [ h2 [ class "font-bold text-2xl" ] [ text menuItem.name ]
                        , div [ class "text-sm text-base-content/70 mt-1" ]
                            [ text ("¥" ++ String.fromInt menuItem.price) ]
                        ]
                    , button
                        [ class "btn btn-ghost"
                        , onClick Cancel
                        ]
                        [ text "キャンセル" ]
                    ]
                ]

            -- 数量選択エリア
            , div [ class "flex-1 p-6" ]
                [ div [ class "flex items-center justify-center gap-8 py-4" ]
                    [ button
                        [ class "btn btn-circle btn-outline btn-lg"
                        , disabled (qty <= 0)
                        , onClick StandaloneDecrement
                        ]
                        [ text "−" ]
                    , span [ class "text-4xl font-bold w-16 text-center" ]
                        [ text (String.fromInt qty) ]
                    , button
                        [ class "btn btn-circle btn-primary btn-lg"
                        , onClick StandaloneIncrement
                        ]
                        [ text "+" ]
                    ]
                ]

            -- 固定フッター（確定ボタン）
            , div [ class "flex-shrink-0 border-t border-base-300 p-4" ]
                [ if qty == 0 then
                    case maybeId of
                        Just _ ->
                            button
                                [ class "btn btn-error btn-block btn-lg rounded-full"
                                , onClick Confirm
                                ]
                                [ text "削除する" ]

                        Nothing ->
                            button
                                [ class "btn btn-ghost btn-block btn-lg rounded-full"
                                , disabled True
                                ]
                                [ text "数量を選択してください" ]

                  else
                    button
                        [ class "btn btn-primary btn-block btn-lg rounded-full"
                        , onClick Confirm
                        ]
                        [ text ("注文する（¥" ++ String.fromInt (menuItem.price * qty) ++ "）") ]
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
            , onInput (\s -> EditOkonomiyaki (Okonomiyaki.SelectNoodleKind (noodleKindFromString s)))
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
                    , onClick (EditOkonomiyaki Okonomiyaki.DecrementNoodleQuantity)
                    ]
                    [ text "−" ]
                , span [ class "text-lg font-bold w-12 text-center" ]
                    [ text (Okonomiyaki.noodleSelectionDisplay currentSelection ++ "玉") ]
                , button
                    [ class "btn btn-xs btn-circle btn-primary"
                    , disabled (not hasNoodle)
                    , onClick (EditOkonomiyaki Okonomiyaki.IncrementNoodleQuantity)
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
        , onClick (EditOkonomiyaki (Okonomiyaki.ToggleTopping item))
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
