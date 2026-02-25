module Order exposing (Order, Msg(..), OrderItemType(..), StandaloneOrderItem, BaseOrderItem, emptyOrder, calculateTotal, update)

import Menu exposing (MenuItem)
import Okonomiyaki exposing (Okonomiyaki)


-- 独立した商品（焼き物、飲み物）
type alias StandaloneOrderItem =
    { menuItem : MenuItem
    , quantity : Int
    }


-- お好み焼き注文アイテム（構成 + 枚数）
type alias BaseOrderItem =
    { okonomiyaki : Okonomiyaki
    , quantity : Int
    }


-- 注文アイテムの型（Sum Type）
type OrderItemType
    = BaseOrder BaseOrderItem
    | StandaloneOrder StandaloneOrderItem


-- 注文全体
type alias Order =
    { items : List OrderItemType
    }


-- 空の注文
emptyOrder : Order
emptyOrder =
    { items = [] }


-- Order に対する操作メッセージ
type Msg
    = AddOkonomiyaki Okonomiyaki.BaseKind
    | AddStandaloneItem MenuItem
    | IncrementOkonomiyakiQuantity Int
    | DecrementOkonomiyakiQuantity Int
    | EditOkonomiyaki Int Okonomiyaki.Msg
    | IncrementStandaloneQuantity String
    | DecrementStandaloneQuantity String


-- Order を更新する
update : Msg -> Order -> Order
update msg order =
    case msg of
        AddOkonomiyaki kind ->
            { order | items = order.items ++ [ BaseOrder { okonomiyaki = Okonomiyaki.init kind, quantity = 1 } ] }

        AddStandaloneItem menuItem ->
            let
                existingIndex =
                    order.items
                        |> List.indexedMap Tuple.pair
                        |> List.filterMap
                            (\( idx, item ) ->
                                case item of
                                    StandaloneOrder standaloneItem ->
                                        if standaloneItem.menuItem.id == menuItem.id then
                                            Just idx

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
                        |> List.head
            in
            case existingIndex of
                Just idx ->
                    { order
                        | items =
                            List.indexedMap
                                (\i item ->
                                    if i == idx then
                                        case item of
                                            StandaloneOrder standaloneItem ->
                                                StandaloneOrder { standaloneItem | quantity = standaloneItem.quantity + 1 }

                                            _ ->
                                                item

                                    else
                                        item
                                )
                                order.items
                    }

                Nothing ->
                    { order | items = order.items ++ [ StandaloneOrder { menuItem = menuItem, quantity = 1 } ] }

        IncrementOkonomiyakiQuantity index ->
            { order
                | items =
                    List.indexedMap
                        (\i item ->
                            if i == index then
                                case item of
                                    BaseOrder baseOrderItem ->
                                        BaseOrder { baseOrderItem | quantity = baseOrderItem.quantity + 1 }

                                    _ ->
                                        item

                            else
                                item
                        )
                        order.items
            }

        DecrementOkonomiyakiQuantity index ->
            { order
                | items =
                    List.indexedMap
                        (\i item ->
                            if i == index then
                                case item of
                                    BaseOrder baseOrderItem ->
                                        let
                                            newQuantity =
                                                baseOrderItem.quantity - 1
                                        in
                                        if newQuantity <= 0 then
                                            Nothing

                                        else
                                            Just (BaseOrder { baseOrderItem | quantity = newQuantity })

                                    _ ->
                                        Just item

                            else
                                Just item
                        )
                        order.items
                        |> List.filterMap identity
            }

        EditOkonomiyaki index okonomiyakiMsg ->
            { order
                | items =
                    List.indexedMap
                        (\i item ->
                            if i == index then
                                case item of
                                    BaseOrder baseOrderItem ->
                                        BaseOrder { baseOrderItem | okonomiyaki = Okonomiyaki.update okonomiyakiMsg baseOrderItem.okonomiyaki }

                                    _ ->
                                        item

                            else
                                item
                        )
                        order.items
            }

        IncrementStandaloneQuantity targetId ->
            { order
                | items =
                    List.map
                        (\item ->
                            case item of
                                StandaloneOrder standaloneItem ->
                                    if standaloneItem.menuItem.id == targetId then
                                        StandaloneOrder { standaloneItem | quantity = standaloneItem.quantity + 1 }

                                    else
                                        item

                                _ ->
                                    item
                        )
                        order.items
            }

        DecrementStandaloneQuantity targetId ->
            { order
                | items =
                    List.map
                        (\item ->
                            case item of
                                StandaloneOrder standaloneItem ->
                                    if standaloneItem.menuItem.id == targetId then
                                        StandaloneOrder { standaloneItem | quantity = standaloneItem.quantity - 1 }

                                    else
                                        item

                                _ ->
                                    item
                        )
                        order.items
                        |> List.filter
                            (\item ->
                                case item of
                                    StandaloneOrder standaloneItem ->
                                        standaloneItem.quantity > 0

                                    _ ->
                                        True
                            )
            }


-- 注文全体の合計金額
calculateTotal : Order -> Int
calculateTotal order =
    order.items
        |> List.map
            (\item ->
                case item of
                    BaseOrder baseOrderItem ->
                        Okonomiyaki.calculateTotal baseOrderItem.okonomiyaki * baseOrderItem.quantity

                    StandaloneOrder standaloneItem ->
                        standaloneItem.menuItem.price * standaloneItem.quantity
            )
        |> List.sum
