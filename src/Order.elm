module Order exposing (BaseOrderItem, Msg(..), Order, OrderItem, OrderItemContent(..), OrderItemId, StandaloneOrderItem, calculateTotal, emptyOrder, update)

import Menu exposing (MenuItem)
import Okonomiyaki exposing (Okonomiyaki)


-- 注文アイテムID（安定した一意識別子）
type alias OrderItemId =
    Int


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


-- 注文アイテムの内容（Sum Type）
type OrderItemContent
    = BaseOrder BaseOrderItem
    | StandaloneOrder StandaloneOrderItem


-- 注文アイテム（ID + 内容）
type alias OrderItem =
    { id : OrderItemId
    , content : OrderItemContent
    }


-- 注文全体
type alias Order =
    { items : List OrderItem
    , nextId : OrderItemId
    }


-- 空の注文
emptyOrder : Order
emptyOrder =
    { items = []
    , nextId = 0
    }


-- Order に対する操作メッセージ
type Msg
    = AddOkonomiyaki Okonomiyaki.BaseKind
    | AddStandaloneItem MenuItem
    | IncrementQuantity OrderItemId
    | DecrementQuantity OrderItemId
    | EditOkonomiyaki OrderItemId Okonomiyaki.Msg


-- Order を更新する
update : Msg -> Order -> Order
update msg order =
    case msg of
        AddOkonomiyaki kind ->
            { order
                | items = order.items ++ [ { id = order.nextId, content = BaseOrder { okonomiyaki = Okonomiyaki.init kind, quantity = 1 } } ]
                , nextId = order.nextId + 1
            }

        AddStandaloneItem menuItem ->
            let
                existingItem =
                    order.items
                        |> List.filter
                            (\item ->
                                case item.content of
                                    StandaloneOrder standaloneItem ->
                                        standaloneItem.menuItem.id == menuItem.id

                                    _ ->
                                        False
                            )
                        |> List.head
            in
            case existingItem of
                Just existing ->
                    { order
                        | items =
                            List.map
                                (\item ->
                                    if item.id == existing.id then
                                        case item.content of
                                            StandaloneOrder standaloneItem ->
                                                { item | content = StandaloneOrder { standaloneItem | quantity = standaloneItem.quantity + 1 } }

                                            _ ->
                                                item

                                    else
                                        item
                                )
                                order.items
                    }

                Nothing ->
                    { order
                        | items = order.items ++ [ { id = order.nextId, content = StandaloneOrder { menuItem = menuItem, quantity = 1 } } ]
                        , nextId = order.nextId + 1
                    }

        IncrementQuantity targetId ->
            { order
                | items =
                    List.map
                        (\item ->
                            if item.id == targetId then
                                case item.content of
                                    BaseOrder baseOrderItem ->
                                        { item | content = BaseOrder { baseOrderItem | quantity = baseOrderItem.quantity + 1 } }

                                    StandaloneOrder standaloneItem ->
                                        { item | content = StandaloneOrder { standaloneItem | quantity = standaloneItem.quantity + 1 } }

                            else
                                item
                        )
                        order.items
            }

        DecrementQuantity targetId ->
            { order
                | items =
                    List.filterMap
                        (\item ->
                            if item.id == targetId then
                                case item.content of
                                    BaseOrder baseOrderItem ->
                                        let
                                            newQuantity =
                                                baseOrderItem.quantity - 1
                                        in
                                        if newQuantity <= 0 then
                                            Nothing

                                        else
                                            Just { item | content = BaseOrder { baseOrderItem | quantity = newQuantity } }

                                    StandaloneOrder standaloneItem ->
                                        let
                                            newQuantity =
                                                standaloneItem.quantity - 1
                                        in
                                        if newQuantity <= 0 then
                                            Nothing

                                        else
                                            Just { item | content = StandaloneOrder { standaloneItem | quantity = newQuantity } }

                            else
                                Just item
                        )
                        order.items
            }

        EditOkonomiyaki targetId okonomiyakiMsg ->
            { order
                | items =
                    List.map
                        (\item ->
                            if item.id == targetId then
                                case item.content of
                                    BaseOrder baseOrderItem ->
                                        { item | content = BaseOrder { baseOrderItem | okonomiyaki = Okonomiyaki.update okonomiyakiMsg baseOrderItem.okonomiyaki } }

                                    _ ->
                                        item

                            else
                                item
                        )
                        order.items
            }


-- 注文全体の合計金額
calculateTotal : Order -> Int
calculateTotal order =
    order.items
        |> List.map
            (\item ->
                case item.content of
                    BaseOrder baseOrderItem ->
                        Okonomiyaki.calculateTotal baseOrderItem.okonomiyaki * baseOrderItem.quantity

                    StandaloneOrder standaloneItem ->
                        standaloneItem.menuItem.price * standaloneItem.quantity
            )
        |> List.sum
