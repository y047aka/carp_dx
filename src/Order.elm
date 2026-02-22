module Order exposing (Order, OrderItemType(..), StandaloneOrderItem, addBaseItem, addStandaloneItem, addNoodleToLastBase, addToppingToLastBase, calculateTotal, emptyOrder, incrementBaseQuantity, decrementBaseQuantity, incrementNoodleQuantity, decrementNoodleQuantity, toggleTopping, incrementStandaloneQuantity, decrementStandaloneQuantity, normalizeBase, BaseOrderItem)

import Menu exposing (MenuItem)
import Okonomiyaki exposing (Noodle, Okonomiyaki, Topping)


-- 独立した商品（焼き物、飲み物）
type alias StandaloneOrderItem =
    { menuItem : MenuItem
    , quantity : Int
    }


-- お好み焼き注文アイテム（構成 + 枚数）
type alias BaseOrderItem =
    { okonomiyaki : Okonomiyaki.Okonomiyaki
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


-- ヘルパー：指定インデックスのお好み焼きに変換を適用する
updateBaseItemAt : Int -> (Okonomiyaki.Okonomiyaki -> Okonomiyaki.Okonomiyaki) -> Order -> Order
updateBaseItemAt index transform order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == index then
                        case item of
                            BaseOrder baseOrderItem ->
                                BaseOrder { baseOrderItem | okonomiyaki = transform baseOrderItem.okonomiyaki }

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


-- ヘルパー：指定インデックスのBaseOrderItemに変換を適用し、Nothing なら削除する
updateOrRemoveBaseOrderItemAt : Int -> (BaseOrderItem -> Maybe BaseOrderItem) -> Order -> Order
updateOrRemoveBaseOrderItemAt index transform order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == index then
                        case item of
                            BaseOrder baseOrderItem ->
                                transform baseOrderItem |> Maybe.map BaseOrder

                            _ ->
                                Just item

                    else
                        Just item
                )
                order.items
                |> List.filterMap identity
    }


-- 新しいお好み焼きを追加
addBaseItem : Okonomiyaki.BaseKind -> Order -> Order
addBaseItem kind order =
    { order | items = order.items ++ [ BaseOrder { okonomiyaki = Okonomiyaki.init kind, quantity = 1 } ] }


-- 独立商品（焼き物・飲み物）を追加または数量を増やす
addStandaloneItem : MenuItem -> Order -> Order
addStandaloneItem menuItem order =
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
            -- 既存の商品があれば数量+1
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
            -- 新しい商品を追加
            { order
                | items = order.items ++ [ StandaloneOrder { menuItem = menuItem, quantity = 1 } ]
            }


-- ヘルパー：最後のお好み焼きのインデックスを取得
getLastBaseItemIndex : Order -> Maybe Int
getLastBaseItemIndex order =
    order.items
        |> List.indexedMap Tuple.pair
        |> List.reverse
        |> List.filterMap
            (\( idx, item ) ->
                case item of
                    BaseOrder _ ->
                        Just idx

                    StandaloneOrder _ ->
                        Nothing
            )
        |> List.head


-- 最後のお好み焼きに麺を追加し、ベースを正規化する
addNoodleToLastBase : Noodle -> Order -> Order
addNoodleToLastBase noodle order =
    case getLastBaseItemIndex order of
        Just idx ->
            order
                |> updateBaseItemAt idx (Okonomiyaki.addNoodle noodle)
                |> normalizeBase idx

        Nothing ->
            order


-- 最後のお好み焼きにトッピングを追加
addToppingToLastBase : Topping -> Order -> Order
addToppingToLastBase toppingItem order =
    case getLastBaseItemIndex order of
        Just idx ->
            updateBaseItemAt idx (Okonomiyaki.addTopping toppingItem) order

        Nothing ->
            order


-- トッピングのトグル（未選択なら追加、選択済みなら削除）
toggleTopping : Int -> Topping -> Order -> Order
toggleTopping index toppingItem order =
    updateBaseItemAt index (Okonomiyaki.toggleTopping toppingItem) order


-- お好み焼きの数量を増やす
incrementBaseQuantity : Int -> Order -> Order
incrementBaseQuantity index order =
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


-- お好み焼きの数量を減らす（0になったら削除）
decrementBaseQuantity : Int -> Order -> Order
decrementBaseQuantity index order =
    updateOrRemoveBaseOrderItemAt index
        (\baseOrderItem ->
            let
                newQuantity =
                    baseOrderItem.quantity - 1
            in
            if newQuantity <= 0 then
                Nothing

            else
                Just { baseOrderItem | quantity = newQuantity }
        )
        order


-- 麺の数量を増やす（0.5玉単位、新規追加にも対応）
incrementNoodleQuantity : Int -> Noodle -> Order -> Order
incrementNoodleQuantity baseIndex noodle order =
    updateBaseItemAt baseIndex (Okonomiyaki.addNoodle noodle) order


-- 麺の数量を減らす（0.5玉単位、0になったら削除）
decrementNoodleQuantity : Int -> Noodle -> Order -> Order
decrementNoodleQuantity baseIndex noodle order =
    updateBaseItemAt baseIndex (Okonomiyaki.decrementNoodle noodle) order


-- 独立商品の数量を増やす
incrementStandaloneQuantity : String -> Order -> Order
incrementStandaloneQuantity targetId order =
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


-- 独立商品の数量を減らす（0になったら削除）
decrementStandaloneQuantity : String -> Order -> Order
decrementStandaloneQuantity targetId order =
    { order
        | items =
            List.map
                (\item ->
                    case item of
                        StandaloneOrder standaloneItem ->
                            if standaloneItem.menuItem.id == targetId then
                                StandaloneOrder { standaloneItem | quantity = max 0 (standaloneItem.quantity - 1) }

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


-- 麺・トッピング操作後に呼ぶ。ベースを麺・トッピングの状態から一意に決定する
normalizeBase : Int -> Order -> Order
normalizeBase index order =
    updateBaseItemAt index Okonomiyaki.normalizeBase order
