module Order exposing (Order, OrderItemType(..), StandaloneOrderItem, addBaseItem, addStandaloneItem, addNoodleToLastBase, addToppingToLastBase, calculateTotal, emptyOrder, incrementBaseQuantity, decrementBaseQuantity, incrementNoodleQuantity, decrementNoodleQuantity, toggleTopping, incrementStandaloneQuantity, decrementStandaloneQuantity, normalizeBaseOnNoodleChange, normalizeBaseOnNoodleAdd, normalizeBaseOnToppingChange)

import Menu exposing (MenuItem)
import Okonomiyaki exposing (BaseOrderItem, Noodle, Topping, ToppingKind)


-- 独立した商品（焼き物、飲み物）
type alias StandaloneOrderItem =
    { menuItem : MenuItem
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


-- 新しいお好み焼きを追加
addBaseItem : Okonomiyaki.OkonomiyakiBase -> Order -> Order
addBaseItem base order =
    { order | items = order.items ++ [ BaseOrder (Okonomiyaki.initialBaseOrderItem base) ] }


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

                    _ ->
                        Nothing
            )
        |> List.head


-- 最後のお好み焼きに麺を追加（既存の麺を置き換え）
addNoodleToLastBase : Noodle -> Order -> Order
addNoodleToLastBase noodle order =
    case getLastBaseItemIndex order of
        Just idx ->
            addNoodleToBase idx noodle order

        Nothing ->
            -- お好み焼きがない場合は何もしない
            order


-- 特定インデックスのお好み焼きに麺を追加（既存あれば数量+1、なければ新規追加）
addNoodleToBase : Int -> Noodle -> Order -> Order
addNoodleToBase index noodle order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == index then
                        case item of
                            BaseOrder baseItem ->
                                let
                                    existingNoodle =
                                        baseItem.noodles
                                            |> List.filter (\n -> n.noodle.kind == noodle.kind)
                                            |> List.head
                                in
                                case existingNoodle of
                                    Just _ ->
                                        BaseOrder
                                            { baseItem
                                                | noodles =
                                                    List.map
                                                        (\n ->
                                                            if n.noodle.kind == noodle.kind then
                                                                { n | quantity = n.quantity + 1 }

                                                            else
                                                                n
                                                        )
                                                        baseItem.noodles
                                            }

                                    Nothing ->
                                        BaseOrder
                                            { baseItem
                                                | noodles = baseItem.noodles ++ [ { noodle = noodle, quantity = 2 } ]
                                            }

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


-- 最後のお好み焼きにトッピングを追加
addToppingToLastBase : Topping -> Order -> Order
addToppingToLastBase toppingItem order =
    case getLastBaseItemIndex order of
        Just idx ->
            addToppingToBase idx toppingItem order

        Nothing ->
            order


-- 特定インデックスのお好み焼きにトッピングを追加
addToppingToBase : Int -> Topping -> Order -> Order
addToppingToBase index toppingItem order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == index then
                        case item of
                            BaseOrder baseItem ->
                                let
                                    existingTopping =
                                        baseItem.toppings
                                            |> List.filter (\t -> t.topping.kind == toppingItem.kind)
                                            |> List.head
                                in
                                case existingTopping of
                                    Just _ ->
                                        -- 既存のトッピングがあれば数量+1
                                        BaseOrder
                                            { baseItem
                                                | toppings =
                                                    List.map
                                                        (\t ->
                                                            if t.topping.kind == toppingItem.kind then
                                                                { t | quantity = t.quantity + 1 }

                                                            else
                                                                t
                                                        )
                                                        baseItem.toppings
                                            }

                                    Nothing ->
                                        -- 新しいトッピングを追加
                                        BaseOrder
                                            { baseItem
                                                | toppings = baseItem.toppings ++ [ { topping = toppingItem, quantity = 1 } ]
                                            }

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


-- 特定インデックスのお好み焼きからトッピングを削除
removeToppingFromBase : Int -> ToppingKind -> Order -> Order
removeToppingFromBase index toppingKind order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == index then
                        case item of
                            BaseOrder baseItem ->
                                BaseOrder
                                    { baseItem
                                        | toppings =
                                            List.filter
                                                (\t -> t.topping.kind /= toppingKind)
                                                baseItem.toppings
                                    }

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


-- トッピングのトグル（未選択なら追加、選択済みなら削除）
toggleTopping : Int -> Topping -> Order -> Order
toggleTopping index toppingItem order =
    let
        hasTopping =
            order.items
                |> List.drop index
                |> List.head
                |> Maybe.andThen
                    (\item ->
                        case item of
                            BaseOrder baseItem ->
                                if List.any (\t -> t.topping.kind == toppingItem.kind) baseItem.toppings then
                                    Just True

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
    in
    case hasTopping of
        Just True ->
            removeToppingFromBase index toppingItem.kind order

        _ ->
            addToppingToBase index toppingItem order


-- お好み焼きの数量を増やす
incrementBaseQuantity : Int -> Order -> Order
incrementBaseQuantity index order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == index then
                        case item of
                            BaseOrder baseItem ->
                                BaseOrder { baseItem | quantity = baseItem.quantity + 1 }

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
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == index then
                        case item of
                            BaseOrder baseItem ->
                                BaseOrder { baseItem | quantity = baseItem.quantity - 1 }

                            _ ->
                                item

                    else
                        item
                )
                order.items
                |> List.filterMap
                    (\item ->
                        case item of
                            BaseOrder baseItem ->
                                if baseItem.quantity > 0 then
                                    Just item

                                else
                                    Nothing

                            _ ->
                                Just item
                    )
    }


-- 麺の数量を増やす（0.5玉単位、内部的に+1、新規追加にも対応）
incrementNoodleQuantity : Int -> Noodle -> Order -> Order
incrementNoodleQuantity baseIndex noodle order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == baseIndex then
                        case item of
                            BaseOrder baseItem ->
                                let
                                    existingNoodle =
                                        baseItem.noodles
                                            |> List.filter (\n -> n.noodle.kind == noodle.kind)
                                            |> List.head
                                in
                                case existingNoodle of
                                    Just _ ->
                                        -- 既存の麺があれば数量+1（0.5玉増加）
                                        BaseOrder
                                            { baseItem
                                                | noodles =
                                                    List.map
                                                        (\n ->
                                                            if n.noodle.kind == noodle.kind then
                                                                { n | quantity = n.quantity + 1 }

                                                            else
                                                                n
                                                        )
                                                        baseItem.noodles
                                            }

                                    Nothing ->
                                        -- 新規麺を追加（quantity = 2で初期化 = 1玉）
                                        BaseOrder
                                            { baseItem
                                                | noodles = baseItem.noodles ++ [ { noodle = noodle, quantity = 2 } ]
                                            }

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


-- 麺の数量を減らす（0.5玉単位、内部的に-1。0になったら削除）
decrementNoodleQuantity : Int -> Noodle -> Order -> Order
decrementNoodleQuantity baseIndex noodle order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == baseIndex then
                        case item of
                            BaseOrder baseItem ->
                                BaseOrder
                                    { baseItem
                                        | noodles =
                                            List.map
                                                (\n ->
                                                    if n.noodle.kind == noodle.kind then
                                                        { n | quantity = max 0 (n.quantity - 1) }

                                                    else
                                                        n
                                                )
                                                baseItem.noodles
                                                |> List.filter (\n -> n.quantity > 0)
                                    }

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


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
                    BaseOrder baseItem ->
                        Okonomiyaki.calculateBaseItemTotal baseItem

                    StandaloneOrder standaloneItem ->
                        standaloneItem.menuItem.price * standaloneItem.quantity
            )
        |> List.sum


-- 麺の「＋」操作後に呼ぶ。baseYasai（includedNoodleKind なし）の場合に追加した麺に応じてベースを切り替える
normalizeBaseOnNoodleAdd : Int -> Noodle -> Order -> Order
normalizeBaseOnNoodleAdd index noodle order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == index then
                        case item of
                            BaseOrder baseItem ->
                                BaseOrder (Okonomiyaki.normalizeBaseOnNoodleAdd noodle baseItem)

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


-- 麺の「−」操作後に呼ぶ。includedNoodleKind が0玉になった場合に baseItem を baseYasai に切り替える
normalizeBaseOnNoodleChange : Int -> Order -> Order
normalizeBaseOnNoodleChange index order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == index then
                        case item of
                            BaseOrder baseItem ->
                                BaseOrder (Okonomiyaki.normalizeBaseOnNoodleChange baseItem)

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


-- トッピングの追加・削除後に呼ぶ。全部入り条件の成立・解除に応じてベースを切り替える
normalizeBaseOnToppingChange : Int -> Order -> Order
normalizeBaseOnToppingChange index order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == index then
                        case item of
                            BaseOrder baseItem ->
                                BaseOrder (Okonomiyaki.normalizeBaseOnToppingChange baseItem)

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }
