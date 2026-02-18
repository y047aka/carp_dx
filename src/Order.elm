module Order exposing (Order, OrderItemType(..), BaseOrderItem, StandaloneOrderItem, Addition, addBaseItem, addStandaloneItem, addNoodleToLastBase, addToppingToLastBase, addNoodleToBase, removeNoodleFromBase, calculateTotal, calculateBaseItemTotal, calculateAdditionPrice, emptyOrder, incrementBaseQuantity, decrementBaseQuantity, incrementNoodleQuantity, decrementNoodleQuantity, noodleQuantityDisplay, toggleTopping, incrementStandaloneQuantity, decrementStandaloneQuantity, getLastBaseItemIndex)

import Menu exposing (MenuItem(..), menuItemId)


-- お好み焼きに付属する追加オプション（麺、トッピング）
type alias Addition =
    { menuItem : MenuItem
    , quantity : Int
    }


-- お好み焼きベースの注文アイテム
type alias BaseOrderItem =
    { baseItem : MenuItem          -- お好み焼き本体
    , quantity : Int               -- お好み焼きの数量
    , noodles : List Addition       -- 麺（複数可）
    , toppings : List Addition     -- トッピング（複数可）
    }


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
addBaseItem : MenuItem -> Order -> Order
addBaseItem menuItem order =
    let
        newBaseItem =
            { baseItem = menuItem
            , quantity = 1
            , noodles = []
            , toppings = []
            }
    in
    { order | items = order.items ++ [ BaseOrder newBaseItem ] }


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
                                if menuItemId standaloneItem.menuItem == menuItemId menuItem then
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
addNoodleToLastBase : MenuItem -> Order -> Order
addNoodleToLastBase noodleItem order =
    case getLastBaseItemIndex order of
        Just idx ->
            addNoodleToBase idx noodleItem order

        Nothing ->
            -- お好み焼きがない場合は何もしない
            order


-- 特定インデックスのお好み焼きに麺を追加（既存あれば数量+1、なければ新規追加）
addNoodleToBase : Int -> MenuItem -> Order -> Order
addNoodleToBase index noodleItem order =
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
                                            |> List.filter (\n -> menuItemId n.menuItem == menuItemId noodleItem)
                                            |> List.head
                                in
                                case existingNoodle of
                                    Just _ ->
                                        BaseOrder
                                            { baseItem
                                                | noodles =
                                                    List.map
                                                        (\n ->
                                                            if menuItemId n.menuItem == menuItemId noodleItem then
                                                                { n | quantity = n.quantity + 1 }

                                                            else
                                                                n
                                                        )
                                                        baseItem.noodles
                                            }

                                    Nothing ->
                                        BaseOrder
                                            { baseItem
                                                | noodles = baseItem.noodles ++ [ { menuItem = noodleItem, quantity = 2 } ]
                                            }

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


-- 最後のお好み焼きにトッピングを追加
addToppingToLastBase : MenuItem -> Order -> Order
addToppingToLastBase toppingItem order =
    case getLastBaseItemIndex order of
        Just idx ->
            addToppingToBase idx toppingItem order

        Nothing ->
            order


-- 特定インデックスのお好み焼きにトッピングを追加
addToppingToBase : Int -> MenuItem -> Order -> Order
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
                                            |> List.filter (\t -> menuItemId t.menuItem == menuItemId toppingItem)
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
                                                            if menuItemId t.menuItem == menuItemId toppingItem then
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
                                                | toppings = baseItem.toppings ++ [ { menuItem = toppingItem, quantity = 1 } ]
                                            }

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


-- 特定インデックスのお好み焼きから麺を削除
removeNoodleFromBase : Int -> String -> Order -> Order
removeNoodleFromBase index noodleId order =
    { order
        | items =
            List.indexedMap
                (\i item ->
                    if i == index then
                        case item of
                            BaseOrder baseItem ->
                                BaseOrder
                                    { baseItem
                                        | noodles =
                                            List.filter
                                                (\n -> menuItemId n.menuItem /= noodleId)
                                                baseItem.noodles
                                    }

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


-- 特定インデックスのお好み焼きからトッピングを削除
removeToppingFromBase : Int -> String -> Order -> Order
removeToppingFromBase index toppingId order =
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
                                                (\t -> menuItemId t.menuItem /= toppingId)
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
toggleTopping : Int -> MenuItem -> Order -> Order
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
                                if List.any (\t -> menuItemId t.menuItem == menuItemId toppingItem) baseItem.toppings then
                                    Just True

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
    in
    case hasTopping of
        Just True ->
            removeToppingFromBase index (menuItemId toppingItem) order

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
incrementNoodleQuantity : Int -> MenuItem -> Order -> Order
incrementNoodleQuantity baseIndex noodleItem order =
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
                                            |> List.filter (\n -> menuItemId n.menuItem == menuItemId noodleItem)
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
                                                            if menuItemId n.menuItem == menuItemId noodleItem then
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
                                                | noodles = baseItem.noodles ++ [ { menuItem = noodleItem, quantity = 2 } ]
                                            }

                            _ ->
                                item

                    else
                        item
                )
                order.items
    }


-- 麺の数量を減らす（0.5玉単位、内部的に-1。0になったら削除）
decrementNoodleQuantity : Int -> String -> Order -> Order
decrementNoodleQuantity baseIndex noodleId order =
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
                                                    if menuItemId n.menuItem == noodleId then
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


-- 麺の内部quantity値を表示用文字列に変換（1→"0.5", 2→"1", 3→"1.5"...）
noodleQuantityDisplay : Int -> String
noodleQuantityDisplay internalQuantity =
    let
        wholePart =
            internalQuantity // 2

        hasHalf =
            modBy 2 internalQuantity /= 0
    in
    if hasHalf then
        if wholePart == 0 then
            "0.5"

        else
            String.fromInt wholePart ++ ".5"

    else
        String.fromInt wholePart


-- 独立商品の数量を増やす
incrementStandaloneQuantity : String -> Order -> Order
incrementStandaloneQuantity targetId order =
    { order
        | items =
            List.map
                (\item ->
                    case item of
                        StandaloneOrder standaloneItem ->
                            if menuItemId standaloneItem.menuItem == targetId then
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
                            if menuItemId standaloneItem.menuItem == targetId then
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


-- 追加オプションの価格計算（MenuItem の種類に応じて計算方法を切り替え）
calculateAdditionPrice : MenuItem -> Int -> Int
calculateAdditionPrice item quantity =
    case item of
        StandardItem r ->
            r.price * quantity

        NoodleItem r ->
            r.basePrice + r.pricePerHalfBall * quantity


-- お好み焼きベースの小計計算（本体 + 麺 + トッピング）× お好み焼きの数量
calculateBaseItemTotal : BaseOrderItem -> Int
calculateBaseItemTotal baseItem =
    let
        basePrice =
            case baseItem.baseItem of
                StandardItem r ->
                    r.price

                NoodleItem r ->
                    r.basePrice

        noodlePrice =
            let
                totalNoodleQuantity =
                    baseItem.noodles
                        |> List.map .quantity
                        |> List.sum
            in
            if totalNoodleQuantity == 0 then
                0

            else
                100 + 100 * totalNoodleQuantity

        toppingsPrice =
            baseItem.toppings
                |> List.map (\t -> calculateAdditionPrice t.menuItem t.quantity)
                |> List.sum
    in
    (basePrice + noodlePrice + toppingsPrice) * baseItem.quantity


-- 注文全体の合計金額
calculateTotal : Order -> Int
calculateTotal order =
    order.items
        |> List.map
            (\item ->
                case item of
                    BaseOrder baseItem ->
                        calculateBaseItemTotal baseItem

                    StandaloneOrder standaloneItem ->
                        calculateAdditionPrice standaloneItem.menuItem standaloneItem.quantity
            )
        |> List.sum
