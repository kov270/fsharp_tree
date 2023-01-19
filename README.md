## Лабораторная №2

<b>Выполнил:</b> Коваленко Илья Русланович \
<b>Группа:</b> p34102 \
<b>Преподаватель:</b> Пенской Александр Владимирович

### Двоичное дерево поиска на F#
главный тип union из Node и Leaf. Node содержит значение некоторого универсального типа T и два дочерних узла, которые также являются деревьями.

```f#
type Tree<'T> =
    | Node of 'T * Tree<'T> * Tree<'T>
    | Leaf


let rec minValue = function
    | Leaf -> failwith "minValue called on empty tree"
    | Node(v, Leaf, _) -> v
    | Node(_, l, _) -> minValue l

let rec insert value = function
    | Leaf -> Node(value, Leaf, Leaf)
    | Node(v, left, right) when value < v -> Node(v, insert value left, right)
    | Node(v, left, right) when value > v -> Node(v, left, insert value right)
    | Node(v, left, right) -> Node(v, left, right)

let rec find value = function
    | Leaf -> false
    | Node(v, left, right) when value = v -> true
    | Node(v, left, right) when value < v -> find value left
    | Node(v, left, right) -> find value right

let rec delete value = function
    | Leaf -> Leaf
    | Node(v, left, right) when value < v -> Node(v, delete value left, right)
    | Node(v, left, right) when value > v -> Node(v, left, delete value right)
    | Node(v, left, right) ->
        match left, right with
        | Leaf, Leaf -> Leaf
        | Leaf, r -> r
        | l, Leaf -> l
        | l, r ->
            let min = minValue r
            Node(min, l, delete min r)


let rec filter f = function
    | Leaf -> Leaf
    | Node (value, left, right) ->
        let newLeft = filter f left
        let newRight = filter f right
        if f value then
            Node (value, newLeft, newRight)
        else
            match newLeft, newRight with
            | Leaf, Leaf -> Leaf
            | Leaf, r -> r
            | l, Leaf -> l
            | l, r -> Node (minValue r, l, delete (minValue r) r)



let rec map f = function
    | Leaf -> Leaf
    | Node (value, left, right) ->
        let newValue = f value
        let newLeft = map f left
        let newRight = map f right
        Node (newValue, newLeft, newRight)



let rec rightFold f acc = function
    | Leaf -> acc
    | Node (value, left, right) ->
        let newAcc = f acc value
        let newRightAcc = rightFold f newAcc right
        leftFold f newRightAcc left

and leftFold f acc = function
    | Leaf -> acc
    | Node (value, left, right) ->
        let newAcc = f acc value
        let newLeftAcc = leftFold f newAcc left
        rightFold f newLeftAcc right


let rec merge t1 t2 =
    let rec mergeHelper t1 t2 acc =
        match t1, t2 with
        | Leaf, Leaf -> List.rev acc
        | Leaf, Node (value, left, right) -> mergeHelper Leaf right (value :: mergeHelper Leaf left acc)
        | Node (value, left, right), Leaf -> mergeHelper right Leaf (value :: mergeHelper left Leaf acc)
        | Node (value1, left1, right1), Node (value2, left2, right2) ->
            if value1 < value2 then
                mergeHelper right1 t2 (value1 :: mergeHelper left1 t2 acc)
            else
                mergeHelper t1 right2 (value2 :: mergeHelper t1 left2 acc)

    let sortedList = mergeHelper t1 t2 []
    sortedList |> List.fold (fun tree x -> insert x tree) Leaf


let Empty = Leaf


let rec size = function
    | Leaf -> 0
    | Node(_, left, right) -> 1 + size left + size right

```

### Тесты
PBT тесты при помощи FsCheck

```f#
[<Property>]
let ``associativity`` (list1: List<int>, list2: List<int>, list3: List<int>) =
    let tree1 = list1 |> List.fold (fun tree x -> insert x tree) Leaf
    let tree2 = list2 |> List.fold (fun tree x -> insert x tree) Leaf
    let tree3 = list3 |> List.fold (fun tree x -> insert x tree) Leaf

    let a = merge tree1 (merge tree2 tree3)
    let b = merge (merge tree1 tree2) tree3
    let asum = leftFold (+) 0 a
    let bsum = leftFold (+) 0 b
    // printfn "%A %A" astring bstring
    asum = bsum

[<Property>]
let ``merge with Empty element`` (tree1: Tree<int>) =
    let a = merge tree1 Empty
    let b = merge Empty tree1
    let astring = leftFold (fun acc x -> acc + string x) "" a
    let bstring = leftFold (fun acc x -> acc + string x) "" b
    astring = bstring

[<Property>]
let ``inserting and finding an element preserves the BST property`` (value: int, list1: List<int>) =
    let tree: Tree<int> = list1 |> List.fold (fun tree x -> insert x tree) Leaf
    let newTree = insert value tree
    let isBST = isBST (<) newTree
    let result = find value newTree
    result ==> isBST

[<Property>]
let ``deleting a value preserves the BST property`` (value: int, list1: List<int>) =
    let tree = list1 |> List.fold (fun tree x -> insert x tree) Leaf
    let newTree = delete value tree
    let isBST = isBST (<) newTree
    isBST

[<Property>]
let ``map preserves the size of the tree`` (list1: List<int>, f: int -> int) =
    let tree = list1 |> List.fold (fun tree x -> insert x tree) Leaf
    let newTree = map f tree
    let sizeTree = size tree
    let sizeNewTree = size newTree
    sizeTree = sizeNewTree

[<Property>]
let ``leftFold and rightFold same result on sum`` (list1: List<int>, acc: int) =
    let tree = list1 |> List.fold (fun tree x -> insert x tree) Leaf
    let leftFoldRes = leftFold (+) acc tree
    let rightFoldRes = rightFold (+) acc tree
    leftFoldRes = rightFoldRes

```

<https://github.com/kov270/fsharp_tree/actions/runs/3942512043/jobs/6746251385>

```
Passed!  - Failed:     0, Passed:     7, Skipped:     0, Total:     7, Duration: 409 ms - Laba2.dll (net7.0)
```

### Выводы
property-based testing - позволяет автоматически генерировать большое количество тестовых случаев на основе свойств или требований. Были протестирвоанный все ствойства от А до Б, такие как свойства моноида - ассоциативность, наличие нейтрального элемента, свойства вставки и удаления, которые сохраняют упорядоченность элементов в дереве, и т.д. В ходе лабораторной работы я научилися использовать property-based testing и осознал эффективность такого подхода.
