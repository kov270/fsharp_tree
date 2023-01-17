module Prog

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
