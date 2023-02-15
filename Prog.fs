module Prog


type Tree<'T> = 
  | Leaf 
  | Node of 'T * Tree<'T> * Tree<'T>

let rec toSeq (t: Tree<'T>) : seq<'T> =
    match t with
    | Leaf -> Seq.empty
    | Node (v, l, r) -> seq {
        yield! toSeq l
        yield v
        yield! toSeq r
    }

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


let map f = function
    | Leaf -> Leaf
    | Node (value, left, right) -> leftFold (fun tree x -> insert x tree) Leaf (Node (value, left, right))


let Empty = Leaf


let rec size = function
    | Leaf -> 0
    | Node(_, left, right) -> 1 + size left + size right


let treeFromList list = List.fold (fun tree x -> insert x tree) Leaf list


let merge t1 t2 = treeFromList (Seq.append (toSeq t1) (toSeq t2) |> Seq.toList)
// let merge t1 t2 =
//     let rec mergeHelper t1 t2 acc =
//         match t1, t2 with
//         | Leaf, Leaf -> List.rev acc
//         | Leaf, Node (value, left, right) -> mergeHelper Leaf right (value :: mergeHelper Leaf left acc)
//         | Node (value, left, right), Leaf -> mergeHelper right Leaf (value :: mergeHelper left Leaf acc)
//         | Node (value1, left1, right1), Node (value2, left2, right2) ->
//             if value1 < value2 then
//                 mergeHelper right1 t2 (value1 :: mergeHelper left1 t2 acc)
//             else
//                 mergeHelper t1 right2 (value2 :: mergeHelper t1 left2 acc)

//     let sortedList = mergeHelper t1 t2 []
//     sortedList |> List.fold (fun tree x -> insert x tree) Leaf


let rec strongEqTrees t1 t2 =
    match t1, t2 with
    | Leaf, Leaf -> true
    | Leaf, Node _ | Node _, Leaf -> false
    | Node (value1, left1, right1), Node (value2, left2, right2) ->
        value1 = value2 && strongEqTrees left1 left2 && strongEqTrees right1 right2


// let eqTrees t1 t2 =
//     (size t1 = size t2) && (leftFold (fun acc x -> acc && (find x t2)) true t1)

let eqTrees t1 t2 =
    Seq.zip (toSeq t1) (toSeq t2) |> Seq.forall (fun (x, y) -> x = y)
