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


// // Insert some values into the tree
// let tree = [5; 3; 7; 2; 4] |> List.fold (fun tree x -> insert x tree) Leaf

// // Check if a value is in the tree
// printfn "Is 5 in the tree? %b" (find 5 tree)  // Output: Is 5 in the tree? true
// printfn "Is 6 in the tree? %b" (find 6 tree)  // Output: Is 6 in the tree? false


// let newTree = delete 5 tree

// // Check if the value is still in the tree
// printfn "Is 5 in the tree? %b" (find 5 newTree)  // Output: Is 5 in the tree? false



// let newTree = filter (fun x -> x <= 4) tree

// // Check if the values greater than 4 are removed
// printfn "Is 5 in the tree? %b" (find 5 newTree)  // Output: Is 5 in the tree? false
// printfn "Is 4 in the tree? %b" (find 4 newTree)  // Output: Is 4 in the tree? true

