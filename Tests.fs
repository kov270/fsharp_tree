module Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Prog

let rec isBST cmp = function
    | Leaf -> true
    | Node(value, left, right) ->
        let leftIsBST = isBST cmp left
        let rightIsBST = isBST cmp right
        let leftIsSmaller = match left with Leaf -> true | Node(v, _, _) -> cmp v value
        let rightIsBigger = match right with Leaf -> true | Node(v, _, _) -> cmp value v
        leftIsBST && rightIsBigger && leftIsSmaller


let leftSubTree = function
    | Leaf -> Leaf
    | Node(_, l, _) -> l


let rightSubTree = function
    | Leaf -> Leaf
    | Node(_, _, r) -> r


[<Property>]
let ``associativity`` (list1: List<int>, list2: List<int>, list3: List<int>) =
    let tree1 = list1 |> List.fold (fun tree x -> insert x tree) Leaf
    let tree2 = list2 |> List.fold (fun tree x -> insert x tree) Leaf
    let tree3 = list3 |> List.fold (fun tree x -> insert x tree) Leaf

    let a = merge tree1 (merge tree2 tree3)
    let b = merge (merge tree1 tree2) tree3
    let asum = leftFold (+) 0 a
    let bsum = leftFold (+) 0 b
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


let tree = [5; 3; 7; 2; 4] |> List.fold (fun tree x -> insert x tree) Leaf

[<Fact>]
let ``leftFold calculates the sum of all values``() =
    let expected = [5; 3; 7; 2; 4] |> List.sum
    let actual = leftFold (+) 0 tree
    Assert.Equal(expected, actual)

