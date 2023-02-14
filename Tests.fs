module Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Prog

let rec isThisTreeBST = function
    | Leaf -> true
    | Node(value, left, right) ->
        let leftIsBST = isThisTreeBST left
        let rightIsBST = isThisTreeBST right
        let leftIsSmaller = match left with Leaf -> true | Node(v, _, _) -> v < value
        let rightIsBigger = match right with Leaf -> true | Node(v, _, _) -> value < v
        leftIsBST && rightIsBST && rightIsBigger && leftIsSmaller


let leftSubTree = function
    | Leaf -> Leaf
    | Node(_, l, _) -> l


let rightSubTree = function
    | Leaf -> Leaf
    | Node(_, _, r) -> r


[<Property>]
let ``associativity of merge`` (list1: List<int>, list2: List<int>, list3: List<int>) =
    let tree1 = treeFromList list1
    let tree2 = treeFromList list2
    let tree3 = treeFromList list3

    let a = merge tree1 (merge tree2 tree3)
    let b = merge (merge tree1 tree2) tree3
    // let aConnected = leftFold (fun x li -> li :: x) [] a |> List.sort
    // let bConnected = leftFold (fun x li -> li :: x) [] b |> List.sort
    (eqTrees a b)

[<Property>]
let ``merge with Empty element`` (list1: List<int>) =
    let tree1 = treeFromList list1
    let a = merge tree1 Empty
    let b = merge Empty tree1
    // let astring = leftFold (fun acc x -> acc + string x) "" a
    // let bstring = leftFold (fun acc x -> acc + string x) "" b
    eqTrees a b

[<Property>]
let ``inserting and finding an element preserves the BST property`` (value: int, list1: List<int>) =
    let tree: Tree<int> = list1 |> treeFromList
    let newTree = insert value tree
    let isBST = isThisTreeBST newTree
    let result = find value newTree
    result ==> isBST

[<Property>]
let ``deleting a value preserves the BST property`` (value: int, list1: List<int>) =
    let tree = list1 |> treeFromList
    let newTree = delete value tree
    let isBST = isThisTreeBST newTree
    isBST

[<Property>]
let ``map preserves the size of the tree`` (list1: List<int>, f: int -> int) =
    let tree = list1 |> treeFromList
    let newTree = map f tree
    let sizeTree = size tree
    let sizeNewTree = size newTree
    sizeTree = sizeNewTree

[<Property>]
let ``map preserves BST`` (list1: List<int>, f: int -> int) =
    let tree = list1 |> treeFromList
    let newTree = map f tree
    let isBST = isThisTreeBST newTree
    isBST

[<Property>]
let ``leftFold and rightFold same result on sum`` (list1: List<int>, acc: int) =
    let tree = list1 |> treeFromList
    let leftFoldRes = leftFold (+) acc tree
    let rightFoldRes = rightFold (+) acc tree
    leftFoldRes = rightFoldRes


let tree = [5; 3; 7; 2; 4] |> treeFromList

[<Fact>]
let ``leftFold calculates the sum of all values``() =
    let expected = [5; 3; 7; 2; 4] |> List.sum
    let actual = leftFold (+) 0 tree
    Assert.Equal(expected, actual)

