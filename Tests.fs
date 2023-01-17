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
let ``inserting and finding an element preserves the BST property`` (value: int, tree: Tree<int>) =
    let newTree = insert value tree
    let leftIsBST = isBST (<) (leftSubTree newTree)
    let rightIsBST = isBST (>) (rightSubTree newTree)
    let result = find value newTree
    let isBST = leftIsBST && rightIsBST
    result ==> isBST


[<Property>]
let ``deleting a value preserves the BST property`` (value: int, tree: Tree<int>) =
    let newTree = delete value tree
    let leftIsBST = isBST (<) (leftSubTree newTree)
    let rightIsBST = isBST (>) (rightSubTree newTree)
    let isBST = leftIsBST && rightIsBST
    isBST


let tree = [5; 3; 7; 2; 4] |> List.fold (fun tree x -> insert x tree) Leaf

[<Fact>]
let ``leftFold calculates the sum of all values``() =
    let expected = [5; 3; 7; 2; 4] |> List.sum
    let actual = leftFold (fun acc x -> acc + x) 0 tree
    Assert.Equal(expected, actual)




// Check.QuickThrowOnFailure ``inserting and finding an element preserves the BST property``
// Check.QuickThrowOnFailure ``deleting a value preserves the BST property``
