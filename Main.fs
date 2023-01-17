module Main

open Prog

[<EntryPoint>]
let main argv =
    let tree1 = [-1; 0] |> List.fold (fun tree x -> insert x tree) Leaf
    let tree2 = [0] |> List.fold (fun tree x -> insert x tree) Leaf
    let tree3 = Leaf
    
    let mergedTree21 = merge tree1 (merge tree2 tree3)
    let mergedTree12 = merge (merge tree1 tree2) tree3

    printfn "mergedTree12"
    rightFold (fun acc x -> printfn "%i" x; acc) () mergedTree12 |> ignore
    printfn "mergedTree21"
    rightFold (fun acc x -> printfn "%i" x; acc) () mergedTree21 |> ignore

    0
