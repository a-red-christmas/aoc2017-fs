// https://adventofcode.com/2017/day/7

open System.Text.RegularExpressions

[<StructuredFormatDisplay("{Name} ({Weight}) -> {Children}")>]
type InputTreeNode =
    { Name: string; Weight: int; Children: string array }
    override m.ToString() =
        Printf.sprintf "%s (%d) -> %A" m.Name m.Weight m.Children

let strIntoTree x =
    let r = Regex.Match(x, "([a-z]+) \((\d+)\)(?: -> ([a-z, ]*))?")
    let name = r.Groups.[1].Value
    let weight = int r.Groups.[2].Value
    let children =
        if r.Groups.[3].Success then
            r.Groups.[3].Value.Split([|", "|],
                System.StringSplitOptions.None)
        else
            [||]
    { Name = name; Weight = weight; Children = children}

let getChildren nodes node =
    Array.map (fun x ->
        Array.find (fun y -> y.Name = x) nodes) node.Children

let mode l =
    l
        |> Seq.groupBy (fun x -> x)
        |> Seq.sortByDescending (fun x -> snd x |> Seq.length)
        |> Seq.map (fun x -> fst x)
        |> Seq.head

let rec checkWeight nodes node =
    if Array.length node.Children = 0 then
        printfn "tip %s %d" node.Name node.Weight
        node.Weight
    else
        let children = getChildren nodes node
        let childrenWeight = Array.map (fun x -> checkWeight nodes x) children
        let childrenJoined = Array.zip children childrenWeight
        let childrenSum = Array.sum childrenWeight
        let totalWeight = node.Weight + childrenSum
        printfn "branch %s w %d + c %d = %d" node.Name node.Weight childrenSum totalWeight
        if Array.distinct childrenWeight |> Array.length <> 1 then
            let m = mode childrenWeight
            let i = Array.findIndex (fun x -> x <> m) childrenWeight
            let ci = children.[i]
            let wi = childrenWeight.[i]
            let x = ci.Weight + (m - wi)
            failwith "imbalance"
        else
            totalWeight

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadAllLines("input.txt")
    let input = Array.map strIntoTree lines
    let children = Array.collect (fun x -> x.Children) input
    let part1res = (Array.find (fun x ->
        Array.contains x.Name children = false) input)
    let part2res = checkWeight input part1res
    printfn "Part 1: %s; Part 2: %d" part1res.Name -1
    0 // return an integer exit code
