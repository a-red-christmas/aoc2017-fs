// https://adventofcode.com/2017/day/24

type Node =
    | Leaf of int * int
    | Branch of (int * int) * Node list

let parse line =
    let r =
        System.Text.RegularExpressions.Regex.Match(line, "(\d+)/(\d+)")
    (int r.Groups.[1].Value, int r.Groups.[2].Value)

let swap (x, y) = (y, x)

let rec buildNodes pipes current =
    let matches =
        pipes
            |> List.filter (fun x ->
                (fst x = snd current || snd x = snd current)
                    && x <> current && swap x <> current)
            // since we can match either end, keep it simple and only
            // match on snd each time; do this by swapping if on snd
            |> List.map (fun x ->
                if snd x = snd current then swap x else x)
    if List.length matches = 0 then
        Leaf current
    else
        let newPipes =
            List.filter (fun x ->
                 (x = current || swap x = current) = false) pipes
        let builtBranches = List.map (buildNodes newPipes) matches
        Branch (current, builtBranches)

let buildTrunks pipes =
    pipes
        |> List.filter (fun x -> fst x = 0)
        |> List.map (buildNodes (List.filter (fun x -> fst x <> 0) pipes))

let indent x = Seq.replicate x '\t' |> System.String.Concat

let rec printTree i n =
    let is = indent i
    match n with
        | Leaf (x, y) -> printfn "%sLeaf %d/%d" is x y
        | Branch ((x, y), l) ->
            printfn "%sBranch %d/%d" is x y
            List.iter (printTree (i + 1)) l 

let rec sumNodes = function
    | Leaf (x, y) -> x + y
    | Branch ((x, y), l) ->
        x + y + (List.map sumNodes l |> List.max)

[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllLines("input.txt")
    let parts = input |> Array.map parse |> Array.toList
    let tree = buildTrunks parts
    //List.iter (printTree 0) tree
    let part1res =
        tree
            |> List.map sumNodes
            |> List.max
    printfn "Part 1: %d" part1res
    0 // return an integer exit code
