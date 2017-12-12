// https://adventofcode.com/2017/day/12

open System.Text.RegularExpressions

type Pipe =
    { Id: int; ConnectsTo: int[] }
    override m.ToString() = 
        Printf.sprintf "%d <-> %A" m.Id m.ConnectsTo

let str2pipe str =
    let r = Regex.Match(str, "(\d+) <-> (.+)")
    let id = int r.Groups.[1].Value
    let connects =
        r.Groups.[2].Value.Split([|", "|], System.StringSplitOptions.None)
            |> Array.map int
    { Id = id; ConnectsTo = connects }

// cameFrom is to prevent infinite loops
let rec part1 start finish (pipes: Pipe[]) cameFrom =
    if start = finish then
        true
    else
        let children = pipes.[start].ConnectsTo
        if Array.exists (fun x -> x = finish) children then
            true
        else
            let results =
                children
                    |> Array.filter (fun x ->
                        List.contains x cameFrom = false)
                    |> Array.map (fun x ->
                        part1 x finish pipes (start :: cameFrom))
            Array.exists (fun x -> x) results

let overlap x y = Set.intersect x y |> Set.count > 0

let rec part2 (groups: List<Set<int>>) (pipes: Pipe[]) pc =
    if Array.length pipes = pc then
        groups
    else
        let current = pipes.[pc]
        let newSet =
            current.Id :: List.ofArray current.ConnectsTo
                |> Set.ofList
        let newGroups = 
            if List.exists (fun x -> overlap x newSet) groups then
                let m = List.filter (fun x -> overlap x newSet) groups
                let foldedSet =
                    Set.union
                        newSet
                        (List.fold (fun a e -> Set.union a e) Set.empty m)
                foldedSet ::
                    List.filter (fun x -> List.contains x m = false) groups
            else
                newSet :: groups
        part2 newGroups pipes (pc + 1)

[<EntryPoint>]
let main argv = 
    let lines =
        System.IO.File.ReadAllLines("input.txt")
    let input = Array.map str2pipe lines
    let part1res =
        input
            |> Array.map (fun x -> part1 x.Id 0 input [-1])
            |> Array.countBy (fun x -> x)
            |> Array.filter (fun x -> fst x)
            |> Array.map (fun x-> snd x)
            |> Array.exactlyOne
    let part2list = part2 (List.empty<Set<int>>) input 0
    let part2res = part2list |> List.length
    printfn "Part 1: %d; Part 2: %d" part1res part2res
    
    0 // return an integer exit code
