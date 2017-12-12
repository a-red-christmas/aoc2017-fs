// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

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

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadAllLines("input.txt")
    let input = Array.map str2pipe lines
    let part1res =
        input
            |> Array.map (fun x -> part1 x.Id 0 input [-1])
            |> Array.countBy (fun x -> x)
            |> Array.filter (fun x -> fst x)
            |> Array.map (fun x-> snd x)
            |> Array.exactlyOne
    printfn "Part 1: %d; Part 2: %d" part1res -1
    
    0 // return an integer exit code
