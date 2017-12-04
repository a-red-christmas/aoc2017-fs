// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let part1 z =
    let values = z |> List.map (fun x ->
        List.distinct x = x) |> List.filter (fun y -> y)
    List.length values

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadAllLines("input.txt") |> List.ofArray
    let split = List.map (fun (x: string) ->
        x.Split(' ') |> List.ofArray) lines
    let part1res = part1 split 
    printfn "Part 1: %d" part1res
    0 // return an integer exit code
