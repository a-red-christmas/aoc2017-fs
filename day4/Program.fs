// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadAllLines("input.txt") |> List.ofArray
    let split = List.map (fun (x: string) ->
        x.Split(' ') |> List.ofArray) lines
    let values = List.map (fun x ->
        List.distinct x = x) split |> List.filter (fun y -> y)
    let part1 = List.length values
    printfn "Part 1: %d" part1
    0 // return an integer exit code
