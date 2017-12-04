// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let part1 z =
    let values = z |> List.map (fun x ->
        List.distinct x = x) |> List.filter (fun y -> y)
    List.length values

let part2 z =
    let sort (x: string) = x |> Seq.sort |> System.String.Concat
    let values =
        List.map (fun x -> List.map (fun y -> sort y) x) z
        |> List.map (fun x -> List.distinct x = x)
        |> List.filter (fun x -> x)
    List.length values

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadAllLines("input.txt") |> List.ofArray
    let split = List.map (fun (x: string) ->
        x.Split(' ') |> List.ofArray) lines
    let part1res = part1 split 
    let part2res = part2 split 
    printfn "Part 1: %d; Part 2: %d" part1res part2res
    0 // return an integer exit code
