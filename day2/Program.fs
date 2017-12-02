// https://adventofcode.com/2017/day/2

let part1 x =
    List.map (fun y -> List.max y - List.min y) x
        |> List.sum

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadAllLines("input.txt") |> List.ofArray
    let values = List.map (fun (x: string) -> x.Split('\t') |> List.ofArray |> List.map int) lines
    let part1Res = part1 values
    let part2Res = -1

    printfn "Part 1: %d; Part 2: %d" part1Res part2Res

    0 // return an integer exit code
