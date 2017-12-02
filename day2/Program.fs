// https://adventofcode.com/2017/day/2

let part1 x =
    List.map (fun y -> List.max y - List.min y) x
        |> List.sum

let part2 x =
    // iterate over the list twice to find a divisor that isn't self and clean
    let findDiv l = List.sumBy (fun y ->
        List.sumBy (fun z ->
            if y <> z && y % z = 0 then y / z else 0) l) l
    List.map findDiv x |> List.sum

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadAllLines("input.txt") |> List.ofArray
    let values = List.map (fun (x: string) ->
        x.Split('\t') |> List.ofArray |> List.map int) lines
    let part1Res = part1 values
    let part2Res = part2 values

    printfn "Part 1: %d; Part 2: %d" part1Res part2Res

    0 // return an integer exit code
