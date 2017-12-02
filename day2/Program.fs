// https://adventofcode.com/2017/day/2

let part1 (x: int list list) =
    List.map (fun y -> List.max y - List.min y) x
        |> List.sum

[<EntryPoint>]
let main argv = 
    let values = [[5; 1; 9; 5]; [7; 5; 3]; [2; 4; 6; 8]]
    let part1Res = part1 values
    let part2Res = -1

    printfn "Part 1: %d; Part 2: %d" part1Res part2Res

    0 // return an integer exit code
