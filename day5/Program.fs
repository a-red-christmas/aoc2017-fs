// https://adventofcode.com/2017/day/5

let rec stepThru (array: int array) position steps =
    let next = position + array.[position]
    array.[position] <- array.[position] + 1
    if next < 0 || next >= Array.length array then
        steps + 1
    else
        stepThru array next (steps + 1)

let rec stepThruPart2 (array: int array) position steps =
    let next = position + array.[position]
    array.[position] <- array.[position]
        + if array.[position] > 2 then -1 else 1
    if next < 0 || next >= Array.length array then
        steps + 1
    else
        stepThruPart2 array next (steps + 1)

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadAllLines("input.txt")
    let values = lines |> Array.map int
    let part1res = stepThru (Array.copy values) 0 0
    let part2res = stepThruPart2 (Array.copy values) 0 0
    printfn "Part 1: %d; Part 2: %d" part1res part2res
    0 // return an integer exit code
