// https://adventofcode.com/2017/day/5

let rec stepThru (array: int list) position steps =
    let next = position + array.[position]
    // printfn "ap %d\tn %d\tp %d\ts %d" array.[position] next position steps
    let nextArray = array |> List.mapi (fun x y ->
        if position = x then y + 1 else y)
    if (next < 0 || next >= List.length array) then
        steps + 1
    else
        stepThru nextArray next (steps + 1)

let rec stepThruPart2 (array: int list) position steps =
    let next = position + array.[position]
    // printfn "ap %d\tn %d\tp %d\ts %d" array.[position] next position steps
    let nextArray = array |> List.mapi (fun x y ->
        if position = x then y + (if y > 2 then -1 else 1) else y)
    if (next < 0 || next >= List.length array) then
        steps + 1
    else
        stepThruPart2 nextArray next (steps + 1)

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadAllLines("input.txt") |> List.ofArray
    let values = lines |> List.map int //[0;3;0;1;-3] //
    let part1res = stepThru values 0 0
    let part2res = stepThruPart2 values 0 0
    printfn "Part 1: %d; Part 2: %d" part1res part2res
    0 // return an integer exit code
