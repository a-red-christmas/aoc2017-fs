// https://adventofcode.com/2017/day/6

let nextPos array from =
    let len = Array.length array
    if from + 1 = len then 0 else from + 1

let rec allocBlocks (array: int[]) pos blocks =
    if blocks = 0 then
        pos
    else
        array.[pos] <- array.[pos] + 1
        allocBlocks array (nextPos array pos) (blocks - 1)

let rec realloc seenStack array steps =
    // freeze array state
    let newSeenStack = List.append [Array.copy array] seenStack
    // find largest and drain it
    let x = Array.max array
    let xPos = Array.findIndex (fun y -> x = y) array
    array.[xPos] <- 0
    let newPos = allocBlocks array (nextPos array xPos) x
    // could check newSeenStack for distinct as well
    if List.contains array seenStack then
        let distance = List.findIndex (fun y -> y = array) newSeenStack
        (steps + 1, distance + 1)
    else
        realloc newSeenStack array (steps + 1)

[<EntryPoint>]
let main argv = 
    let blocks = //[| 0; 2; 7; 0|]
        [| 11; 11; 13; 7; 0 ; 15; 5; 5; 4; 4; 1; 1; 7; 1; 15; 11|]
    let (steps, distance) = realloc [] blocks 0
    let part1res = steps
    let part2res = distance
    printfn "Part 1: %d; Part 2: %d" part1res part2res
    0 // return an integer exit code
