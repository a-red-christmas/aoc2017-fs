// https://adventofcode.com/2017/day/9

type State =
    | NoMode
    | Group
    | GarbageInGroup
    | GarbageOutGroup

let rec run input pc state deep score garbage =
    if String.length input = pc then
        (score, garbage)
    else
        let char = input.[pc]
        if state = GarbageInGroup || state = GarbageOutGroup then
            let newState = match char with
                | '>' when state = GarbageInGroup -> Group
                | '<' when state = GarbageOutGroup -> NoMode
                | _ -> state
            let newGarbage = garbage + match char with
                | '!' | '>' -> 0
                | _ -> 1
            let newPc = pc + if char = '!' then 2 else 1
            run input newPc newState deep score newGarbage
        else
            let newScore = if state = Group && char = '}' then score + deep else score
            let newDeep = deep + match char with
                | '{' -> 1
                | '}' -> -1
                | _ -> 0
            let newState = match char with
                | '{' -> Group
                | '<' when state = Group -> GarbageInGroup
                | '<' when state <> Group -> GarbageOutGroup
                | '}' when state = Group && deep = 1 -> NoMode
                | _ -> state
            let newPc = pc + if char = '!' then 2 else 1
            run input newPc newState newDeep newScore garbage

[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllText("input.txt")
    let (part1res, part2res) = run input 0 NoMode 0 0 0
    printfn "Part 1: %d; Part 2: %d" part1res part2res
    0