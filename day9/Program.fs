// https://adventofcode.com/2017/day/9

type State =
    | NoMode
    | Group
    | GarbageInGroup
    | GarbageOutGroup

let rec part1 input pc state deep score =
    if String.length input = pc then
        score
    else
        let char = input.[pc]
        if state = GarbageInGroup || state = GarbageOutGroup then
            let newState = match char with
                | '>' when state = GarbageInGroup -> Group
                | '<' when state = GarbageOutGroup -> NoMode
                | _ -> state
            let newPc = pc + if char = '!' then 2 else 1
            part1 input newPc newState deep score
        else
            printf "%c" char
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
            part1 input newPc newState newDeep newScore

[<EntryPoint>]
let main argv = 
    // "{{<a!>},{<a!>},{<a!>},{<ab>}}"
    let input = System.IO.File.ReadAllText("input.txt")
    let part1res = part1 input 0 NoMode 0 0
    printfn "%A" argv
    0 // return an integer exit code
