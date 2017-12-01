// Day 1: http://adventofcode.com/2017/day/1
// warning: may not be idiomatic

let part1 (inputString: string) = 
    // to simulate a circular string, append the first character to new string
    let input = inputString + string inputString.[0]
    // seq it
    Seq.map (fun x -> string x |> int) input
        |> Seq.pairwise
        |> Seq.map (fun y -> if fst y = snd y then fst y else 0)
        |> Seq.sum

let part2 (inputString: string) = 
    // to simulate a circular string, this time just append itself and iterate
    // through the undoubled string
    let inputCircular = inputString + inputString
    let lookAheadBy = inputString.Length / 2
    let rec goThroughPart2 pos (sum: int) =
        if (pos = inputString.Length) then
            sum
        else
            let toAdd = if (inputString.[pos] = inputCircular.[pos + lookAheadBy]) then int (string inputString.[pos]) else 0
            goThroughPart2 (pos + 1) (sum + toAdd)
    goThroughPart2 0 0

[<EntryPoint>]
let main argv = 
    let inputString = if argv.Length > 0 then argv.[0] else "523425"
    let part1Res = part1 inputString
    let part2Res = part2 inputString

    printfn "Part 1: %d; Part 2: %d" part1Res part2Res

    0 // return an integer exit code
