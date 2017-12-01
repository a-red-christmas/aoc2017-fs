// Day 1: http://adventofcode.com/2017/day/1
// warning: may not be idiomatic

let part1 (inputString: string) = 
    // to simulate a circular string, append the first character to new string
    let input = inputString + inputString.[0].ToString()
    let rec goThroughPart1 pos sum =
        if input.Length < 2 then
            sum
        else
            let toAdd = if input.[pos] = input.[pos + 1] then int (string input.[0]) else 0
            goThroughPart1 (pos + 1) (sum + toAdd)
    goThroughPart1 0 0

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
