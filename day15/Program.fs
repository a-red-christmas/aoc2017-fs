// https://adventofcode.com/2017/day/15

let genAfactor = 16807UL
let genBfactor = 48271UL
let divisor = 2147483647UL

let lower16b i = i &&& 65535UL

let rec generators aPrev bPrev agreed target pc =
    if pc = target then
        agreed
    else
        let newA = (aPrev * genAfactor) % divisor
        let newB = (bPrev * genBfactor) % divisor
        let newAgreed =
            if lower16b newA = lower16b newB then
                agreed + 1
            else agreed
        generators newA newB newAgreed target (pc + 1)

[<EntryPoint>]
let main argv = 
    let genAseed = 699UL
    let genBseed = 124UL
    let part1res = generators genAseed genBseed 0 40000000 0
    printfn "Part 1: %d;" part1res
    0 // return an integer exit code
