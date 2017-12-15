// https://adventofcode.com/2017/day/15

let genAfactor = 16807UL
let genBfactor = 48271UL
let divisor = 2147483647UL

let lower16b i = i &&& 65535UL

let rec keepTryingFor x y multipleOf =
    let z = (x * y) % divisor
    if z % multipleOf = 0UL then
        z
    else
        keepTryingFor z y multipleOf

let rec generators aPrev bPrev aModReq bModReq agreed target pc =
    if pc = target then
        agreed
    else
        let newA = keepTryingFor aPrev genAfactor aModReq
        let newB = keepTryingFor bPrev genBfactor bModReq
        let newAgreed =
            if lower16b newA = lower16b newB then
                agreed + 1
            else agreed
        generators newA newB aModReq bModReq newAgreed target (pc + 1)

[<EntryPoint>]
let main argv = 
    let genAseed = 699UL
    let genBseed = 124UL
    let part1res = generators genAseed genBseed 1UL 1UL 0 40000000 0
    let part2res = generators genAseed genBseed 4UL 8UL 0 5000000 0
    printfn "Part 1: %d; Part 2: %d" part1res part2res
    assert(false)
    0 // return an integer exit code
