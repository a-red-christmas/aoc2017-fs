// https://adventofcode.com/2017/day/10

let l = [|0..255|]
let ll = Array.length l

// HACK: horrific "circular" array reversing code I could understand, thanks to
// pay.reddit.com/r/adventofcode/comments/7irzg5/2017_day_10_solutions/dr11jzy/
let tie (list: int[]) cur length =
    let subList = Array.create<int> length 0
    for i = 0 to length - 1 do
        subList.[i] <- list.[(cur + i) % ll]
    let revSubList = Array.rev subList
    for i = 0 to length - 1 do
        list.[(cur + i) % ll] <- revSubList.[i]
    list

let rec run (list: int[]) cur skip lengths =
    if List.length lengths = 0 then
        (list, cur, skip)
    else 
        let length = List.head lengths
        let newList = tie (Array.copy list) cur length
        let newSkip = skip + 1
        let newCur = (cur + length + skip) % ll
        run newList newCur newSkip (List.tail lengths)

let run2 list lengths =
    let appendedInput =
        List.append lengths [ 17; 31; 73; 47; 23 ]
    // get part 1 a few times
    let rec run2internal list cur skip input rounds = 
        let (newList, newCur, newSkip) = run list cur skip input
        if rounds = 0 then
            newList
        else
            run2internal newList newCur newSkip input (rounds - 1)
    let sparseHash = run2internal list 0 0 appendedInput 63
    let denseHash = sparseHash |> Array.chunkBySize 16
    let denseProcessed =
        Array.map (fun x ->
            Array.fold (fun a e ->
                a ^^^ e) (Array.head x) (Array.tail x)) denseHash
    let denseFormatted =
        Array.fold (fun a e -> a + Printf.sprintf "%02x" e) "" denseProcessed
    denseFormatted

let str2byte s = 
    Seq.cast<char> s |> Seq.map (fun x -> int x) |> Seq.toList

[<EntryPoint>]
let main argv =
    let inputPart1 =
        [130;126;1;11;140;2;255;207;18;254;246;164;29;104;0;224]
    let inputPart2 = //str2byte "AoC 2017"
        str2byte "130,126,1,11,140,2,255,207,18,254,246,164,29,104,0,224"
    let (part1list, part1cur, part1skip) = run l 0 0 inputPart1
    let part1res = part1list.[0] * part1list.[1]
    let part2res = run2 l inputPart2
    printfn "Part 1: %d; Part 2: %s" part1res part2res
    0 // return an integer exit code
