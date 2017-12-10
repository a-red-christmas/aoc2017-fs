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

let rec run (list: int[]) cur skip inputs =
    if List.length inputs = 0 then
        list.[0] * list.[1]
    else 
        let length = List.head inputs
        let newList = tie (Array.copy list) cur length
        let newSkip = skip + 1
        let newCur = (cur + length + skip) % (ll)
        run newList newCur newSkip (List.tail inputs)

[<EntryPoint>]
let main argv =
    let input = [130;126;1;11;140;2;255;207;18;254;246;164;29;104;0;224]
    let part1res = run l 0 0 input
    printfn "%A" argv
    0 // return an integer exit code
