// https://adventofcode.com/2017/day/14

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

let binaryString (s: string) =
    // HACK: probably a nicer way to do it
    s.ToLower()
        |> Seq.map (fun x ->
            match x with
                | '0' -> "0000"
                | '1' -> "0001"
                | '2' -> "0010"
                | '3' -> "0011"
                | '4' -> "0100"
                | '5' -> "0101"
                | '6' -> "0110"
                | '7' -> "0111"
                | '8' -> "1000"
                | '9' -> "1001"
                | 'a' -> "1010"
                | 'b' -> "1011"
                | 'c' -> "1100"
                | 'd' -> "1101"
                | 'e' -> "1110"
                | 'f' -> "1111"
                | _ -> failwith "invalid char")
        |> System.String.Concat

[<EntryPoint>]
let main argv = 
    let input = "ugkiagan"
    let processedInputs =
        [0..127]
            |> List.map (fun x -> input + "-" + string x)
            |> List.map str2byte
            |> List.map (run2 l)
            |> List.map binaryString
    let part1res =
        processedInputs
            |> List.fold (fun a e -> a + e) ""
            |> Seq.countBy (fun x -> x = '1')
            |> Seq.filter fst
            |> Seq.map snd
            |> Seq.exactlyOne
    printfn "Part 1: %d" part1res
    assert(false)
    0 // return an integer exit code
