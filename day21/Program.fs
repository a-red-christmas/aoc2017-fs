// https://adventofcode.com/2017/day/21

let strRev s = Seq.rev s |> System.String.Concat

let symmetric l = List.map strRev l |> List.rev

let flip = List.rev

[<EntryPoint>]
let main argv = 
    let template = [".#."; "..#"; "###"]
    // no answer for today i dont like this one
    printfn "%A" argv
    0 // return an integer exit code
