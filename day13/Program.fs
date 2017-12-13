// https://adventofcode.com/2017/day/13

open System.Text.RegularExpressions

type ScannerDirection =
    | ScanUp
    | ScanDown

type FirewallLayer =
    {
        Depth: int
        ScannerPosition: int
        ScannerDirection: ScannerDirection
    }

let parse str =
    let r = Regex.Match(str, "(\d+): (\d+)")
    (
        int r.Groups.[1].Value,
        {
            Depth = int r.Groups.[2].Value
            ScannerPosition = 0
            ScannerDirection = ScanDown
        }
    )

let moveScanners k v =
    match v.ScannerDirection with
        | ScanDown ->
            {
                v with
                    ScannerPosition = v.ScannerPosition + 1
                    ScannerDirection =
                        if v.ScannerPosition + 1 = v.Depth - 1 then
                            ScanUp
                        else
                            ScanDown
            }
        | ScanUp ->
            {
                v with
                    ScannerPosition = v.ScannerPosition - 1
                    ScannerDirection =
                        if v.ScannerPosition - 1 = 0 then
                            ScanDown
                        else
                            ScanUp
            }

let rec coreLoop position severity catches maxCatch input =
    let pastLimits =
        (Map.toList input |> List.map fst |> List.max) + 1 = position
    let hitMaxCatches =
        match maxCatch with
            | Some(x) -> catches > x
            | None -> false
    if hitMaxCatches || pastLimits then
        (severity, catches)
    else
        let (newSeverity, newCatches) =
            match input.TryFind(position) with
                | Some(layer) when layer.ScannerPosition = 0 ->
                    (severity + (layer.Depth * position), catches + 1)
                | _ -> (severity, catches)
        let newPosition = position + 1
        let newInput = Map.map moveScanners input
        coreLoop newPosition newSeverity newCatches maxCatch newInput

let rec findBestDelay input delay =
    let delayedInput = Map.map moveScanners input
    let (s, c) = coreLoop 0 0 0 (Some 1) delayedInput
    if c > 0 then
        findBestDelay delayedInput (delay + 1)
    else
        delay + 1

[<EntryPoint>]
let main argv =
    let lines = System.IO.File.ReadAllLines("input.txt")
    let input = lines |> Array.map parse |> Map.ofArray
    let (part1sev, part1catch) = coreLoop 0 0 0 None input
    let part2res = findBestDelay input 0
    printfn "Part 1: %d; Part 2: %d" part1sev part2res
    0 // return an integer exit code
