// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

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

let rec coreLoop position severity (input: Map<int, FirewallLayer>) =
    if (Map.toList input |> List.map fst |> List.max) + 1 = position then
        severity
    else
        let newSeverity =
            if Map.containsKey position input then
                let layer = input.[position]
                if layer.ScannerPosition = 0 then
                    severity + (layer.Depth * position)
                else severity
            else severity
        let newPosition = position + 1
        let newInput = Map.map moveScanners input
        coreLoop newPosition newSeverity newInput

[<EntryPoint>]
let main argv =
    let lines = System.IO.File.ReadAllLines("input.txt")
    let input = lines |> Array.map parse |> Map.ofArray
    let part1res = coreLoop 0 0 input
    printfn "Part 1: %d" part1res
    0 // return an integer exit code
