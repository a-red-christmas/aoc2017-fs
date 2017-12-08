// https://adventofcode.com/2017/day/8

open System.Text.RegularExpressions

type ComparisonOperation =
    | Equal
    | NotEqual
    | Greater
    | GreaterOrEqual
    | Lesser
    | LesserOrEqual

type Opcode =
    {
        Register: string;
        Increment: int; // dec = -Operation
        ComparisonRegister: string;
        ComparisonOperation: ComparisonOperation;
        ComparisonNumber: int
    }
    override m.ToString() =
        Printf.sprintf "%s inc %d if %s %A %d"
            m.Register m.Increment m.ComparisonRegister
            m.ComparisonOperation m.ComparisonNumber

let regex = "([a-z]+) (inc|dec) (-?\d+) if ([a-z]+) (>=|>|<|<=|==|!=) (-?\d+)"

let parseOpcode x =
    let r = Regex.Match(x, regex)
    let name = r.Groups.[1].Value
    let incrementBy = int r.Groups.[3].Value
    let inc = if r.Groups.[2].Value = "dec" then -incrementBy else incrementBy
    let cmpReg = r.Groups.[4].Value
    let cmpOp = match r.Groups.[5].Value with
        | "==" -> Equal
        | "!=" -> NotEqual
        | ">"  -> Greater
        | ">=" -> GreaterOrEqual
        | "<"  -> Lesser
        | "<=" -> LesserOrEqual
    let cmpNum = int r.Groups.[6].Value
    {
        Register = name;
        Increment = inc;
        ComparisonRegister = cmpReg;
        ComparisonOperation = cmpOp;
        ComparisonNumber = cmpNum
    }

let regMapMax map =
    map |> Map.toList |> List.maxBy (fun (x, y) -> y) |> snd

let rec run (registers: Map<string, int>) opcodes pc peak =
    if pc = Array.length opcodes then
        (registers, peak)
    else
        let current = opcodes.[pc]
        let crn = current.ComparisonRegister
        let cr = registers.[crn]
        let cn = current.ComparisonNumber
        let i = current.Increment
        let r = current.Register
        let can = match current.ComparisonOperation with
            | Equal -> cr = cn
            | NotEqual -> cr <> cn
            | Greater -> cr > cn
            | GreaterOrEqual -> cr >= cn
            | Lesser -> cr < cn
            | LesserOrEqual -> cr <= cn
        let newReg = Map.map (fun k v ->
            if can && k = r then v + i else v) registers
        let newPeak = max (regMapMax newReg) peak
        run newReg opcodes (pc + 1) newPeak

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadAllLines("input.txt")
    let input = Array.map parseOpcode lines
    let ar =
        input 
            |> Array.collect (fun x -> [| x.Register; x.ComparisonRegister |])
            |> Array.distinct
    let arz = Array.zip ar (Array.init (Array.length ar) (fun x -> 0))
    let regMap = Map.ofArray arz
    let (regs, peak) = run regMap (Array.copy input) 0 0
    let part1res = regMapMax regs
    let part2res = peak
    printfn "Part 1: %d; Part 2: %d" part1res part1res
    0 // return an integer exit code
