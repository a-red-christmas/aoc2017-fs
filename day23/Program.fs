// https://adventofcode.com/2017/day/23

open System.Text.RegularExpressions

type Value =
    | Literal of int64
    | Register of char

type OpSet =
    { Register: char; Value: Value }

type OpSub =
    { Register: char; Value: Value }

type OpMul =
    { Register: char; Value: Value }

type OpJnz =
    { Comparison: Value; Offset: Value }

type Opcode =
    | OpSet of OpSet
    | OpSub of OpSub
    | OpMul of OpMul
    | OpJnz of OpJnz

let opcodeRegex =
    "(set|sub|mul|jnz) ([a-z]|[-\d]+)(?: ([a-z]|[-\d]+))?"

let regOrInt (r: string) =
    try
        Literal (int64 r)
    with
        | :? System.FormatException as e -> Register r.[0]

let parse input =
    let parseInternal opcode =
        let r = Regex.Match(opcode, opcodeRegex)
        match r.Groups.[1].Value with
            | "set" ->
                OpSet {
                    Register = r.Groups.[2].Value.[0]
                    Value = regOrInt r.Groups.[3].Value
                }
            | "sub" ->
                OpSub {
                    Register = r.Groups.[2].Value.[0]
                    Value = regOrInt r.Groups.[3].Value
                }
            | "mul" ->
                OpMul {
                    Register = r.Groups.[2].Value.[0]
                    Value = regOrInt r.Groups.[3].Value
                }
            | "jnz" ->
                OpJnz {
                    Comparison = regOrInt r.Groups.[2].Value
                    Offset = regOrInt r.Groups.[3].Value
                }
            | _ -> failwith "Invalid opcode"
    input |> Array.map parseInternal |> Array.toList

let regChange registers k v =
    Map.map (fun x y -> if x = k then v else y) registers

let getValue registers value =
    match value with
        | Register r -> Map.find r registers
        | Literal ov -> ov

let rec run tape registers pc mulExec =
    if pc >= List.length tape || pc < 0 then
        (mulExec, registers)
    else
        let toExec = tape.[pc]
        let (newPc, newMulExec, newReg) =
            match toExec with
                | OpSet { Register = r; Value = v } ->
                    let toSet = getValue registers v
                    let nr = regChange registers r toSet
                    (pc + 1, mulExec, nr)
                | OpSub { Register = r; Value = v } ->
                    let x = Map.find r registers
                    let y = getValue registers v
                    let nr = regChange registers r (x - y)
                    (pc + 1, mulExec, nr)
                | OpMul { Register = r; Value = v } ->
                    let x = Map.find r registers
                    let y = getValue registers v
                    let nr = regChange registers r (x * y)
                    (pc + 1, mulExec + 1, nr)
                | OpJnz { Comparison = c; Offset = o } ->
                    let ov = getValue registers o
                    let newPc =
                        if getValue registers c <> 0L then
                            pc + int ov
                        else
                            pc + 1
                    (newPc, mulExec, registers)
        run tape newReg newPc newMulExec

let isPrime x  =
    [2..x - 1]
        |> List.filter (fun y -> x % y = 0)
        |> List.length = 0

let rec optimizedPart2 start finish step primes =
    if start >= finish then
        primes
    else
        let newPrimes = primes + if isPrime start then 0 else 1
        optimizedPart2 (start + step) finish step newPrimes

[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllLines("input.txt")
    let tape = input |> parse
    let registers =
        ['a'..'h']
            |> List.map (fun x -> (x, 0L))
            |> Map.ofList
    let part1res = run tape registers 0 0 |> fst
    printfn "Part 1: %d" part1res
    // change these to match your first line b register
    let part2res = optimizedPart2 109900 126901 17 0
    printfn "Part 2: %d" part2res
    0 // return an integer exit code
