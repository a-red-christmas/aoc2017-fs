// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.Text.RegularExpressions

type Value =
    | OpValue of int64
    | Register of char

type OpSnd =
    { Frequency: char }

type OpSet =
    { Register: char; Value: Value }

type OpAdd =
    { Register: char; Value: Value }

type OpMul =
    { Register: char; Value: Value }

type OpMod =
    { Register: char; Value: Value }

type OpRcv =
    { Frequency: char }

type OpJgz =
    { Comparison: Value; Offset: Value }

type Opcode =
    | OpSnd of OpSnd
    | OpSet of OpSet
    | OpAdd of OpAdd
    | OpMul of OpMul
    | OpMod of OpMod
    | OpRcv of OpRcv
    | OpJgz of OpJgz

let opcodeRegex =
    "(snd|set|add|mul|mod|rcv|jgz) ([a-z]|[-\d]+)(?: ([a-z]|[-\d]+))?"

let regOrInt (r: string) =
    try
        OpValue (int64 r)
    with
        | :? System.FormatException as e -> Register r.[0]

let parse input =
    let parseInternal opcode =
        let r = Regex.Match(opcode, opcodeRegex)
        match r.Groups.[1].Value with
            | "snd" ->
                OpSnd { Frequency = r.Groups.[2].Value.[0] }
            | "set" ->
                OpSet {
                    Register = r.Groups.[2].Value.[0]
                    Value = regOrInt r.Groups.[3].Value
                }
            | "add" ->
                OpAdd {
                    Register = r.Groups.[2].Value.[0]
                    Value = regOrInt r.Groups.[3].Value
                }
            | "mul" ->
                OpMul {
                    Register = r.Groups.[2].Value.[0]
                    Value = regOrInt r.Groups.[3].Value
                }
            | "mod" ->
                OpMod {
                    Register = r.Groups.[2].Value.[0]
                    Value = regOrInt r.Groups.[3].Value
                }
            | "rcv" ->
                OpRcv { Frequency = r.Groups.[2].Value.[0] }
            | "jgz" ->
                OpJgz {
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
        | OpValue ov -> ov

let rec run tape registers pc lastSnd firstRcv =
    if firstRcv <> 0L || ( pc >= List.length tape || pc < 0) then
        firstRcv
    else
        let toExec = tape.[pc]
        let (newPc, newLastSnd, newFirstRcv, newReg) =
            match toExec with
                | OpSnd { Frequency = f } ->
                    (pc + 1, Map.find f registers, firstRcv, registers)
                | OpSet { Register = r; Value = v } ->
                    let toSet = getValue registers v
                    let nr = regChange registers r toSet
                    (pc + 1, lastSnd, firstRcv, nr)
                | OpAdd { Register = r; Value = v } ->
                    let x = Map.find r registers
                    let y = getValue registers v
                    let nr = regChange registers r (x + y)
                    (pc + 1, lastSnd, firstRcv, nr)
                | OpMul { Register = r; Value = v } ->
                    let x = Map.find r registers
                    let y = getValue registers v
                    let nr = regChange registers r (x * y)
                    (pc + 1, lastSnd, firstRcv, nr)
                | OpMod { Register = r; Value = v } ->
                    let x = Map.find r registers
                    let y = getValue registers v
                    let nr = regChange registers r (x % y)
                    (pc + 1, lastSnd, firstRcv, nr)
                | OpRcv { Frequency = f } ->
                    let nr = regChange registers f lastSnd
                    let nfr =
                        if firstRcv = 0L then lastSnd else firstRcv
                    (pc + 1, lastSnd, nfr, nr)
                | OpJgz { Comparison = c; Offset = o } ->
                    let ov = getValue registers o
                    let newPc =
                        if getValue registers c > 0L then
                            pc + int ov
                        else
                            pc + 1
                    (newPc, lastSnd, firstRcv, registers)
        run tape newReg newPc newLastSnd newFirstRcv

[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllLines("input.txt")
    let tape = input |> parse
    let registers =
        tape
            |> List.collect (fun x ->
                match x with
                    | OpSnd { Frequency = f }
                    | OpRcv { Frequency = f }  ->
                        [f]
                    | OpSet { Register = r; Value = v }
                    | OpAdd { Register = r; Value = v }
                    | OpMul { Register = r; Value = v }
                    | OpMod { Register = r; Value = v } ->
                        r :: match v with | Register rv -> [rv] | _ -> []
                    | OpJgz { Comparison = c; Offset = o } ->
                        let o1 = match c with | Register rc -> [rc] | _ -> []
                        let o2 = match o with | Register ro -> [ro] | _ -> []
                        List.append o1 o2
                    | _ -> [])
            |> List.distinct
            |> List.map (fun x -> (x, 0L))
            |> Map.ofList
    let part1res = run tape registers 0 0L 0L
    printfn "Part 1: %d" part1res
    0 // return an integer exit code
