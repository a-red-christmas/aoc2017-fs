// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.Text.RegularExpressions

type Spin =
    { HowMany: int }

type Exchange = 
    { ExchangeA: int; ExchangeB: int }

type Partner = 
    { PartnerA: char; PartnerB: char }

type Opcode =
    | Spin of Spin
    | Exchange of Exchange
    | Partner of Partner

let parseTape (tape: string) =
    let parseInternal opcode =
        let r = Regex.Match(opcode, "(s|x|p)([a-z]|\d+)(?:/([a-z]|\d+))?")
        match r.Groups.[1].Value with
            | "s" ->
                Spin {
                    HowMany = int r.Groups.[2].Value
                }
            | "x" ->
                Exchange {
                    ExchangeA = int r.Groups.[2].Value
                    ExchangeB = int r.Groups.[3].Value
                }
            | "p" ->
                Partner {
                    PartnerA = r.Groups.[2].Value.[0]
                    PartnerB = r.Groups.[3].Value.[0]
                }
            | _ -> failwith "Invalid opcode"
    let split = tape.Split(',')
    split
        |> Array.map parseInternal
        |> Array.toList

let strSwapCharByIndex (str: string) x y =
    let a = str.ToCharArray()
    a.[y] <- str.[x]
    a.[x] <- str.[y]
    System.String.Concat(a)

let rec runThrough programs tape =
    match tape with
        | x :: xs ->
            runThrough
                (match List.head tape with
                    | Spin { HowMany = m } ->
                        let l = String.length programs - 1
                        let tail = programs.[l - m + 1..l]
                        let head = programs.[0..l - m]
                        tail + head
                    | Exchange { ExchangeA = a; ExchangeB = b } ->
                        strSwapCharByIndex programs a b
                    | Partner { PartnerA = a; PartnerB = b } ->
                        let ia = programs.IndexOf(a)
                        let ib = programs.IndexOf(b)
                        strSwapCharByIndex programs ia ib)
                (List.tail tape)
        | [] -> programs

let rec runThroughSeveral programs tape seenBefore =
    let np = runThrough programs tape
    if List.contains np seenBefore then
        let r = List.rev seenBefore
        let l = List.length seenBefore
        let m = 1000000000 % l
        let p = List.item m r
        failwith "memoization"
    else
        runThroughSeveral np tape (np ::seenBefore)

[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllText("input.txt") //"s1,x3/4,pe/b"
    let tape = parseTape input
    let programs = System.String.Concat([|'a'..'p'|])
    let part1 = runThrough programs tape
    let part2 = runThroughSeveral programs tape [programs]
    printfn "Part 1: %s; Part 2: %s" part1 part2
    assert(false)
    0 // return an integer exit code
