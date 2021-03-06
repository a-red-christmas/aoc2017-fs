﻿// https://adventofcode.com/2017/day/11

let goDirectionHex (y, x) desiredDirection =
    match desiredDirection with
        // thanks to leah2 for a less fucky grid idea
        | "n" -> (y - 1, x)
        | "s" -> (y + 1, x)
        | "nw" -> (y, x - 1)
        | "ne" -> (y - 1, x + 1)
        | "sw" -> (y + 1, x - 1)
        | "se" -> (y, x + 1)
        | _ -> failwith "Invalid direction"

let rec followGrid (y, x, m) directions =
    if Array.length directions = 0 then
        (y, x, m)
    else
        let dir = Array.head directions
        let (ny, nx) = goDirectionHex (y, x) dir
        let nm = max m (max (abs ny) (abs nx))
        followGrid (ny, nx, nm) (Array.tail directions)

[<EntryPoint>]
let main argv =
    let inputRaw = System.IO.File.ReadAllText("input.txt")
    let input = inputRaw.Split(',')
    let (y, x, m) = followGrid (0, 0, 0) input
    // 08:58 < leah2> you need max(abs(a),abs(b),abs(a)+abs(b))
    let distance = max (abs y) (abs x)
    printfn "Part 1: %d; Part 2: %d" distance m
    0 // return an integer exit code
