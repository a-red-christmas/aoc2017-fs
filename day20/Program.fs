// https://adventofcode.com/2017/day/20

open System.Text.RegularExpressions

type Particle =
    {
        Position: int * int * int
        Velocity: int * int * int
        Acceleration: int * int * int
    }

let regex =
    "p=<([-\d]+),([-\d]+),([-\d]+)>, " +
    "v=<([-\d]+),([-\d]+),([-\d]+)>, " +
    "a=<([-\d]+),([-\d]+),([-\d]+)>"

let parse lines =
    let gv (r: Match) (i: int) = int r.Groups.[i].Value
    let parseInternal line =
        let r = Regex.Match (line, regex)
        {
            Position = (gv r 1, gv r 2, gv r 3)
            Velocity = (gv r 4, gv r 5, gv r 6)
            Acceleration = (gv r 7, gv r 8, gv r 9)
        }
    Array.map parseInternal lines |> Array.toList

let addTuple (a, b, c) (x, y, z) =
    (a + x, b + y, c + z)

let accelerate particle =
    let newVelocity = addTuple particle.Velocity particle.Acceleration
    {
        particle with
            Velocity = newVelocity
            Position = addTuple particle.Position newVelocity
    }

let manhattan (x, y, z) =
    abs(x) + abs(y) + abs(z)

let rec excite particles iterations =
    if iterations = 0 then
        particles
    else
        let excited = List.map accelerate particles
        excite excited (iterations - 1)

[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllLines("input.txt")
    let particles = parse input
    let excited = excite particles 1000
    let closest = List.minBy (fun x -> manhattan x.Position) excited
    let part1res = List.findIndex (fun x -> x = closest) excited
    printfn "Part 1: %d" part1res
    0 // return an integer exit code
