// https://adventofcode.com/2017/day/22

type Direction =
    | Up
    | Down
    | Left
    | Right

let rotateDir dir infected =
    match dir with
        | Up -> if infected then Right else Left
        | Down -> if infected then Left else Right
        | Left -> if infected then Up else Down
        | Right -> if infected then Down else Up

let moveDir (y, x) dir =
    match dir with
        | Up -> (y - 1, x)
        | Down -> (y + 1, x)
        | Left -> (y, x - 1)
        | Right -> (y, x + 1)

let rec moveCarrier (grid: bool[,]) dir (y, x) infectingBursts bursts =
    if bursts = 0 then
        infectingBursts
    else
        let isCurrentInfected = grid.[y,x]
        let newDir = rotateDir dir isCurrentInfected
        let newPos = moveDir (y, x) newDir
        grid.[y, x] <- isCurrentInfected = false
        let newIB = infectingBursts + if isCurrentInfected then 0 else 1
        moveCarrier grid newDir newPos newIB (bursts - 1)

[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllLines("input.txt")
    let grid = Array2D.create<bool> 1000 1000 false
    // manually blit the input array onto our "infinite" grid"
    let iy = Array.length input
    let ix = Array.head input |> String.length
    let starty = (Array2D.length1 grid / 2) - (iy / 2) // - 1?
    let startx = (Array2D.length2 grid / 2) - (ix / 2) // - 1?
    let endy = starty + ((iy / 2) * 2)
    let endx = startx + ((ix / 2) * 2) 
    let initPos = (Array2D.length1 grid / 2, Array2D.length2 grid / 2)
    for i = starty to endy do
        for j = startx to endx do
            grid.[i, j] <- input.[i - starty].[j - startx] = '#'
    let part1res = moveCarrier grid Up initPos 0 10000
    printfn "Part 1: %d" part1res
    0 // return an integer exit code
