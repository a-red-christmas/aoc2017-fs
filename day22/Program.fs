// https://adventofcode.com/2017/day/22

type Direction =
    | Up
    | Down
    | Left
    | Right

type State =
    | Clean
    | Weakened
    | Infected
    | Flagged

let infect state part2 =
    match state with
        | Clean when part2 -> Weakened
        | Clean -> Infected
        | Weakened -> Infected
        | Infected when part2 -> Flagged
        | Infected -> Clean
        | Flagged -> Clean

let rotateDir dir infected =
    match dir with
        | Up ->
            match infected with
                | Clean -> Left
                | Weakened -> Up
                | Infected -> Right
                | Flagged -> Down
        | Down ->
            match infected with
                | Clean -> Right
                | Weakened -> Down
                | Infected -> Left
                | Flagged -> Up
        | Left ->
            match infected with
                | Clean -> Down
                | Weakened -> Left
                | Infected -> Up
                | Flagged -> Right
        | Right ->
            match infected with
                | Clean -> Up
                | Weakened -> Right
                | Infected -> Down
                | Flagged -> Left

let moveDir (y, x) dir =
    match dir with
        | Up -> (y - 1, x)
        | Down -> (y + 1, x)
        | Left -> (y, x - 1)
        | Right -> (y, x + 1)

let rec moveCarrier (grid: State[,]) dir (y, x) part2 infectingBursts bursts =
    if bursts = 0 then
        infectingBursts
    else
        let currentState = grid.[y,x]
        let newDir = rotateDir dir currentState
        let newPos = moveDir (y, x) newDir
        let newState = infect currentState part2
        grid.[y, x] <- newState
        let newIB = infectingBursts + if newState <> Infected then 0 else 1
        moveCarrier grid newDir newPos part2 newIB (bursts - 1)

[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllLines("input.txt")
    let grid = Array2D.create<State> 1000 1000 Clean
    // manually blit the input array onto our "infinite" grid"
    let iy = Array.length input
    let ix = Array.head input |> String.length
    let starty = (Array2D.length1 grid / 2) - (iy / 2)
    let startx = (Array2D.length2 grid / 2) - (ix / 2)
    let endy = starty + ((iy / 2) * 2)
    let endx = startx + ((ix / 2) * 2) 
    let initPos = (Array2D.length1 grid / 2, Array2D.length2 grid / 2)
    for i = starty to endy do
        for j = startx to endx do
            grid.[i, j] <-
                if input.[i - starty].[j - startx] = '#' then
                    Infected
                else
                    Clean
    // make a copy for part 2, then run part 1
    let grid2 = Array2D.copy grid
    let part1res = moveCarrier grid Up initPos false 0 10000
    printfn "Part 1: %d" part1res
    let part2res = moveCarrier grid2 Up initPos true 0 10000000
    printfn "Part 2: %d" part2res
    0 // return an integer exit code
