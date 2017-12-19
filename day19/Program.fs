// https://adventofcode.com/2017/day/19

type Tile =
    | Empty
    | HPipe
    | VPipe
    | Corner
    | Letter of char

type Direction =
    | Up
    | Down
    | Left
    | Right

let findStart maze =
    seq {
        for i = 0 to Array2D.length2 maze - 1 do
            if maze.[0, i] = VPipe then yield (0, i) else ignore 0
    }
        |> Seq.exactlyOne

let inBounds maze y x =
    y > -1 &&
    y < Array2D.length1 maze && 
    x > -1 &&
    x < Array2D.length2 maze

let isValid maze y x expected =
    if inBounds maze y x then
        match maze.[y, x] with
            | Letter c -> true
            // HACK: do NOT do '| expected'; it resolves to Empty
            | HPipe when expected = HPipe -> true
            | VPipe when expected = VPipe -> true
            | _ -> false
    else false

let rec run (maze: Tile[,]) (y, x) dir seen steps =
    if inBounds maze y x && maze.[y, x] <> Empty then
        let currentTile = maze.[y, x]
        let newSeen =
            match currentTile with
                | Letter c -> seen + c.ToString()
                | _ -> seen
        let (newPosition, newDirection) = 
            if currentTile = Corner then
                let toUp = isValid maze (y - 1) x VPipe
                let toLeft = isValid maze y (x - 1) HPipe
                match dir with
                    | Up
                    | Down ->
                        if toLeft then
                            ((y, x - 1), Left)
                        else
                            ((y, x + 1), Right)
                    | Left
                    | Right ->
                        if toUp then
                            ((y - 1, x), Up)
                        else
                            ((y + 1, x), Down)
            else
                match dir with
                    | Up -> ((y - 1, x), Up)
                    | Down -> ((y + 1, x), Down)
                    | Left -> ((y, x - 1), Left)
                    | Right -> ((y, x + 1), Right)
        run maze newPosition newDirection newSeen (steps + 1)
    else
        (seen, steps)

[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllLines("input.txt")
    let sy = Array.length input
    let sx = Array.head input |> String.length
    let maze = Array2D.create sy sx Empty
    for i = 0 to sy - 1 do
        for j = 0 to sx - 1 do
            let s = Array.item i input
            let c = s.[j]
            maze.[i, j] <- match c with
                | ' ' -> Empty
                | '-' -> HPipe
                | '|' -> VPipe
                | '+' -> Corner
                | _ -> Letter c
    let startPos = findStart maze
    let (part1res, part2res) = run maze startPos Down "" 0
    printfn "Part 1: %s; Part 2: %d" part1res part2res
    0 // return an integer exit code
