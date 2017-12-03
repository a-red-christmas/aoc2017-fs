// https://adventofcode.com/2017/day/3
// HACK HACK HACK

type Direction = 
    | Left
    | Right
    | Up
    | Down

let md2jagged x =
    let xx = Array2D.length1 x
    let xy = Array2D.length2 x
    let newArray = Array.create xx [||]
    for i in 0..(xx - 1) do
        // init arrays here... doing so above will copy the ref
        newArray.[i] <- Array.create xy 0
        for j in 0..(xy - 1) do
            newArray.[i].[j] <- x.[i, j]
    newArray

// HACK: very imperative
let makeUlam (n: int) (i: int) =
    let spiral = Array2D.create n n 0
    let mutable dir = Right
    let mutable j = i
    let mutable y = n / 2
    let mutable x = if (n % 2 = 0) then y - 1 else y
    while (j <= ((n * n) - 1 + i)) do
        spiral.[y, x] <- j
        dir <- match dir with
        | Right -> if (x <= n - 1 && spiral.[y - 1, x] = 0 && j > i) then Up else Right
        | Up -> if (spiral.[y, x - 1] = 0) then Left else Up
        | Left -> if (x = 0 || spiral.[y + 1, x] = 0) then Down else Left
        | Down -> if (spiral.[y, x + 1] = 0) then Right else Down
        x <- match dir with
        | Right -> x + 1
        | Left -> x - 1
        | _ -> x
        y <- match dir with
        | Down -> y + 1
        | Up -> y - 1
        | _ -> y
        j <- j + 1
        //printfn "j %d x %d y %d d %A" j x y dir
    spiral
let makeUlamPart2 (n: int) (i: int) =
    let spiral = Array2D.create n n 0
    let neighboursSum (a, b) =
        spiral.[max 0 (a - 1)..min (a + 1) (n - 1), max 0 (b - 1)..min (b + 1) (n - 1)]
        |> Seq.cast<int>
        |> Seq.sum
    let mutable dir = Right
    let mutable j = i
    let mutable k = j
    let mutable y = n / 2
    let mutable x = if (n % 2 = 0) then y - 1 else y
    while (j <= ((n * n) - 1 + i)) do
        spiral.[y, x] <- k
        dir <- match dir with
        | Right -> if (x <= n - 1 && spiral.[y - 1, x] = 0 && j > i) then Up else Right
        | Up -> if (spiral.[y, x - 1] = 0) then Left else Up
        | Left -> if (x = 0 || spiral.[y + 1, x] = 0) then Down else Left
        | Down -> if (spiral.[y, x + 1] = 0) then Right else Down
        x <- match dir with
        | Right -> x + 1
        | Left -> x - 1
        | _ -> x
        y <- match dir with
        | Down -> y + 1
        | Up -> y - 1
        | _ -> y
        j <- j + 1
        k <- if j = 1 then j else neighboursSum (y, x)
        //printfn "j %d x %d y %d d %A" j x y dir
    spiral

let printSpiral x =
    for i in x do
        let j = Array.map int i
        let joined = System.String.Join("\t", j)
        printfn "%s" joined

let rec bestSquare target i =
    if (i * i >= target) then i else bestSquare target (i + 1)

let findPosition (target: int) (arr: int array array) =
    let iArr = Array.find (fun x -> Array.contains target x) arr
    let x = Array.findIndex (fun a -> target = a) iArr
    let y = Array.findIndex (fun a -> iArr = a) arr
    assert (arr.[y].[x] = target)
    (y, x)
   
let findDistance (x1, y1) (x2, y2) = 
    (max x1 x2 - min x1 x2) + (max y1 y2 - min y1 y2)

[<EntryPoint>]
let main argv =
    let target = 277678
    let square = bestSquare target 1
    let spiral = makeUlam square 1
    let jagged = spiral |> md2jagged
    let positionOfTarget = findPosition target jagged
    let positionOfBeginning = findPosition 1 jagged

    let part1Res = findDistance positionOfBeginning positionOfTarget

    let part2Spiral = makeUlamPart2 9 1
    printSpiral (part2Spiral |> md2jagged)
    let part2Res = part2Spiral |> Seq.cast<int> |> Seq.find (fun x -> x > target)

    printfn "Part 1: %d; Part 2: %d" part1Res part2Res

    0 // return an integer exit code
