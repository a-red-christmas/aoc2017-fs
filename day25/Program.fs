// https://adventofcode.com/2017/day/25

type State = | StateA | StateB | StateC | StateD | StateE | StateF

// HACK: we can go over/under, so simulate negative with a positive offset
let tapeSize = 1048576
let tapeOffset = (tapeSize / 2) - 1
let get tape cursor = Array.item (cursor + tapeOffset) tape
let set tape cursor item = Array.set tape (cursor + tapeOffset) item

let rec run state (tape: bool[]) cursor steps =
    if steps = 0 then
        let counts = Array.countBy (fun x -> x) tape
        Array.filter fst counts |> Array.exactlyOne |> snd
    else
        let newSteps = steps - 1
        match state with
            | StateA ->
                let current = get tape cursor
                set tape cursor (current = false)
                let newCur = cursor + if current then -1 else 1
                let newState = StateB
                run newState tape newCur newSteps
            | StateB ->
                let current = get tape cursor
                set tape cursor current
                let newCur = cursor + if current then -1 else 1
                let newState = if current then StateB else StateC
                run newState tape newCur newSteps
            | StateC ->
                let current = get tape cursor
                set tape cursor (current = false)
                let newCur = cursor + if current then -1 else 1
                let newState = if current then StateA else StateD
                run newState tape newCur newSteps
            | StateD ->
                let current = get tape cursor
                set tape cursor true
                let newCur = cursor - 1
                let newState = if current then StateF else StateE
                run newState tape newCur newSteps
            | StateE ->
                let current = get tape cursor
                set tape cursor (current = false)
                let newCur = cursor - 1
                let newState = if current then StateD else StateA
                run newState tape newCur newSteps
            | StateF ->
                let current = get tape cursor
                set tape cursor true
                let newCur = cursor + if current then -1 else 1
                let newState = if current then StateE else StateA
                run newState tape newCur newSteps

[<EntryPoint>]
let main argv = 
    let tape = Array.create tapeSize false
    let results = run StateA tape 0 12586542
    printfn "Final Answer: %d" results
    0 // return an integer exit code
