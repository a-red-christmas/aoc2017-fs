// https://adventofcode.com/2017/day/25

type State = | StateA | StateB | StateC | StateD | StateE | StateF

// HACK: we can go over/under
let get tape cursor = Array.item (cursor + 511) tape
let set tape cursor item = Array.set tape (cursor + 511) item

let rec run state (tape: bool[]) cursor steps =
    if steps = 0 then
        let counts = Array.countBy (fun x -> x) tape
        Array.filter fst counts |> Array.exactlyOne |> snd
    else
        let newSteps = steps - 1
        // change to match inputs
        match state with
            | StateA ->
                let current = get tape cursor
                set tape cursor (current = false)
                let newCur = cursor + if current then -1 else 1
                let newState = StateB
                run newState tape newCur newSteps
            | StateB ->
                let current = get tape cursor
                set tape cursor true
                let newCur = cursor + if current then 1 else -1
                let newState = StateA
                run newState tape newCur newSteps

[<EntryPoint>]
let main argv = 
    let tape = Array.create 1024 false
    let initialState = StateA
    let results = run initialState tape 0 6
    printfn "%A" argv
    0 // return an integer exit code
