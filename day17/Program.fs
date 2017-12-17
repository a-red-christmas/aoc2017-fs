// https://adventofcode.com/2017/day/17

let rec run (storm: ResizeArray<int>) pc stepBy steps stop =
    if steps = stop then
        storm
    else
        let newSteps = steps + 1
        let newPc = ((pc + stepBy) % storm.Count) + 1
        storm.Insert(newPc, newSteps)
        run storm newPc stepBy newSteps stop

[<EntryPoint>]
let main argv = 
    // HACK: convert this to an immutable array again
    let ra = new ResizeArray<int>()
    ra.Add(0)
    let part1storm = run ra 0 369 0 2018
    let part1res = Seq.item (Seq.findIndex (fun x -> x = 2017) ra + 1) ra
    assert(false)
    printfn "%A" argv
    0 // return an integer exit code
