// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.Text.RegularExpressions

[<StructuredFormatDisplay("{Name} ({Weight}) -> {Children}")>]
type InputTreeNode =
    { Name: string; Weight: int; Children: string array }

let strIntoTree x =
    let r = Regex.Match(x, "([a-z]+) \((\d+)\)(?: -> ([a-z, ]*))?")
    let name = r.Groups.[1].Value
    let weight = int r.Groups.[2].Value
    let children =
        if r.Groups.[3].Success then
            r.Groups.[3].Value.Split([|", "|],
                System.StringSplitOptions.None)
        else
            [||]
    { Name = name; Weight = weight; Children = children}

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadAllLines("input.txt")
    let input = Array.map strIntoTree lines
    let children = Array.collect (fun x -> x.Children) input
    let part1res = (Array.find (fun x ->
        Array.contains x.Name children = false) input).Name
    let part2res = -1
    printfn "Part 1: %s; Part 2: %d" part1res part2res
    0 // return an integer exit code
