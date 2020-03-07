module Intcode

open FSharp.Data

let execOperation (program:int list) offset operation =
    let operand1 = program.[program.[offset+1]]
    let operand2 = program.[program.[offset+2]]
    let replaceValueWith = operation operand1 operand2
    let replaceValueAt = program.[offset+3]

    program
    |> List.mapi (fun i x ->
        if i = replaceValueAt then
            replaceValueWith
        else
            x)

let rec runIntcode index (program:int list) =
    match program.[index] with
    | 1 -> 
        runIntcode (index+4) (execOperation program index (+))
    | 2 -> 
        runIntcode (index+4) (execOperation program index (*))
    | 99 -> 
        program
    | _ -> 
        program

let runIntcodeFromString programString =
    let csvInput = CsvFile.Parse(programString)
    match csvInput.Headers with
    | Some x -> x |> Array.toList |> List.map int |> runIntcode 0
    | None -> List<int>.Empty
