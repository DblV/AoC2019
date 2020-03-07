module Intcode

open FSharp.Data

let execOperation operation (memory:int list) parameters =
    let noun, verb, replaceValueAt = parameters 
    let replaceValueWith = operation noun verb

    memory
    |> List.mapi (fun i x ->
        if i = replaceValueAt then
            replaceValueWith
        else
            x)

let rec runIntcodeComputer address (memory:int list) =
    match memory.[address] with
    | 1 -> 
        runIntcodeComputer (address+4) (execOperation (+) memory (memory.[memory.[address+1]], memory.[memory.[address+2]], memory.[address+3]))
    | 2 -> 
        runIntcodeComputer (address+4) (execOperation (*) memory (memory.[memory.[address+1]], memory.[memory.[address+2]], memory.[address+3]))
    | 99 -> 
        memory
    | _ -> 
        memory

let rec searchForOutput noun verb target memory =
    // Exec program with new settings for noun and verb
    let output = 
        runIntcodeComputer 0 
            (execOperation (+) 
                (execOperation (+) memory (0,verb,2))
                    (noun,0,1))

    // Examine position 0 - if it matches our target, return the noun and verb used to produce this
    match output.[0] with
    | x when x = target -> 100 * noun + verb
    | _ -> 
        match (noun,verb) with
        | (x,y) when x >= memory.Length-1 && y >= memory.Length-1 -> 0
        | (x,y) when x >= memory.Length-1 && y <= memory.Length-1 -> 
            searchForOutput 0 (verb+1) target memory
        | (x,y) when x <= memory.Length-1 && y <= memory.Length-1 -> 
            searchForOutput (noun+1) verb target memory
        | _ -> 0

let initialiseMemoryFromString inputString =
    let csvInput = CsvFile.Parse(inputString)
    match csvInput.Headers with
    | Some x -> x |> Array.toList |> List.map int
    | None -> List<int>.Empty

let runIntcodeComputerFromStringInput inputString =
    runIntcodeComputer 0 (initialiseMemoryFromString inputString)

let runSearchForOutputFromStringInput inputString target =
    // Loop through inputs for positions 1 and 2 until the output of the program at position 0 is 
    searchForOutput 0 0 target (initialiseMemoryFromString inputString)
    