module Intcode

open FSharp.Data

type ParameterMode = Position | Immediate
type OpCode = Add | Multiply | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals | Halt

type Instruction = {
    opCode: OpCode;
    param1Mode: ParameterMode;
    param2Mode: ParameterMode;
}

let parseOpCode opCode =
    match opCode with
    | "01" -> Add
    | "02" -> Multiply
    | "03" -> Input
    | "04" -> Output
    | "05" -> JumpIfTrue
    | "06" -> JumpIfFalse
    | "07" -> LessThan
    | "08" -> Equals
    | _ -> Halt

let parseMode mode = 
    match mode with
    | '0' -> Position
    | _ -> Immediate

let parseInstruction instruction =
    instruction
    |> string
    |> fun x -> x.PadLeft(5,'0')
    |> fun x -> 
        { 
            opCode = parseOpCode x.[3..4];
            param1Mode = parseMode x.[2];
            param2Mode = parseMode x.[1]; 
        }

let memoryLookup (memory:int list) mode address =
    match mode with
    | Position -> memory.[memory.[address]]
    | Immediate -> memory.[address]

let replaceValue replaceValueAt replaceValueWith memory =
    memory
    |> List.mapi (fun i x -> if i = replaceValueAt then replaceValueWith else x)

let rec runIntcodeComputer address (memory:int list) (inputValue:int) (outputValues:int list) =
    let instruction = parseInstruction memory.[address]
    match instruction.opCode with
    | Add ->
        let replaceValueWith = (memoryLookup memory instruction.param1Mode (address + 1)) + (memoryLookup memory instruction.param2Mode (address + 2))
        runIntcodeComputer 
            (address + 4) 
            (replaceValue (memoryLookup memory Immediate (address + 3)) replaceValueWith memory)
            inputValue
            outputValues
    | Multiply -> 
        let replaceValueWith = (memoryLookup memory instruction.param1Mode (address + 1)) * (memoryLookup memory instruction.param2Mode (address + 2))
        runIntcodeComputer 
            (address + 4) 
            (replaceValue (memoryLookup memory Immediate (address + 3)) replaceValueWith memory)
            inputValue
            outputValues
    | Input -> 
        runIntcodeComputer 
            (address + 2) 
            (replaceValue (memoryLookup memory Immediate (address + 1)) inputValue memory)
            inputValue
            outputValues
    | Output -> 
        let outputValue = (memoryLookup memory instruction.param1Mode (address + 1))
        printfn "OUTPUT: %i" outputValue
        runIntcodeComputer 
            (address + 2)
            memory
            inputValue
            (outputValues@[outputValue])
    | JumpIfTrue ->
        let addressTest = (memoryLookup memory instruction.param1Mode (address + 1))
        runIntcodeComputer
            (if addressTest > 0 then (memoryLookup memory instruction.param2Mode (address + 2)) else (address + 3))
            memory
            inputValue
            outputValues
    | JumpIfFalse ->
        let addressTest = (memoryLookup memory instruction.param1Mode (address + 1))
        runIntcodeComputer
            (if addressTest = 0 then (memoryLookup memory instruction.param2Mode (address + 2)) else (address + 3))
            memory
            inputValue
            outputValues
    | LessThan ->
        let firstParam = (memoryLookup memory instruction.param1Mode (address + 1))
        let secondParam = (memoryLookup memory instruction.param2Mode (address + 2))
        runIntcodeComputer
            (address + 4)
            (if firstParam < secondParam then
                (replaceValue (memoryLookup memory Immediate (address + 3)) 1 memory)
            else
                (replaceValue (memoryLookup memory Immediate (address + 3)) 0 memory))
            inputValue
            outputValues
    | Equals ->
        let firstParam = (memoryLookup memory instruction.param1Mode (address + 1))
        let secondParam = (memoryLookup memory instruction.param2Mode (address + 2))
        runIntcodeComputer
            (address + 4)
            (if firstParam = secondParam then
                (replaceValue (memoryLookup memory Immediate (address + 3)) 1 memory)
            else
                (replaceValue (memoryLookup memory Immediate (address + 3)) 0 memory))
            inputValue
            outputValues
    | Halt -> 
        (memory,inputValue,outputValues)

let rec searchForOutput noun verb target memory =
    // Exec program with new settings for noun and verb
    let setMemory = (replaceValue 1 noun) >> (replaceValue 2 verb)
    let (outputMemory, _, _) = runIntcodeComputer 0 (setMemory memory) 0 List<int>.Empty

    // Examine position 0 - if it matches our target, return the noun and verb used to produce this
    match outputMemory.[0] with
    | x when x = target -> 100 * noun + verb
    | _ -> 
        match (noun,verb) with
        | (x,y) when x >= outputMemory.Length-1 && y >= outputMemory.Length-1 -> 0
        | (x,y) when x >= outputMemory.Length-1 && y <= outputMemory.Length-1 -> 
            searchForOutput 0 (verb+1) target memory
        | (x,y) when x <= outputMemory.Length-1 && y <= outputMemory.Length-1 -> 
            searchForOutput (noun+1) verb target memory
        | _ -> 0

let initialiseMemoryFromString inputString =
    let csvInput = CsvFile.Parse(inputString)
    match csvInput.Headers with
    | Some x -> x |> Array.toList |> List.map int
    | None -> List<int>.Empty

let runIntcodeComputerFromStringInputWithInitialInput inputString initialInput =
    runIntcodeComputer 0 (initialiseMemoryFromString inputString) initialInput List<int>.Empty

let runIntcodeComputerFromStringInput inputString =
    runIntcodeComputerFromStringInputWithInitialInput inputString 0

let runSearchForOutputFromStringInput inputString target =
    // Loop through inputs for positions 1 and 2 until the output of the program at position 0 is 
    searchForOutput 0 0 target (initialiseMemoryFromString inputString)
    