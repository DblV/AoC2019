module PasswordCracker

let rec hasAtLeastOneAdjacentPair lastChar numberOfRepeats (input:char list) =
    match input with
    | [] -> numberOfRepeats = 2
    | h::t when h = lastChar -> hasAtLeastOneAdjacentPair h (numberOfRepeats+1) t
    | h::t -> 
        if numberOfRepeats = 2 then
            true
        else
            hasAtLeastOneAdjacentPair h 1 t

let rec nextNotGreaterThanLast lastInt input =
    match input with
    | [] -> true
    | h::_ when h < lastInt -> false
    | h::t -> nextNotGreaterThanLast h t

let meetsCriteria (input:string) =
    input.Length = 6 
        && input |> Seq.toList |> hasAtLeastOneAdjacentPair ' ' 1
        && input |> Seq.map (System.Char.GetNumericValue >> int) |> Seq.toList |> nextNotGreaterThanLast 0
     
let countValidPasswordsInRange min max =
    [min..max]
    |> List.map string
    |> List.filter meetsCriteria
    |> List.length
