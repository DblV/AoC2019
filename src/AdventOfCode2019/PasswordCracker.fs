module PasswordCracker

let rec hasAtLeastOneAdjacentPair lastChar (input:char list) =
    match input with
    | [] -> false
    | h::_ when h = lastChar -> true
    | h::t -> hasAtLeastOneAdjacentPair h t

let rec nextNotGreaterThanLast lastInt input =
    match input with
    | [] -> true
    | h::_ when h < lastInt -> false
    | h::t -> nextNotGreaterThanLast h t

let meetsCriteria (input:string) =
    input.Length = 6 
        && input |> Seq.toList |> hasAtLeastOneAdjacentPair ' '
        && input |> Seq.map (System.Char.GetNumericValue >> int) |> Seq.toList |> nextNotGreaterThanLast 0
     
let countValidPasswordsInRange min max =
    [min..max]
    |> List.map string
    |> List.filter meetsCriteria
    |> List.length
