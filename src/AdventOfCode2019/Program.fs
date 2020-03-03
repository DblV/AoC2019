module Main

[<EntryPoint>]
let main argv =
    match argv.[0] with
    | "1" -> printfn "%i" (DayOne.calculateTotalFuel (Lib.readLines argv.[1]))
    | _ -> printfn "Invalid option"
    0