module Main

open FuelCalculator

[<EntryPoint>]
let main argv =
    match argv.[0] with
    | "1.1" -> printfn "%i" (calculateFuelForAllModules (Lib.readLines argv.[1]) calculateFuelForMass)
    | "1.2" -> printfn "%i" (calculateFuelForAllModules (Lib.readLines argv.[1]) (calculateFuelForMassPlusFuel 0))
    | _ -> printfn "Invalid option"
    0