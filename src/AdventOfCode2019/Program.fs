module Main

open FuelCalculator
open FuelManagementSystem
open Intcode

[<EntryPoint>]
let main argv =
    match argv.[0] with
    | "1.1" -> printfn "%i" (calculateFuelForAllModules (Lib.readLines argv.[1]) calculateFuelForMass)
    | "1.2" -> printfn "%i" (calculateFuelForAllModules (Lib.readLines argv.[1]) (calculateFuelForMassPlusFuel 0))
    | "2.1" -> printfn "%A" (runIntcodeComputerFromStringInput (Lib.readLines argv.[1] |> Seq.head))
    | "2.2" -> printfn "%A" (runSearchForOutputFromStringInput (Lib.readLines argv.[1] |> Seq.head) (argv.[2] |> int))
    | "3.1" -> printfn "%i" (calculateNearestIntersection (Lib.readLines argv.[1]))
    | _ -> printfn "Invalid option"
    0