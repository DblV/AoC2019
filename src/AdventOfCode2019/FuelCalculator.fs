module FuelCalculator

let calculateFuelForMass mass =
    mass / 3
    |> fun x -> (x - 2)
    |> fun y -> (if y < 0 then 0 else y)

let rec calculateFuelForMassPlusFuel totalFuel mass =
    match mass with
    | x when x > 0 -> 
        let fuelForMass = calculateFuelForMass mass
        calculateFuelForMassPlusFuel (totalFuel + fuelForMass) fuelForMass
    | _ -> totalFuel

let calculateFuelForAllModules (masses:seq<string>) (fuelCalculator:int->int) = 
    masses
    |> Seq.map int
    |> Seq.sumBy fuelCalculator

