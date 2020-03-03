module DayOne

let calculateFuel mass =
    mass / 3
    |> fun x -> (x - 2)

let calculateTotalFuel (masses:seq<string>) = 
    masses
    |> Seq.map int
    |> Seq.sumBy calculateFuel
