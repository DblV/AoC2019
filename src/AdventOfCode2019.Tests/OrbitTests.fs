module OrbitTests

open NUnit.Framework
open Orbits

[<SetUp>]
let Setup () =
    ()

let compareAndLog x y =
    if x = y then
        0
    else
        printfn "Comparer failed: value 1 was %A, value 2 was %A" x y
        1

let compareList expectedList actualList =
    let result = List.compareWith compareAndLog expectedList actualList
    Assert.AreEqual(0, result)

[<Test>]
let ParseTests () =
    compareList [("COM","B")] (parseMapData ["COM)B"])
    compareList [("B","C")] (parseMapData ["B)C"])
    compareList [("H1R","Z5F")] (parseMapData ["H1R)Z5F"])

[<Test>]
let OrbitCountTests () =
    Assert.AreEqual(1, countAllOrbits ["COM)A"])
    Assert.AreEqual(7, countAllOrbits ["COM)A";"A)B";"A)C";"A)D"])
    Assert.AreEqual(10, countAllOrbits ["COM)A";"A)B";"B)C";"C)D"])
    Assert.AreEqual(22, countAllOrbits ["A)B";"B)C";"COM)A";"C)D";"B)E";"D)G";"E)F"])
    Assert.AreEqual(42, (countAllOrbits ["COM)B";"B)C";"C)D";"D)E";"E)F";"B)G";"G)H";"D)I";"E)J";"J)K";"K)L)"]))
