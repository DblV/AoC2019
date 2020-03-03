module AdventOfCode2019.Tests

open NUnit.Framework
open DayOne
open Lib

[<SetUp>]
let Setup () =
    ()

[<Test>]
let SingleFuelCalculatedCorrectly () =
    Assert.AreEqual(2, calculateFuel 12)
    Assert.AreEqual(2, calculateFuel 14)
    Assert.AreEqual(654, calculateFuel 1969)
    Assert.AreEqual(33583, calculateFuel 100756)

[<Test>]
let TotalFuelCalculatedCorrectly () =
    Assert.AreEqual(34241, calculateTotalFuel (readLines "day1testdata.txt"))
