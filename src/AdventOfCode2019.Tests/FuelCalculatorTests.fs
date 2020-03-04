module AdventOfCode2019.Tests

open NUnit.Framework
open FuelCalculator
open Lib

[<SetUp>]
let Setup () =
    ()

[<Test>]
let FuelForSingleModuleMassCalculatedCorrectly () =
    Assert.AreEqual(0, calculateFuelForMass 3)
    Assert.AreEqual(2, calculateFuelForMass 12)
    Assert.AreEqual(2, calculateFuelForMass 14)
    Assert.AreEqual(654, calculateFuelForMass 1969)
    Assert.AreEqual(33583, calculateFuelForMass 100756)

[<Test>]
let FuelForSingleModuleMassPlusFuelCalculatedCorrectly () =
    Assert.AreEqual(0, calculateFuelForMassPlusFuel 0 3)
    Assert.AreEqual(2, calculateFuelForMassPlusFuel 0 12)
    Assert.AreEqual(2, calculateFuelForMassPlusFuel 0 14)
    Assert.AreEqual(966, calculateFuelForMassPlusFuel 0 1969)
    Assert.AreEqual(50346, calculateFuelForMassPlusFuel 0 100756)

[<Test>]
let FuelForAllModulesCalculatedCorrectly () =
    Assert.AreEqual(34241, calculateFuelForAllModules (readLines "fuelCalcTestData.txt") calculateFuelForMass)

[<Test>]
let FuelForAllModulesPlusFuelCalculatedCorrectly () =
    Assert.AreEqual(51316, calculateFuelForAllModules (readLines "fuelCalcTestData.txt") (calculateFuelForMassPlusFuel 0))
