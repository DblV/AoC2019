module FuelManagementSystemTests

open NUnit.Framework
open FuelManagementSystem
open Lib

[<SetUp>]
let Setup () =
    ()

[<Test>]
let SimpleWiringPathResolvesCorrectly () =
    let wire1Path = "R8,U5,L5,D3"
    let wire2Path = "U7,R6,D4,L4"
    Assert.AreEqual(6, calculateNearestIntersection wire1Path wire2Path)
