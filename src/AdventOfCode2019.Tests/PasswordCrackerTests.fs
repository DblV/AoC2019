module PasswordCrackerTests

open NUnit.Framework
open PasswordCracker

[<SetUp>]
let Setup () =
    ()

[<TestCase("111111", true)>]
[<TestCase("223450", false)>]
[<TestCase("123789", false)>]
[<TestCase("443200", false)>]
let PasswordCriteriaCheckRunsCorrectly testCase expectedResult =
    Assert.AreEqual(expectedResult, meetsCriteria testCase)
