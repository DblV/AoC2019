module PasswordCrackerTests

open NUnit.Framework
open PasswordCracker

[<SetUp>]
let Setup () =
    ()

[<TestCase("111111", false)>]
[<TestCase("111122", true)>]
[<TestCase("112233", true)>]
[<TestCase("123444", false)>]
[<TestCase("223450", false)>]
[<TestCase("123789", false)>]
[<TestCase("443200", false)>]
let PasswordCriteriaCheckRunsCorrectly testCase expectedResult =
    Assert.AreEqual(expectedResult, meetsCriteria testCase)
