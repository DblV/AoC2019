module IntcodeTests

open NUnit.Framework
open Intcode

[<SetUp>]
let Setup () =
    ()

let compareOutput expectedList actualList =
    let result = List.compareWith (fun elem1 elem2 -> if elem1 <> elem2 then 1 else 0) expectedList actualList
    Assert.AreEqual(0, result)

[<Test>]
let Opcode99OutputCorrect () =
    compareOutput [99] (runIntcodeComputerFromStringInput "99")

[<Test>]
let Opcode1OutputCorrect () =
    compareOutput [2;0;0;0;99] (runIntcodeComputerFromStringInput "1,0,0,0,99")

[<Test>]
let Opcode2OutputCorrect () =
    compareOutput [2;3;0;6;99] (runIntcodeComputerFromStringInput "2,3,0,3,99")
    compareOutput [2;4;4;5;99;9801] (runIntcodeComputerFromStringInput "2,4,4,5,99,0")

[<Test>]
let MultipleOpcodeInputOutputsCorrect () =
    compareOutput [30;1;1;4;2;5;6;0;99] (runIntcodeComputerFromStringInput "1,1,1,4,99,5,6,0,99")
    compareOutput [3500;9;10;70;2;3;11;0;99;30;40;50] (runIntcodeComputerFromStringInput "1,9,10,3,2,3,11,0,99,30,40,50")

[<TestCase("1,0,0,0,99",2,0)>]
[<TestCase("1,0,0,0,99",198,404)>]
[<TestCase("1,0,0,0,99,1,21",6,501)>]
[<TestCase("1,0,0,0,99,1,21",120,604)>]
[<TestCase("1,0,0,0,1,8,8,4,99",101,802)>]
[<TestCase("1,0,0,0,1,8,8,4,99",13,501)>]
[<TestCase("1,0,0,0,2,8,8,4,99,50,100",54,907)>]
[<TestCase("1,0,0,0,2,8,8,4,99,50,100",103,807)>]
[<TestCase("1,0,0,0,2,8,8,4,99,50,100",199,1008)>]
let GivenInputStringAndTargetWhenSearchRunsItReturnsCorrectOutput input target expectedOutput =
    Assert.AreEqual(expectedOutput, (runSearchForOutputFromStringInput input target))
