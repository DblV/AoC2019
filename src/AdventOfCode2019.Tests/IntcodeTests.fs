module IntcodeTests

open NUnit.Framework
open Intcode

[<SetUp>]
let Setup () =
    ()

let compareResultingList expectedList actualOutput =
    let actualOutputMemory, _ = actualOutput
    let result = List.compareWith (fun elem1 elem2 -> if elem1 <> elem2 then 1 else 0) expectedList actualOutputMemory
    printfn "Expected list %A | Actual list %A" expectedList actualOutputMemory
    Assert.AreEqual(0, result)

let compareResultingListAndOutput expectedList expectedOutputValue actualOutput =
    let actualOutputMemory, actualOutputValue = actualOutput
    let result = List.compareWith (fun elem1 elem2 -> if elem1 <> elem2 then 1 else 0) expectedList actualOutputMemory
    printfn "Expected list %A | Actual list %A" expectedList actualOutputMemory
    Assert.AreEqual(0, result)
    Assert.AreEqual(expectedOutputValue, actualOutputValue)

[<Test>]
let Opcode99OutputCorrect () =
    compareResultingList [99] (runIntcodeComputerFromStringInput "99")

[<Test>]
let Opcode1OutputCorrect () =
    compareResultingList [2;0;0;0;99] (runIntcodeComputerFromStringInput "1,0,0,0,99")
    compareResultingList [11101;4;5;9;99] (runIntcodeComputerFromStringInput "11101,4,5,3,99")

[<Test>]
let Opcode2OutputCorrect () =
    compareResultingList [2;3;0;6;99] (runIntcodeComputerFromStringInput "2,3,0,3,99")
    compareResultingList [2;4;4;5;99;9801] (runIntcodeComputerFromStringInput "2,4,4,5,99,0")
    compareResultingList [1002;4;3;4;99] (runIntcodeComputerFromStringInput "1002,4,3,4,33")

[<Test>]
let Opcode3OutputCorrect () =
    compareResultingListAndOutput [5;0;99] 5 (runIntcodeComputerFromStringInputWithInitialInput "3,0,99" 5)
    compareResultingListAndOutput [3;3;99;10] 10 (runIntcodeComputerFromStringInputWithInitialInput "3,3,99,1" 10)

[<Test>]
let Opcode4OutputCorrect () =
    compareResultingListAndOutput [4;0;99] 4 (runIntcodeComputerFromStringInputWithInitialInput "4,0,99" 5)
    compareResultingListAndOutput [4;3;99;1] 1 (runIntcodeComputerFromStringInputWithInitialInput "4,3,99,1" 10)
    compareResultingListAndOutput [104;2;99] 99 (runIntcodeComputerFromStringInputWithInitialInput "104,2,99" 3)

[<Test>]
let MultipleOpcodeInputOutputsCorrect () =
    compareResultingList [30;1;1;4;2;5;6;0;99] (runIntcodeComputerFromStringInput "1,1,1,4,99,5,6,0,99")
    compareResultingList [3500;9;10;70;2;3;11;0;99;30;40;50] (runIntcodeComputerFromStringInput "1,9,10,3,2,3,11,0,99,30,40,50")
    compareResultingList [101;9;10;90;1001;3;11;0;99;9;5] (runIntcodeComputerFromStringInput "11102,9,10,3,1001,3,11,0,99,9,5")
    compareResultingListAndOutput [901;3;1001;2;-100;0;99] 2 (runIntcodeComputerFromStringInputWithInitialInput "3,3,1001,1,-100,0,99" 2)
    compareResultingListAndOutput [11102;-1;-100;100;4;3;99] 100 (runIntcodeComputerFromStringInputWithInitialInput "11102,-1,-100,3,4,3,99" 1)

[<TestCase("1,0,0,0,99",2,0)>]
[<TestCase("1,0,0,0,99",198,404)>]
[<TestCase("1,0,0,0,99,1,21",6,501)>]
[<TestCase("1,0,0,0,99,1,21",120,604)>]
[<TestCase("1,0,0,0,1,8,8,4,99",101,802)>]
[<TestCase("1,0,0,0,1,8,8,4,99",13,501)>]
[<TestCase("1,0,0,0,2,8,8,4,99,50,100",54,907)>]
[<TestCase("1,0,0,0,2,8,8,4,99,50,100",103,807)>]
[<TestCase("1,0,0,0,2,8,8,4,99,50,100",199,1008)>]
let IntCodeSearchReturnsCorrectOutput input target expectedOutput =
    Assert.AreEqual(expectedOutput, (runSearchForOutputFromStringInput input target))
