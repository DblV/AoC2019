module IntcodeTests

open NUnit.Framework
open Intcode

[<SetUp>]
let Setup () =
    ()

let compareAndLog x y =
    if x = y then
        0
    else
        printfn "Comparer failed: value 1 was %i, value 2 was %i" x y
        1

let compareResultingList expectedList actualOutput =
    let actualOutputMemory, _, _ = actualOutput
    let result = List.compareWith (fun elem1 elem2 -> if elem1 <> elem2 then 1 else 0) expectedList actualOutputMemory
    Assert.AreEqual(0, result)

let compareResultingListAndOutput expectedList expectedOutputValues actualOutput =
    let actualOutputMemory, _, actualOutputValues = actualOutput
    let memoryComparison = List.compareWith compareAndLog expectedList actualOutputMemory
    Assert.AreEqual(0, memoryComparison)
    let outputValueComparison = List.compareWith compareAndLog expectedOutputValues actualOutputValues
    Assert.AreEqual(0, outputValueComparison)

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
    compareResultingListAndOutput [5;0;99] [] (runIntcodeComputerFromStringInputWithInitialInput "3,0,99" 5)
    compareResultingListAndOutput [3;3;99;10] [] (runIntcodeComputerFromStringInputWithInitialInput "3,3,99,1" 10)

[<Test>]
let Opcode4OutputCorrect () =
    compareResultingListAndOutput [4;0;99] [4] (runIntcodeComputerFromStringInputWithInitialInput "4,0,99" 5)
    compareResultingListAndOutput [4;3;99;1] [1] (runIntcodeComputerFromStringInputWithInitialInput "4,3,99,1" 10)
    compareResultingListAndOutput [104;2;99] [2] (runIntcodeComputerFromStringInputWithInitialInput "104,2,99" 3)
    compareResultingListAndOutput [104;-1;104;-2;104;-3;99] [-1;-2;-3] (runIntcodeComputerFromStringInputWithInitialInput "104,-1,104,-2,104,-3,99" 3)

[<Test>]
let Opcode5OutputCorrect () =
    compareResultingList [105;0;0;99] (runIntcodeComputerFromStringInput "105,0,0,99")
    compareResultingList [5;2;3;7;0;0;0;99] (runIntcodeComputerFromStringInput "5,2,3,7,0,0,0,99")

[<Test>]
let Opcode6OutputCorrect () =
    compareResultingList [106;5;5;99] (runIntcodeComputerFromStringInput "106,5,5,99")
    compareResultingList [6;4;5;0;0;7;0;99] (runIntcodeComputerFromStringInput "6,4,5,0,0,7,0,99")

[<Test>]
let Opcode7OutputCorrect () =
    compareResultingList [1;3;4;0;99] (runIntcodeComputerFromStringInput "1107,3,4,0,99")
    compareResultingList [7;0;0;2;99] (runIntcodeComputerFromStringInput "7,0,1,2,99")

[<Test>]
let Opcode8OutputCorrect () =
    compareResultingList [1;5;5;0;99] (runIntcodeComputerFromStringInput "1108,5,5,0,99")
    compareResultingList [0;0;1;0;99] (runIntcodeComputerFromStringInput "8,0,1,0,99")

[<Test>]
let MultipleOpcodeInputOutputsCorrect () =
    compareResultingList [30;1;1;4;2;5;6;0;99] (runIntcodeComputerFromStringInput "1,1,1,4,99,5,6,0,99")
    compareResultingList [3500;9;10;70;2;3;11;0;99;30;40;50] (runIntcodeComputerFromStringInput "1,9,10,3,2,3,11,0,99,30,40,50")
    compareResultingList [101;9;10;90;1001;3;11;0;99;9;5] (runIntcodeComputerFromStringInput "11102,9,10,3,1001,3,11,0,99,9,5")
    compareResultingListAndOutput [901;3;1001;2;-100;0;99] [] (runIntcodeComputerFromStringInputWithInitialInput "3,3,1001,1,-100,0,99" 2)
    compareResultingListAndOutput [11102;-1;-100;100;4;3;99] [100] (runIntcodeComputerFromStringInputWithInitialInput "11102,-1,-100,3,4,3,99" 1)
    compareResultingListAndOutput [1105;1;5;0;0;1;0;1;1106;4;8;99] [1106] (runIntcodeComputerFromStringInputWithInitialInput "1105,1,5,0,0,1,0,1,8,4,8,99" -1)
    compareResultingList [99;100;-1;0;1106;0;0] (runIntcodeComputerFromStringInput "1101,100,-1,0,1106,0,0")
    compareResultingList [1;0;6;0;0;0;1108;1;1;0;99] (runIntcodeComputerFromStringInput "1106,0,6,0,0,0,1108,1,1,0,99")
    compareResultingList [1;4;5;0;1105;1105;7;99] (runIntcodeComputerFromStringInput "8,4,5,0,1105,1105,7,99")
    compareResultingListAndOutput [3;9;8;9;10;9;4;9;99;1;8] [1] (runIntcodeComputerFromStringInputWithInitialInput "3,9,8,9,10,9,4,9,99,-1,8" 8)
    compareResultingListAndOutput [3;9;7;9;10;9;4;9;99;0;8] [0] (runIntcodeComputerFromStringInputWithInitialInput "3,9,7,9,10,9,4,9,99,-1,8" 8)
    compareResultingListAndOutput [3;3;1108;0;8;3;4;3;99] [0] (runIntcodeComputerFromStringInputWithInitialInput "3,3,1108,-1,8,3,4,3,99" 10)
    compareResultingListAndOutput [3;3;1107;1;8;3;4;3;99] [1] (runIntcodeComputerFromStringInputWithInitialInput "3,3,1107,-1,8,3,4,3,99" 5)
    compareResultingListAndOutput [3;12;6;12;15;1;13;14;13;4;13;99;0;0;1;9] [0] (runIntcodeComputerFromStringInputWithInitialInput "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 0)
    compareResultingListAndOutput [3;12;6;12;15;1;13;14;13;4;13;99;1;1;1;9] [1] (runIntcodeComputerFromStringInputWithInitialInput "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 1)
    compareResultingListAndOutput [3;3;1105;0;9;1101;0;0;12;4;12;99;0] [0] (runIntcodeComputerFromStringInputWithInitialInput "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 0)
    compareResultingListAndOutput [3;3;1105;1;9;1101;0;0;12;4;12;99;1] [1] (runIntcodeComputerFromStringInputWithInitialInput "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 1)

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
