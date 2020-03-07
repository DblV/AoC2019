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
    compareOutput [99] (runIntcodeFromString "99")

[<Test>]
let Opcode1OutputCorrect () =
    compareOutput [2;0;0;0;99] (runIntcodeFromString "1,0,0,0,99")

[<Test>]
let Opcode2OutputCorrect () =
    compareOutput [2;3;0;6;99] (runIntcodeFromString "2,3,0,3,99")
    compareOutput [2;4;4;5;99;9801] (runIntcodeFromString "2,4,4,5,99,0")

[<Test>]
let MultipleOpcodeInputOutputsCorrect () =
    compareOutput [30;1;1;4;2;5;6;0;99] (runIntcodeFromString "1,1,1,4,99,5,6,0,99")
    compareOutput [3500;9;10;70;2;3;11;0;99;30;40;50] (runIntcodeFromString "1,9,10,3,2,3,11,0,99,30,40,50")

