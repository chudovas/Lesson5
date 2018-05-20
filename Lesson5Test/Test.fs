open NUnit.Framework 
open FsUnit
open FsCheck
open Lesson5

open System

[<Test>]
let ``test of check to correct bracket sequence 1`` () =
    checkToCorrect "(({}[({}[])]))" |> should be True

[<Test>]
let ``test of check to correct bracket sequence 2`` () =
    checkToCorrect "(){}[({}[])]))" |> should be False

[<Test>]
let ``test of check to correct bracket sequence 3`` () =
    checkToCorrect "}]))}[({}[])]))" |> should be False

[<Test>]
let ``test of check to correct bracket sequence 4`` () =
    checkToCorrect "" |> should be True

[<Test>]
let ``test of check to correct bracket sequence 5`` () =
    checkToCorrect "{Hi!}" |> should be False

[<Test>]
let ``test of point free func`` () =
    let checkFunc x l = 
        if ((pointFreeFunc.func x l) = (pointFreeFunc.func'4 x l))
        then true
        else failwith ("Bad arguments: " + x.ToString() + " " + l.ToString())
    Check.Quick checkFunc


[<EntryPoint>]
let main argv =
    ``test of check to correct bracket sequence 1`` ()
    ``test of check to correct bracket sequence 2`` ()
    ``test of check to correct bracket sequence 3`` ()
    ``test of check to correct bracket sequence 4`` ()
    ``test of check to correct bracket sequence 5`` ()
    ``test of point free func`` ()
    0 
