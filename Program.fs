open NUnit.Framework 
open FsUnit
open FsCheck

let checkToCorrect =
    let rec checkToCorrectRec (stack : List<char>) (str : string) =
        if (str.Length = 0 && (List.length stack) = 0) 
        then true
        elif (str.Length = 0 && (List.length stack) > 0)
        then false 
        else
            match str.[0] with
            | '(' -> checkToCorrectRec ('('::stack) (str.Substring(1))
            | '[' -> checkToCorrectRec ('['::stack) (str.Substring(1))
            | '{' -> checkToCorrectRec ('{'::stack) (str.Substring(1))
            | ')' -> if List.length stack = 0 || List.head stack <> '(' then false else checkToCorrectRec (List.tail stack) (str.Substring(1))
            | ']' -> if List.length stack = 0 || List.head stack <> '[' then false else checkToCorrectRec (List.tail stack) (str.Substring(1))
            | '}' -> if List.length stack = 0 || List.head stack <> '{' then false else checkToCorrectRec (List.tail stack) (str.Substring(1))
            | _ -> false
    checkToCorrectRec []

module pointFreeFunc =
    let func x l = 
        List.map (fun y -> y * x) l
    let func'1 x =
        List.map (fun y -> y * x)
    let func'2 x =
        List.map (fun y -> (*) x y)
    let func'3 x =
        List.map ((*) x)
    let func'4 =
        List.map << (*)

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
let checkFunc x l = 
    (pointFreeFunc.func x l) = (pointFreeFunc.func'4 x l)

[<EntryPoint>]
let main argv =
    ``test of check to correct bracket sequence 1`` ()
    ``test of check to correct bracket sequence 2`` ()
    ``test of check to correct bracket sequence 3`` ()
    ``test of check to correct bracket sequence 4`` ()
    ``test of check to correct bracket sequence 5`` ()

    Check.Quick checkFunc
    0 
