let checkToCorrect =
    let getOpenBr c =
        match c with
        | ')' -> '('
        | '}' -> '{'
        | ']' -> '['

    let rec checkToCorrectRec (stack : List<char>) (str : string) =
        match (str.Length, List.length stack) with
        | (0, 0) -> true
        | (0, _) -> false
        | (_, _) ->
            match str.[0] with
            | c when (c = '(') || (c = '{') || (c = '[') -> checkToCorrectRec (c::stack) (str.Substring(1))
            | c when (c = ')') || (c = '}') || (c = ']') -> if List.length stack = 0 || List.head stack <> (getOpenBr c) then false else checkToCorrectRec (List.tail stack) (str.Substring(1))
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

[<EntryPoint>]
let main argv =
    0