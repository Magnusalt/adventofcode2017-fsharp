open System.IO
let readLine (filePath:string) = 
    use sr = new StreamReader (filePath)
    sr.ReadLine()

let pushToStack c (stack:list<char>) =
    if stack.Length > 0 then
        match stack.Head with
        | '<' -> stack
        | _ -> c::stack
    else
        c::stack

let popFromStack c (stack:list<char>) =
    match stack.Head with
    | '<' ->
        match c with
        | '>' -> (stack.Tail, false)
        | _ -> (stack, false)
    | _ -> (stack.Tail, true)

let rec checkChar (inputString:string) (stack:list<char>) sum index =
    if index < (String.length inputString) then
        let c = inputString.[index];
        List.iter (fun c -> printf "%c" c) stack
        printfn ""
        match c with
        | '{' -> checkChar inputString (pushToStack c stack) sum (index + 1)
        | '}' ->
            let (newStack, shouldCount) = popFromStack c stack
            checkChar inputString newStack (if shouldCount then sum + List.length stack else sum) (index + 1)
        | '<' -> checkChar inputString (pushToStack c stack) sum (index + 1)
        | '>' ->
            let (newStack, _) = popFromStack c stack 
            checkChar inputString newStack sum (index + 1)
        | '!' -> checkChar inputString stack sum (index + 2)
        | _ -> checkChar inputString stack sum (index + 1)
    else
        sum

let day9a (input:string) =
    checkChar input List.Empty 0 0
    
let test = "{{<a!>},{<a!>},{<a!>},{<ab>}}"

printfn "result: %i" (day9a (readLine "day9.txt"))

let shouldCountGarbage (stack:list<char>) (c:char) =
    if stack.Length > 0 then
        match stack.Head with
        | '<' ->
            match c with
            | '>' -> 0
            | _ -> 1
        | _ -> 0
    else 
        0

let rec checkCharB (inputString:string) (stack:list<char>) sum index garbageCount =
    if index < (String.length inputString) then
        let c = inputString.[index];
        List.iter (fun c -> printf "%c" c) stack
        printfn ""
        match c with
        | '{' -> checkCharB inputString (pushToStack c stack) sum (index + 1) (garbageCount + (shouldCountGarbage stack c))
        | '}' ->
            let (newStack, shouldCount) = popFromStack c stack
            checkCharB inputString newStack (if shouldCount then sum + List.length stack else sum) (index + 1) (garbageCount + (shouldCountGarbage stack c))
        | '<' -> checkCharB inputString (pushToStack c stack) sum (index + 1) (garbageCount + (shouldCountGarbage stack c))
        | '>' ->
            let (newStack, _) = popFromStack c stack 
            checkCharB inputString newStack sum (index + 1) (garbageCount + (shouldCountGarbage stack c))
        | '!' -> checkCharB inputString stack sum (index + 2) garbageCount
        | _ -> checkCharB inputString stack sum (index + 1) (garbageCount + (shouldCountGarbage stack c))
    else
        (sum, garbageCount)

let testb = "<{o\"i!a,<{i<a>"

let day9b (input:string) =
    checkCharB input List.Empty 0 0 0

//printfn "result: %O" (day9b (readLine "day9.txt"))