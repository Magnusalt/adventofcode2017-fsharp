open System.IO
open System.Collections.Generic
let readLines (filePath:string) = [
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () |> System.Convert.ToInt32
]

let updateInstructions instructions currentPos =
    let newValue = (List.item currentPos instructions) + 1
    let newInstruction = List.singleton (newValue)
    let leading = List.take currentPos instructions
    let trailing = List.skip (currentPos + 1) instructions
    List.append newInstruction trailing |> List.append leading

let rec jump instructions currentPosition numberOfJumps lowerBound upperBound =
    if currentPosition < lowerBound || currentPosition > upperBound then
        numberOfJumps
    else
        let nextJumpLength = List.item currentPosition instructions
        let nextPosition = currentPosition + nextJumpLength
        printfn "%i" currentPosition
        jump (updateInstructions instructions currentPosition) nextPosition (numberOfJumps + 1) lowerBound upperBound

let day5a input = 
    let start = 0
    let lowerBound = 0
    let upperBound = (List.length input) - 1

    jump input start 0 lowerBound upperBound

let test = [0;3;0;1;-3]

//printfn "number of jumps %i" (day5a (readLines "day5.txt"))

let updateInstructionsStrangely instructions currentPos directionModifier =
    let newValue = (List.item currentPos instructions) + (1 * directionModifier)
    let newInstruction = List.singleton (newValue)
    let leading = List.take currentPos instructions
    let trailing = List.skip (currentPos + 1) instructions
    List.append newInstruction trailing |> List.append leading

let getDirectionModifier offset =
    if  offset >= 3 then
        -1
    else
        1

let printVisual currentPos nbrOfJumps=
    let currentPosPart = float currentPos/1000.0
    let indicatorPos = System.Convert.ToInt32(floor (currentPosPart * 100.0))
    let s = String.init 100 (fun i -> if i = indicatorPos then "|" else "-")
    printfn "%s %i" s nbrOfJumps

let rec jumpStrangely instructions currentPosition numberOfJumps lowerBound upperBound =
    if currentPosition < lowerBound || currentPosition > upperBound then
        numberOfJumps
    else
        let nextJumpLength = List.item currentPosition instructions
        let directionModifier = getDirectionModifier nextJumpLength
        let nextPosition = currentPosition + nextJumpLength
        jumpStrangely (updateInstructionsStrangely instructions currentPosition directionModifier) nextPosition (numberOfJumps + 1) lowerBound upperBound

let day5b input = 
    let start = 0
    let lowerBound = 0
    let upperBound = (List.length input) - 1

    jumpStrangely input start 0 lowerBound upperBound

//printfn "number of jumps %i" (day5b (readLines "day5.txt"))
//printfn "number of jumps %i" (day5b test)

let rec jumpStrangelyMutable (instructions:System.Collections.Generic.List<int>) currentPosition numberOfJumps lowerBound upperBound =
    if currentPosition < lowerBound || currentPosition > upperBound then
        numberOfJumps
    else
        let nextJumpLength = instructions.[currentPosition]
        let directionModifier = getDirectionModifier nextJumpLength
        let nextPosition = currentPosition + nextJumpLength
        instructions.[currentPosition] <- (nextJumpLength + directionModifier)
        printVisual currentPosition numberOfJumps
        jumpStrangelyMutable instructions nextPosition (numberOfJumps + 1) lowerBound upperBound


let day5bMutable input =
    let mutableList = new List<int>()
    mutableList.AddRange(input)
    let start = 0
    let lowerBound = 0
    let upperBound = (List.length input) - 1
    jumpStrangelyMutable mutableList start 0 lowerBound upperBound

printfn "number of jumps %i" (day5bMutable (readLines "day5.txt"))
