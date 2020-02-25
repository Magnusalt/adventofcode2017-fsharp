open System.IO
type OpCode = Inc | Dec
type Comparsions = Equal | NotEqual | Less | Greater | LessOrEqual | GreaterOrEqual

type Statement = | If of (string * Comparsions * int)

type InstructionRow =
    {RegisterName:string; OpCode:OpCode; RegisterValue:int; Predicate:Statement}

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let getPredicate reg operation value =
    let op =
        match operation with
        | "==" -> Equal
        | "!=" -> NotEqual
        | "<" -> Less
        | "<=" -> LessOrEqual
        | ">" -> Greater
        | ">=" -> GreaterOrEqual
        | _ -> invalidArg "operation" "should be a valid comparsion"
    If (reg, op, value)
let getOpcode oc =
    match oc with
    | "inc" -> Inc
    | "dec" -> Dec
    | _ -> invalidArg "oc" "should be inc or dec"
let parseInput (row:string) = 
    let rowArray = row.Split(' ')
    {
        RegisterName = rowArray.[0]; 
        OpCode = getOpcode rowArray.[1];
        RegisterValue = System.Convert.ToInt32 rowArray.[2]; 
        Predicate = (getPredicate rowArray.[4] rowArray.[5] (System.Convert.ToInt32 rowArray.[6])) 
    }

let executeInstruction (instruction:InstructionRow) (registry:System.Collections.Generic.Dictionary<string, int>) =
    let shouldExecute = match instruction.Predicate with
                        | If (r, c, v) ->
                            match c with
                            | Equal          -> registry.[r] = v
                            | NotEqual       -> registry.[r] <> v
                            | Less           -> registry.[r] < v
                            | LessOrEqual    -> registry.[r] <= v
                            | Greater        -> registry.[r] > v
                            | GreaterOrEqual -> registry.[r] >= v
    if shouldExecute then
        let oldValue = registry.[instruction.RegisterName];
        match instruction.OpCode with
        | Inc -> registry.[instruction.RegisterName] <- (oldValue + instruction.RegisterValue)
        | Dec -> registry.[instruction.RegisterName] <- (oldValue - instruction.RegisterValue)


let day8a input =
    let instructions = (Seq.toList input) |> List.map parseInput
    let registry = new System.Collections.Generic.Dictionary<string, int>()
    let keys = List.map (fun row -> row.RegisterName.ToString()) instructions |> List.distinct
    (List.iter (fun k -> registry.Add(k, 0)) keys)
    List.iter (fun r -> executeInstruction r registry) instructions
    
    Seq.toList registry.Values |> List.max

let testInput = ["b inc 5 if a > 1";"a inc 1 if b < 5";"c dec -10 if a >= 1";"c inc -20 if c == 10"]

//printfn "%i" (day8a (readLines "day8.txt"))
//printfn "%i" (day8a testInput)

let executeInstructionB (instruction:InstructionRow) (registry:System.Collections.Generic.Dictionary<string, int>) =
    let shouldExecute = match instruction.Predicate with
                        | If (r, c, v) ->
                            match c with
                            | Equal          -> registry.[r] = v
                            | NotEqual       -> registry.[r] <> v
                            | Less           -> registry.[r] < v
                            | LessOrEqual    -> registry.[r] <= v
                            | Greater        -> registry.[r] > v
                            | GreaterOrEqual -> registry.[r] >= v
    if shouldExecute then
        let oldValue = registry.[instruction.RegisterName];
        match instruction.OpCode with
        | Inc -> registry.[instruction.RegisterName] <- (oldValue + instruction.RegisterValue)
        | Dec -> registry.[instruction.RegisterName] <- (oldValue - instruction.RegisterValue)
    List.max (Seq.toList registry.Values) 


let day8b input =
    let instructions = (Seq.toList input) |> List.map parseInput
    let registry = new System.Collections.Generic.Dictionary<string, int>()
    let keys = List.map (fun row -> row.RegisterName.ToString()) instructions |> List.distinct
    (List.iter (fun k -> registry.Add(k, 0)) keys)
    List.map (fun r -> executeInstructionB r registry) instructions |> List.max

printfn "%i" (day8b (readLines "day8.txt"))
//printfn "%i" (day8b testInput)