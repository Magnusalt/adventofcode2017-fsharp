let getIndexOfFirstMax (state:List<int>) =
    let max = List.max state
    ((List.findIndex (fun v -> v = max) state), max)

let rec addLast (state:List<int>) leftToAdd index lastIndex =
    match leftToAdd with
    | 0 -> state
    | _ -> addLast (List.mapi (fun i v -> if i = index then v + 1 else v) state) (leftToAdd - 1) (if index < lastIndex then index + 1 else 0) lastIndex

let distribute state =
    let length = List.length state
    let (index, max) = getIndexOfFirstMax state
    let addToSome = max%length
    let addToAll = (max - addToSome)/length
    let almostNewState = List.mapi (fun i v -> if i = index then addToAll else v + addToAll) state
    addLast almostNewState addToSome (if index = length - 1 then 0 else index + 1) (length - 1)

let stringify (state:List<int>) =
    List.reduce (fun i1 i2 -> i1 + "," + i2 ) (List.map (fun i -> i.ToString()) state)

let rec balance currentState record iterations =
    let currentStateAsString = stringify currentState
    if List.contains currentStateAsString record then
        iterations
    else
        let newRecord = currentStateAsString::record
        balance (distribute currentState) newRecord (iterations + 1)


let test = [0;2;7;0]
let day6Input = [4;10;4;1;8;4;9;14;5;1;14;15;0;15;3;5]
let day6a input =
    balance input List.empty 0

printfn "%i" (day6a day6Input)

let rec balanceB currentState record iterations =
    let currentStateAsString = stringify currentState
    if List.contains currentStateAsString record then
        currentStateAsString::record
    else
        let newRecord = currentStateAsString::record
        balanceB (distribute currentState) newRecord (iterations + 1)


let day6b input =
    let record = balanceB input List.empty 0
    List.iter (fun s -> printfn "%s" s) record
    let toFind = List.head record
    let indexOf = List.findIndex (fun i -> i = toFind) record.Tail
    printfn "index is %i" indexOf
    List.length (List.take (indexOf + 1) record.Tail) 

printfn "answer b is %i" (day6b day6Input)