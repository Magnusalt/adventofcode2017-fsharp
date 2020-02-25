open System.IO
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let splitInput input =
    List.map (fun (s:string) -> s.Split(' ') ) input

let removeTrailingComma (s:string) =
    String.filter (fun c -> c <> ',') s
let getChildren splitInput =
    (List.map ((fun a -> Array.skip 3 a) >> (fun s -> (Array.map removeTrailingComma s))) (List.where (fun (sa:string[])-> (Array.length sa) > 3) splitInput)) |> List.reduce (Array.append)

let day7a input =
    let arrays = splitInput input
    let children = getChildren arrays |> Array.toList
    let firstColumn = List.map (fun (sa:string[]) -> sa.[0] ) arrays
    List.find (fun s -> (not (List.exists (fun ae -> ae = s) children))) firstColumn
    
//printfn "%s" (day7a  (Seq.toList (readLines "day7.txt")))

type Node (weight:int, name:string) =
    let children = new System.Collections.Generic.List<Node>()
    member this.Name = name
    member this.Weight = weight
    member this.AddChild (node:Node) =
        children.Add(node)
    member this.Children = children

let getWeight weightP =
    let num = String.filter (fun c -> c <> '(' && c <> ')') weightP
    System.Convert.ToInt32 num

let getChildNames row =
    match Array.length row with
    | 2 -> List.empty<string> 
    | _ -> row |> Array.skip 3 |> Array.map removeTrailingComma |> Array.toList

let findAndRemove input nodeName =
    let indexToRemove = List.findIndex (fun (a:string[]) -> a.[0] = nodeName) input
    let (lead, tail) = List.splitAt indexToRemove input
    ((List.item indexToRemove input), (List.append lead tail ))

let rec buildTree input (currentNodeRow:string[]) =
    let node = Node ((getWeight currentNodeRow.[1]), currentNodeRow.[0])
    let childNames = getChildNames currentNodeRow
    List.iter ( fun name ->
                    let (nodeAsArray, inputLeft) = findAndRemove input name
                    node.AddChild(buildTree inputLeft nodeAsArray)) childNames
    node

let rec getTowerWeight (node:Node) level =
    let childNodes = List.ofSeq node.Children
    let weights = List.map (fun n -> getTowerWeight n (level + 1)) childNodes
    if (not (List.isEmpty weights)) then
        let allSame = (List.distinct weights |> List.length) = 1 
        if not allSame then
                printfn "level: %i acc-w: %i node-w: %i name: %s" level (node.Weight + (List.sum weights)) node.Weight node.Name
                List.iter (fun (n:Node) -> printf "%i, " n.Weight) childNodes
                printfn ""
                List.iter (fun (w) -> printf "%i, " w) weights
                printfn ""
    node.Weight + (List.sum weights)

let day7b input =
    let rootName = day7a input
    let arrays = splitInput input
    let (nodeAsArray, inputLeft) = findAndRemove arrays rootName
    let tree = buildTree inputLeft nodeAsArray 
    getTowerWeight tree 0

let testData = ["pbga (66)";"xhth (57)";"ebii (61)";"havc (66)";"ktlj (57)";"fwft (72) -> ktlj, cntj, xhth";"qoyq (66)";"padx (45) -> pbga, havc, qoyq";"tknk (41) -> ugml, padx, fwft";"jptl (61)";"ugml (68) -> gyxo, ebii, jptl";"gyxo (61)";"cntj (57)"]

printfn "answer %i" (day7b (Seq.toList (readLines "day7.txt")))

//printfn "%i" (day7b testData)

// right answer is 1206, difference between highest level where differs