open System.IO
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let splitOnTab (input:seq<string>) = 
    Seq.map (fun (r:string) -> r.Split ((char)9)) input

let parseStringsToInts (input:seq<string[]>) =
    Seq.map (fun s -> Seq.map (fun n -> System.Int32.Parse(n)) s) input

let day2a (input:seq<string>) =
    let minMax = splitOnTab input |> parseStringsToInts |> Seq.map (fun i -> Seq.min i, Seq.max i)
    Seq.sumBy (fun ((min,max):int*int) -> max-min) minMax
    

//printfn "Solution 2a: %i" (readLines "day2.txt" |> day2a)

let rec findEvenDivisorsInSortedSeq (input:seq<int>) =
    let head = Seq.head input 
    let tail = Seq.tail input
    let possibleDivisors = Seq.filter (fun i -> i <= head/2) tail
    for i in possibleDivisors do
        printf "%i, " i
    printfn ""
    if Seq.exists (fun i -> head%i = 0) possibleDivisors then
        let res = Seq.find (fun i -> head%i = 0) possibleDivisors
        printfn "%i and %i" head res
        head/res
    else
        findEvenDivisorsInSortedSeq tail

let day2b (input:seq<string>) =
    Seq.sumBy (Seq.sortDescending >> findEvenDivisorsInSortedSeq) (splitOnTab input |> parseStringsToInts)

printfn "Solution 2a: %i" (readLines "day2.txt" |> day2b)