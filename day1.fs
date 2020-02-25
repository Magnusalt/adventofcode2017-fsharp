open System.IO
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
let rec numberStringToIntSeqRec (numberString:string) (intSeq:seq<int>) = 
    match numberString.Length with
    | 0 -> intSeq
    | _ -> 
        let parsedInt = Seq.singleton (System.Convert.ToInt32 (numberString.[0].ToString()))
        let newIntSeq = Seq.append intSeq parsedInt
        let newNumberString = numberString.Substring(1)
        numberStringToIntSeqRec newNumberString newIntSeq
let singleRowNumberStringToIntSeq (numberSeq: seq<string>) =
    let line = Seq.head numberSeq
    let emptyIntSeq = Seq.empty :> seq<int>
    numberStringToIntSeqRec line emptyIntSeq
let matchFirstLast (input:seq<int>) =
    let head = Seq.head input
    let last = Seq.last input
    if (head = last) then
        head
    else
        0
let day1a (input:seq<string>) =
    let intSeq = singleRowNumberStringToIntSeq input
    let filter (i: int[]) = i.[0] = i.[1]
    let takeFirst (i: int[]) = i.[0]
    (Seq.sumBy takeFirst (Seq.windowed 2 intSeq |> Seq.filter filter)) + (matchFirstLast intSeq) 
// Test and Solve day 1 A
let testSeq1a = Seq.singleton "1122"
let testSeq2a = Seq.singleton "1111"
printfn "test 1a: %i" (testSeq2a |> day1a)
printfn "Solution 1a: %i" (readLines "day1.txt" |> day1a)
// Test and Solve day 1 B
let rec sumIfEqual (first:seq<int>) (second:seq<int>) (sum:int) =
    match (Seq.length first) with
    | 0 -> sum
    | _ ->
        if Seq.head first = Seq.head second then
            sumIfEqual (Seq.skip 1 first) (Seq.skip 1 second) (sum + (Seq.head first) + (Seq.head second))
        else
            sumIfEqual (Seq.skip 1 first) (Seq.skip 1 second) sum
let day1b (input:seq<string>) =
    let intSeq = singleRowNumberStringToIntSeq input
    let halfLength = (Seq.length intSeq)/2
    let firstHalf = Seq.take halfLength intSeq
    let secondHalf = Seq.skip halfLength intSeq
    sumIfEqual firstHalf secondHalf 0
let testSeq1b = Seq.singleton "12131415"
let testSeq2b = Seq.singleton "123123"
printfn "test 1b: %i" (day1b testSeq2b)
printfn "Solution 1b: %i" (readLines "day1.txt" |> day1b)