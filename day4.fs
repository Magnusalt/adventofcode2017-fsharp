open System.IO
open System.Text
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
let splitOnSpace (input:seq<string>) = 
    Seq.map (fun (r:string) -> r.Split ' ') input

let day4a (input:seq<string[]>) =
    let distincts = Seq.map Seq.distinct input
    let isMatching = Seq.map2 (fun (i:string[]) d -> i.Length = (Seq.length d)) input distincts
    let matches = Seq.where (id) isMatching
    Seq.length matches

printfn "day 4a: %i" ((readLines "day4.txt") |> splitOnSpace |> day4a)

let sortStringChars (stringToSort:string) =
    let sb = StringBuilder()
    let s = sb.Append (Seq.toArray (stringToSort.ToCharArray() |> Seq.sort))
    s.ToString()
let day4b (input:seq<string[]>) =
    (Seq.map (fun s -> (Seq.toArray (Seq.map sortStringChars s))) input) |> day4a 

printfn "day 4b: %i" ((readLines "day4.txt") |> splitOnSpace |> day4b)