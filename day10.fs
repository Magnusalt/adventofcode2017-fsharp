let seed = seq { 0..255 }
let testSeq = seq [0; 1; 2; 3; 4];
let testInput = [3; 4; 1; 5];
let input = [14;58;0;116;179;16;1;104;2;254;167;86;255;55;122;244]

let hash length currentPos seed =
    let (l, rt) = List.splitAt currentPos seed
    if currentPos + length > (List.length seed) then
        let overflow = (currentPos + length) - (List.length seed)
        printfn "--------------------------------"
        printfn "overflow: %i" overflow
        printfn "--------------------------------"
        let (overflowedPart, rest) = List.splitAt overflow l
        let overflowed = List.append rt overflowedPart
        let revOverflowed = List.rev overflowed
        if List.isEmpty rest then
            let diff = (List.length seed) - overflow
            printfn "First: "
            List.iter (fun i -> printf "%i," i) (List.skip diff revOverflowed)
            printfn ""

            let last = List.take overflow revOverflowed
            printfn "Last: "
            List.iter (fun i -> printf "%i," i) (List.take diff revOverflowed)
            printfn ""
            printfn ""
            List.append (List.skip diff revOverflowed) (List.take diff revOverflowed)
        else
            let first = List.skip overflow revOverflowed
            printfn "First: "
            List.iter (fun i -> printf "%i," i) first
            printfn "length %i: " (List.length first)

            printfn ""
            
            printfn "Middle: "
            List.iter (fun i -> printf "%i," i) rest
            printfn "length %i: " (List.length rest)
            printfn ""

            let last = List.take overflow revOverflowed
            printfn "Last: "
            List.iter (fun i -> printf "%i," i) last
            printfn "length %i: " (List.length last)
            printfn ""
            printfn ""

            List.append (List.append first rest) last
    else
        if length > 1 then
            let (r, t) = List.splitAt length rt
            printfn "First: "
            List.iter (fun i -> printf "%i," i) l
            printfn ""

            printfn "Middle: "
            List.iter (fun i -> printf "%i," i) (List.rev r)
            printfn "length %i: " (List.length r)

            printfn ""

            printfn "Last: "
            List.iter (fun i -> printf "%i," i) t
            printfn ""
            printfn ""

            List.append (List.append l (List.rev r)) t
        else
            seed

let rec run seed input currPos skip index =
    if index = List.length input then
        seed
    else
        let length = List.item index input
        let newSeed = hash length currPos seed
        let nextPos = (currPos + length + skip)%(List.length seed)
        run newSeed input nextPos (skip + 1) (index + 1)
    
let day10a (s:seq<int>) i =
    let res = run (Seq.toList s) i 0 0 0
    (List.item 0 res) * (List.item 1 res)

printfn "result: %i" (day10a seed input)