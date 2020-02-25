open System
// every circle increases with 8
// bottom corner, highest value for circle = sideLength squared
let input = 347991

let rec nthCircle p c carry =
    let accumCircum = (c * 8) + carry
    if (accumCircum + 1) >= p then
        (c, accumCircum+1)
    else
        nthCircle p (c+1) accumCircum

let findSideLength n = n * 2 - 1

let day3a position =
    let ((n, max):int*int) = nthCircle position 0 0
    let side = findSideLength (n +  1)
    printfn "n: %i sideSq: %i max: %i" n (side * side) max
    let firstOrthognal = max - (side-1)/2
    let secondOrthogonal = firstOrthognal - (side-1)
    let thirdOrthogonal = secondOrthogonal - (side-1)
    let fourthOrthogonal = thirdOrthogonal - (side-1)
    let orthogonals = [firstOrthognal; secondOrthogonal; thirdOrthogonal; fourthOrthogonal] :> seq<int>
    (Seq.map (fun o -> abs (o-position)) orthogonals |> Seq.min) + n

printfn "%i" (day3a input)

type MemPos = {x:int; y:int; v:int}
// loop - step out to new, step up sideLength - 1, step left sideLength, step down sideLength, step right sideLengt

let sumNeighbours (visited:seq<MemPos>) x y =
    let neighbours = seq { for horizontal in x - 1 .. x + 1 do
                               for vertical in y - 1 .. y + 1 do
                                   yield (horizontal, vertical)}
    let actualNeighbours = Seq.except (Seq.singleton (x,y)) neighbours
    Seq.sumBy (fun mp-> mp.v) (Seq.where (fun mp -> (Seq.contains (mp.x, mp.y) actualNeighbours)) visited)

let moveUp (visited:seq<MemPos>) currentStep =
    let last = Seq.last visited
    match currentStep with
    | 0 -> { x=last.x + 1; y=last.y; v=(sumNeighbours visited (last.x + 1) last.y) }
    | _ -> {x=last.x; y=last.y + 1; v=(sumNeighbours visited last.x (last.y + 1))}

let moveLeft (visited:seq<MemPos>) =
    let last = Seq.last visited
    let newX = last.x - 1 
    { x=newX; y=last.y; v=(sumNeighbours visited newX last.y) }

let moveDown (visited:seq<MemPos>) =
    let last = Seq.last visited
    let newY = last.y - 1
    {x=last.x; y=newY; v=(sumNeighbours visited last.x newY)}

let moveRight (visited:seq<MemPos>) =
    let last = Seq.last visited
    let newX = last.x + 1 
    {x=newX; y=last.y; v=(sumNeighbours visited newX last.y)}

let move dir (visited:seq<MemPos>) currentStep =
    match dir with
    | 0 -> moveUp visited currentStep
    | 1 -> moveLeft visited
    | 2 -> moveDown visited
    | 3 -> moveRight visited
    | _ -> raise (ArgumentOutOfRangeException("invalid direction"))

let rec visitNext (visited:seq<MemPos>) endValue nthCircle currentStep currentDir =
    let next = move currentDir visited currentStep
    if next.v > endValue then
        next.v
    else
        let nowVisited = Seq.append visited (Seq.singleton next)
        if currentStep < (nthCircle * 2 - 1) then
            visitNext nowVisited endValue nthCircle (currentStep + 1) currentDir
        else
            if currentDir < 3 then
                visitNext nowVisited endValue nthCircle 0 (currentDir + 1)
            else
                visitNext nowVisited endValue (nthCircle + 1) 0 0


let day3b endValue =
    let startPos = {x=0; y=0; v=1}
    let visited = Seq.singleton startPos
    visitNext visited endValue 1 0 0

printfn "3b: %i" (day3b input)

