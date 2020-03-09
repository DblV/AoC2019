module FuelManagementSystem

open FSharp.Data

type Direction = | U | D | R | L
type Plane = Horizontal | Vertical

type Instruction = {
    Direction: Direction;
    Distance: int
}

type Path = {
    plane: Plane;
    minX: int;
    minY: int;
    maxX: int;
    maxY: int;
}

let min x y =
    if x < y then x 
    else y

let max x y = 
    if x > y then x
    else y

let abs x =
    if x < 0 then x * -1
    else x

let parseInstruction (instructionString:string) =
    let direction = instructionString.[0]
    let distance = instructionString.[1..(instructionString.Length-1)]
    match direction with
    | 'U' -> { Direction = U; Distance = int distance }
    | 'D' -> { Direction = D; Distance = int distance }
    | 'R' -> { Direction = R; Distance = int distance }
    | 'L' -> { Direction = L; Distance = int distance }
    | _ -> invalidArg "direction" (sprintf "Invalid arg %c" direction)

let loadInstructionsFromString inputString =
    let csvInput = CsvFile.Parse(inputString)
    match csvInput.Headers with
    | Some x -> x |> Array.toList |> List.map parseInstruction
    | None -> List<Instruction>.Empty

let rec buildPathList instructions currentX currentY pathList =
    match instructions with
    | [] ->
        pathList
    | h::t -> 
        match h.Direction with
        | U ->  
            let newPath = { plane = Vertical; minX = currentX; maxX = currentX; minY = min currentY (currentY + h.Distance); maxY = max currentY (currentY + h.Distance) }
            buildPathList t currentX (currentY + h.Distance) pathList@[newPath]
        | D ->
            let newPath = { plane = Vertical; minX = currentX; maxX = currentX; minY = min currentY (currentY - h.Distance); maxY = max currentY (currentY - h.Distance) }
            buildPathList t currentX (currentY - h.Distance) pathList@[newPath]
        | R ->
            let newPath = { plane = Horizontal; minX = min currentX (currentX + h.Distance); maxX = max currentX (currentX + h.Distance); minY = currentY; maxY = currentY }
            buildPathList t (currentX + h.Distance) currentY pathList@[newPath]
        | L ->
            let newPath = { plane = Horizontal; minX = min currentX (currentX - h.Distance); maxX = max currentX (currentX - h.Distance); minY = currentY; maxY = currentY }
            buildPathList t (currentX - h.Distance) currentY pathList@[newPath]

let findIntersectionDistance (path1:Path) (path2:Path) =
    match (path1.plane, path2.plane) with
    | (Horizontal,Vertical) ->
        if path2.minX >= path1.minX && path2.maxX <= path1.maxX
            && path1.minY >= path2.minY && path1.maxY <= path2.maxY then
            // Paths intersect; distance is abs(x) of Path 1 and abs(y) of Path 2
            abs path2.minX + abs path1.minY
        else
            0
    | (Vertical,Horizontal) ->
        if path1.minX >= path2.minX && path1.maxX <= path2.maxX
            && path2.minY >= path1.minY && path2.maxY <= path1.maxY then
            // Paths intersect; distance is abs(x) of Path 2 and abs(y) of Path 1
            abs path1.minX + abs path2.minY
        else
            0
    | (_,_) -> 0

let findIntersections (possibleIntersections:(Path) list) (path:(Path)) =
    possibleIntersections
    |> List.map (findIntersectionDistance path)

let calculateNearestIntersection instructions =
    let wire1Paths = buildPathList (loadInstructionsFromString (instructions |> Seq.head)) 0 0 List<Path>.Empty
    let wire2Paths = buildPathList (loadInstructionsFromString (instructions |> Seq.last)) 0 0 List<Path>.Empty

    wire1Paths
    |> List.collect (findIntersections wire2Paths)
    |> List.filter (fun x -> x > 0)
    |> List.min
