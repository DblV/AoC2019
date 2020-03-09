module FuelManagementSystem

open FSharp.Data

type Direction = | U | D | R | L

type Instruction = {
    Direction: Direction;
    Distance: int
}

type Coordinate = {
    x: int;
    y: int
}

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

let rec buildCoordinateList instructions currentCoordinates coordinateList =
    match instructions with
    | [] ->
        coordinateList
    | h::t -> 
        match h.Direction with
        | U ->  
            let newCoordinates = { x = currentCoordinates.x; y = currentCoordinates.y + h.Distance }
            buildCoordinateList t newCoordinates coordinateList@[(currentCoordinates,newCoordinates)]
        | D ->
            let newCoordinates = { x = currentCoordinates.x; y = currentCoordinates.y - h.Distance }
            buildCoordinateList t newCoordinates coordinateList@[(currentCoordinates,newCoordinates)]
        | R ->
            let newCoordinates = { x = currentCoordinates.x + h.Distance; y = currentCoordinates.y }
            buildCoordinateList t newCoordinates coordinateList@[(currentCoordinates,newCoordinates)]
        | L ->
            let newCoordinates = { x = currentCoordinates.x - h.Distance; y = currentCoordinates.y }
            buildCoordinateList t newCoordinates coordinateList@[(currentCoordinates,newCoordinates)]

let min x y =
    if x < y then x 
    else y

let max x y = 
    if x > y then x
    else y

let abs x =
    if x < 0 then x * -1
    else x

let findIntersectionDistance path1 path2 =
    let path1CoordA, path1CoordB = path1
    let path2CoordC, path2CoordD = path2

    if path1CoordA.x = path1CoordB.x then
        // Path 1 is vertical
        if path2CoordC.x = path2CoordD.x then
            // Both vertical lines - no intersection
            0
        else
            let minimumX = min path2CoordC.x path2CoordD.x
            let maximumX = max path2CoordC.x path2CoordD.x
            let minimumY = min path1CoordA.y path1CoordB.y
            let maximumY = max path1CoordA.y path1CoordB.y
            if path1CoordA.x >= minimumX && path1CoordA.x <= maximumX
                && path2CoordC.y >= minimumY && path2CoordC.y <= maximumY then
                // Paths intersect
                // Intersect is abs(x) of Path 2 and abs(y) of Path 1
                printfn "---------"
                printfn "Comparing Path 1 %A and Path 2 %A" path1 path2
                printfn "minx %i maxx %i miny %i maxy %i" minimumX maximumX minimumY maximumY
                printfn "Paths intersect at %i %i" path1CoordA.x path2CoordC.y
                abs path1CoordA.x + abs path2CoordC.y
            else
                0
    else
        // Path 1 is horizontal
        if path2CoordC.y = path2CoordD.y then
            // Both horizontal lines - no intersection
            0
        else
            let minimumX = min path1CoordA.x path1CoordB.x
            let maximumX = max path1CoordA.x path1CoordB.x
            let minimumY = min path2CoordC.y path2CoordD.y
            let maximumY = max path2CoordC.y path2CoordD.y
            if path2CoordC.x >= minimumX && path2CoordC.x <= maximumX
                && path1CoordA.y >= minimumY && path1CoordA.y <= maximumY then
                // Paths intersect
                // Intersect is abs(x) of Path 1 and abs(y) of Path 2
                printfn "---------"
                printfn "Comparing Path 1 %A and Path 2 %A" path1 path2
                printfn "minx %i maxx %i miny %i maxy %i" minimumX maximumX minimumY maximumY
                printfn "Paths intersect at %i %i" path2CoordC.x path1CoordA.y
                abs path2CoordC.x + abs path1CoordA.y
            else
                0

let findIntersections (possibleIntersections:(Coordinate*Coordinate) list) (path:(Coordinate*Coordinate)) =
    possibleIntersections
    |> List.map (findIntersectionDistance path)

let calculateNearestIntersection wire1Path wire2Path =
    let wire1Coordinates = buildCoordinateList (loadInstructionsFromString wire1Path) {x=0;y=0} List<Coordinate*Coordinate>.Empty
    let wire2Coordinates = buildCoordinateList (loadInstructionsFromString wire2Path) {x=0;y=0} List<Coordinate*Coordinate>.Empty

    wire1Coordinates
    |> List.collect (findIntersections wire2Coordinates)
    |> List.filter (fun x -> x > 0)
    |> List.min
