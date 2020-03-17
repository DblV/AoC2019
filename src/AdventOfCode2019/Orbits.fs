module Orbits

let parseOrbitData (orbitDataStr:string) =
    let separatorIndex = orbitDataStr.IndexOf ')'
    (orbitDataStr.[0..separatorIndex-1] |> string, orbitDataStr.[separatorIndex+1..orbitDataStr.Length-1] |> string)

let parseMapData data = 
    data |> Seq.toList |> List.map parseOrbitData

let findAllNodes orbitMap =
    orbitMap
    |> List.map (fun x -> [fst x;snd x])
    |> List.collect id
    |> List.groupBy id
    |> List.map (fun y -> (fst y))

let rec countOrbitsForNode fullOrbitMap remainingOrbitMap count node =
    match remainingOrbitMap with
    | [] -> count
    | h::t ->
        if (snd h) = node then
            countOrbitsForNode fullOrbitMap fullOrbitMap (count+1) (fst h)
        else
            countOrbitsForNode fullOrbitMap t count node

let countAllOrbits data =
    let orbitMap = parseMapData data

    findAllNodes orbitMap
    |> List.sumBy (countOrbitsForNode orbitMap orbitMap 0)
