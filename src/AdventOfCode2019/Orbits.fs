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

let rec getOrbitalPath fullOrbitMap remainingOrbitMap orbitalPath node =
    match remainingOrbitMap with
    | [] -> orbitalPath
    | h::t ->
        if (snd h) = node then
            getOrbitalPath fullOrbitMap fullOrbitMap (orbitalPath@[fst h]) (fst h)
        else
            getOrbitalPath fullOrbitMap t orbitalPath node

let getOrbitalPath' orbitMap =
    getOrbitalPath orbitMap orbitMap List<string>.Empty

let countAllOrbits data =
    let orbitMap = parseMapData data

    findAllNodes orbitMap
    |> List.map (getOrbitalPath' orbitMap)
    |> List.sumBy (fun x -> x.Length)

let calculateShortestHop data =
    let orbitMap = parseMapData data
    let youPath = getOrbitalPath' orbitMap "YOU"
    let santaPath = getOrbitalPath' orbitMap "SAN"

    youPath@santaPath
    |> List.countBy id
    |> List.filter (fun x -> (snd x) = 1)
    |> List.length
