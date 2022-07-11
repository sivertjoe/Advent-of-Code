let input = [1; 0; 16; 5; 17; 4]
// let input = [3; 1; 2]


type Inner =
| Once of int
| More of int * int

let findNumber n =
    let getMap =
        input
        |> List.mapi (fun i x -> (x, Once (i + 1)))
        |> Map.ofSeq

    let nextMap (map: Map<int, Inner>) i next =
        let v = match  map.TryFind next with
                | None -> Once i
                | Some inner -> match inner with
                                | Once n -> More (i, n)
                                | More (n2, _) -> More (i, n2)
        map.Add (next, v)

    let rec findNumber n i (map: Map<int, Inner>) prev =
        if i >= n + 1 then
            prev
        else
            let next = match map.TryFind prev with
                        | None -> 0
                        | Some seen -> match seen with
                                        | Once _ -> 0
                                        | More (n2, n1) -> n2 - n1
            findNumber n (i + 1) (nextMap map i next) next

    findNumber n (input.Length + 1) getMap (input.[input.Length - 1])

let partOne =
    findNumber 2020

let partTwo =
    findNumber 30000000

printfn "%i" partOne
printfn "%i" partTwo
