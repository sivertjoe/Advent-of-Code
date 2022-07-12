open System.Linq

let input = [1; 0; 16; 5; 17; 4]
        
let findNumber n =
    let temp = input
                |> List.mapi (fun i x -> (x, i))
                |> List.take (input.Length - 1)

    let map = temp.ToDictionary(fst, snd)

    let mutable i = input.Length
    let mutable prev = input.[input.Length - 1]

    while i < n do
        let next = match map.TryGetValue prev with
                    | true, v -> i  - v - 1
                    | _ -> 0
        map.[prev] <- i - 1
        prev <- next
        i <- i + 1
       
    prev
    
let partOne =
    findNumber 2020

let partTwo =
    findNumber 30000000

printfn "%i" partOne
printfn "%i" partTwo
