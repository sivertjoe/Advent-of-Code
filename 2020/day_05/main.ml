open Printf
open Unix
open Float

let f res i x =
    res + (Int.of_float (2. ** Float.of_int (9 - i))  *  x)
    
let to_digit str = 
    String.to_seq str |>
    Seq.map (fun x -> if x == 'R' || x == 'B' then 1 else 0) |>
    Seq.fold_lefti f 0
  
let lines file =
    let contents = In_channel.with_open_bin file In_channel.input_all in
    String.split_on_char '\n' contents

let seats =
   (lines "input") |>
   List.map to_digit

let biggest_seat unit =
   seats |>
   List.fold_left Stdlib.max 0

let rec missing_seat = function
    | [] -> failwith "unreachable"
    | _ :: [] -> failwith "unreachable"
    | n1 :: n2 :: xs ->
        if n1 + 2 == n2 then
            n2 - 1
        else
            missing_seat (n2 :: xs)

let my_seat unit =
    seats |>
    List.sort Int.compare |>
    missing_seat

let part_one unit = 
    biggest_seat ()

let part_two unit =
    my_seat ()

let time unit =
    Unix.gettimeofday () *. 1000.0

let diff t0 t1 =
    Float.to_int (Float.round (t1 -. t0))

let timer f  =
    let start = time () in
        let res = f () in
            let stop = time () in
                printf "(%dms)\t%d\n" (diff start stop) res

let () = timer part_one
let () = timer part_two
