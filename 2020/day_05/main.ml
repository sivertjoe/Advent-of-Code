open Printf

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

let biggest_seat =
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
let my_seat =
    seats |>
    List.sort Int.compare |>
    missing_seat

let () = printf "%d\n" biggest_seat
let () = printf "%d\n" my_seat