let read_and_parse () = read_line () |> String.split_on_char ',' |> List.map int_of_string

let fuel pos coords =
  coords |> List.map (fun coord -> abs (coord - pos)) |> List.fold_left ( + ) 0
;;

let sum_1_to_n n = n * (n + 1) / 2

let fuel' pos coords =
  coords
  |> List.map (fun coord -> abs (coord - pos) |> sum_1_to_n)
  |> List.fold_left ( + ) 0
;;

let () =
  let input = read_and_parse () in
  let rec aux pos min =
    let cons = fuel' pos input in
    if cons > min then pos - 1, min else aux (pos + 1) cons
  in
  let pos, cons = aux 0 Int.max_int in
  Printf.printf "@%i: %i\n" pos cons
;;
