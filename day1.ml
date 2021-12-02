let count_increments =
  let rec aux c = function
    | x :: y :: tl ->
      let c' = if x < y then c + 1 else c in
      aux c' (y :: tl)
    | _ -> c
  in
  aux 0
;;

let windows =
  let rec aux ws = function
    | x :: y :: z :: tl ->
      let w = x + y + z in
      aux (w :: ws) (y :: z :: tl)
    | _ -> List.rev ws
  in
  aux []
;;

let () =
  let measures = Aoc.read () |> List.map int_of_string in
  Printf.printf "part1: %d\n" (measures |> count_increments);
  Printf.printf "part2: %d\n" (measures |> windows |> count_increments)
;;
