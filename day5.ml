type point = int * int
type segment = point * point

let segment_re = Str.regexp "^\\([0-9]+\\),\\([0-9]+\\) -> \\([0-9]+\\),\\([0-9]+\\)$"

let parse input : segment =
  if Str.string_match segment_re input 0
  then (
    let x1 = Str.matched_group 1 input |> int_of_string in
    let y1 = Str.matched_group 2 input |> int_of_string in
    let x2 = Str.matched_group 3 input |> int_of_string in
    let y2 = Str.matched_group 4 input |> int_of_string in
    (x1, y1), (x2, y2))
  else assert false
;;

let is_horizontal_or_vertical ((x1, y1), (x2, y2)) = x1 = x2 || y1 = y2

let to_dir_len ((x1, y1), (x2, y2)) =
  match x2 - x1, y2 - y1 with
  | 0, dy when dy > 0 -> 0, 1, dy
  | 0, dy when dy < 0 -> 0, -1, -dy
  | dx, 0 when dx > 0 -> 1, 0, dx
  | dx, 0 when dx < 0 -> -1, 0, -dx
  | dx, dy when dx > 0 && dy > 0 -> 1, 1, dx
  | dx, dy when dx > 0 && dy < 0 -> 1, -1, dx
  | dx, dy when dx < 0 && dy > 0 -> -1, 1, dy
  | dx, dy when dx < 0 && dy < 0 -> -1, -1, -dx
  | _ -> assert false
;;

let make_map segments =
  let map = Array.make_matrix 1000 1000 0 in
  let add (((x, y), _) as seg) =
    let dx, dy, len = to_dir_len seg in
    for i = 0 to len do
      map.(x + (dx * i)).(y + (dy * i)) <- map.(x + (dx * i)).(y + (dy * i)) + 1
    done
  in
  Seq.iter add segments;
  map
;;

let count pred map =
  let count = Array.fold_left (fun acc x -> if pred x then acc + 1 else acc) 0 in
  let sum_ys = Array.map count map in
  Array.fold_left ( + ) 0 sum_ys
;;

(* let print_segment ((x1, y1), (x2, y2)) = Printf.printf "[(%i %i) (%i %i)]\n" x1 y1 x2 y2 *)

let () =
  let input = Aoc.read () |> Seq.map parse in
  (* uncomment for part1 answer *)
  (* let input = Seq.filter is_horizontal_or_vertical input in *)
  let count_min_2 = make_map input |> count (fun x -> x >= 2) in
  Printf.printf "%i points in min 2 segments\n" count_min_2
;;
