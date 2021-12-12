module S = Set.Make (Char)

let parse_combinations cs =
  cs |> String.split_on_char ' ' |> List.map String.to_seq |> List.map S.of_seq
;;

let parse input =
  let input =
    String.split_on_char '|' input |> List.map String.trim |> List.map parse_combinations
  in
  match input with
  | [ entry; output ] -> entry, output
  | _ -> assert false
;;

let digit_of_unique c =
  match S.cardinal c with
  | 2 -> Some 1
  | 3 -> Some 7
  | 4 -> Some 4
  | 7 -> Some 8
  | _ -> None
;;

module M = Map.Make (S)

let classify cs =
  let one = List.find (fun c -> S.cardinal c = 2) cs in
  let four = List.find (fun c -> S.cardinal c = 4) cs in
  let digit c =
    match S.cardinal c with
    | 2 -> 1
    | 3 -> 7
    | 4 -> 4
    | 5 when S.subset one c -> 3
    | 5 when S.cardinal (S.inter four c) = 2 -> 2
    | 5 -> 5
    | 6 when S.subset four c -> 9
    | 6 when S.subset one c -> 0
    | 6 -> 6
    | 7 -> 8
    | _ -> assert false
  in
  cs |> List.fold_left (fun m c -> M.add c (digit c) m) M.empty
;;

let decode (cs, out) =
  let cls = classify cs in
  let digit c = M.find c cls in
  let collect out c = (out * 10) + digit c in
  List.fold_left collect 0 out
;;

let () =
  let input = Aoc.read () |> Seq.map parse |> List.of_seq in
  let unique_combinations =
    input |> List.map snd |> List.map (List.filter_map digit_of_unique) |> List.flatten
  in
  Printf.printf "part1: %i times (1,4,7,8)\n" (List.length unique_combinations);
  let sum_decoded = input |> List.map decode |> List.fold_left ( + ) 0 in
  Printf.printf "part2: %i\n" sum_decoded
;;
