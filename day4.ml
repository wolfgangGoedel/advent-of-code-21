let parse_numbers input = String.split_on_char ',' input |> List.map int_of_string

let parse_boards input =
  let rec aux boards = function
    | [] -> boards
    | "" :: tl -> aux ([] :: boards) tl
    | line :: tl ->
      let line =
        String.split_on_char ' ' line
        |> List.filter_map (function
               | "" -> None
               | s -> Some (Some (int_of_string s)))
      in
      let board = line :: List.hd boards in
      aux (board :: List.tl boards) tl
  in
  aux [] input
;;

let mark_number number =
  List.map @@ List.map (fun n -> if n = Some number then None else n)
;;

let rec winning_boards = function
  | [] -> []
  | board :: tl ->
    let rows_and_cols = board @ Aoc.transpose board in
    if List.exists (List.for_all Option.is_none) rows_and_cols
    then board :: winning_boards tl
    else winning_boards tl
;;

let rec play_bingo boards winnings = function
  | [] -> winnings
  | n :: ns ->
    let boards = List.map (mark_number n) boards in
    let winning = winning_boards boards in
    let remaining = List.filter (fun b -> not (List.memq b winning)) boards in
    let winning_with_number = List.map (fun b -> n, b) winning in
    play_bingo remaining (winnings @ winning_with_number) ns
;;

let count_points =
  let sum = List.fold_left ( + ) 0 in
  fun board -> board |> List.map (List.filter_map Fun.id) |> List.map sum |> sum
;;

let () =
  let input = Aoc.read () |> List.of_seq in
  let numbers, boards =
    match input with
    | numbers :: boards ->
      let numbers = parse_numbers numbers in
      let boards = parse_boards boards in
      numbers, boards
    | _ -> assert false
  in
  let winnings = play_bingo boards [] numbers in
  let _ =
    let number, board = List.hd winnings in
    Printf.printf
      "part1: first board won on number %i with %i points\n"
      number
      (number * count_points board)
  in
  let _ =
    let number, board = List.hd (List.rev winnings) in
    Printf.printf
      "part2: last board won on number %i with %i points\n"
      number
      (number * count_points board)
  in
  ()
;;
