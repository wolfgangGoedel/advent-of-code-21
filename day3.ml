let parse bin =
  bin
  |> String.to_seq
  |> Seq.map (function
         | '0' -> 0
         | '1' -> 1
         | _ -> assert false)
  |> List.of_seq
;;

let rec transpose = function
  | [] -> []
  | [] :: ys -> transpose ys
  | ys -> List.map List.hd ys :: transpose (List.map List.tl ys)
;;

let () =
  let input = Aoc.read () |> Seq.map parse |> List.of_seq in
  let length = List.length input in
  let columns = transpose input in
  let width = List.length columns in
  let dominants =
    let sum = List.fold_left ( + ) 0 in
    let dom count = if count > length / 2 then 1 else 0 in
    columns |> List.map (fun col -> col |> sum |> dom)
  in
  let gamma =
    let collect_bits x bit = (x lsl 1) lor bit in
    List.fold_left collect_bits 0 dominants
  in
  let epsilon =
    let ones l = (1 lsl width) - 1 in
    gamma lxor ones width
  in
  Printf.printf "part1: gamma=%d epsilon=%d result=%d\n" gamma epsilon (gamma * epsilon)
;;
