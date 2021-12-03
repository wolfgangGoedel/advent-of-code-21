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

let ones n = (1 lsl n) - 1

let collect =
  let collect_bits x bit = (x lsl 1) lor bit in
  List.fold_left collect_bits 0
;;

let compute_part1 input =
  let length = List.length input in
  let columns = transpose input in
  let width = List.length columns in
  let dominants =
    let sum = List.fold_left ( + ) 0 in
    let dom count = if count > length - count then 1 else 0 in
    columns |> List.map (fun col -> col |> sum |> dom)
  in
  let gamma = collect dominants in
  let epsilon = gamma lxor ones width in
  gamma, epsilon
;;

(* part2 *)

let filter_with xs keep ys =
  List.map2 (fun x y -> x, y) xs ys
  |> List.filter_map (fun (x, y) -> if keep x then Some y else None)
;;

let reduce_input choose input =
  let columns = transpose input in
  let filter col = choose col |> Int.equal |> filter_with col in
  let rec aux = function
    | [], _ -> assert false
    | [ n ], _ -> n
    | ns, [] -> assert false
    | ns, col :: cols ->
      let filter xs = filter col xs in
      let ns' = filter ns in
      let cols' = List.map filter cols in
      aux (ns', cols')
  in
  aux (input, columns)
;;

let choose choose_1 bits =
  let length = List.length bits in
  let sum = List.fold_left ( + ) 0 bits in
  if choose_1 sum length then 1 else 0
;;

let choose_o2 = choose (fun sum l -> sum >= l - sum)
let choose_co2 = choose (fun sum l -> sum < l - sum)

let () =
  let input = Aoc.read () |> Seq.map parse |> List.of_seq in
  let gamma, epsilon = compute_part1 input in
  Printf.printf "part1: gamma=%d epsilon=%d result=%d\n" gamma epsilon (gamma * epsilon);
  let o2 = reduce_input choose_o2 input |> collect in
  let co2 = reduce_input choose_co2 input |> collect in
  Printf.printf "part2: o2=%d co2=%d result=%d\n" o2 co2 (o2 * co2)
;;
