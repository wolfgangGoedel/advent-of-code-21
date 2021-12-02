type cmd =
  | Forward of int
  | Up of int
  | Down of int

let parse cmd =
  match String.split_on_char ' ' cmd with
  | [ "forward"; x ] -> Forward (int_of_string x)
  | [ "up"; x ] -> Up (int_of_string x)
  | [ "down"; x ] -> Down (int_of_string x)
  | _ -> assert false
;;

let exec =
  let update_position (pos, depth) = function
    | Forward n -> pos + n, depth
    | Up n -> pos, depth - n
    | Down n -> pos, depth + n
  in
  List.fold_left update_position (0, 0)
;;

type substate =
  { aim : int
  ; pos : int
  ; depth : int
  }

let exec' =
  let update_position ({ aim; pos; depth } as state) = function
    | Forward n -> { state with pos = pos + n; depth = depth + (aim * n) }
    | Up n -> { state with aim = aim - n }
    | Down n -> { state with aim = aim + n }
  in
  List.fold_left update_position { aim = 0; pos = 0; depth = 0 }
;;

let () =
  let cmds = Aoc.read () |> Seq.map parse |> List.of_seq in
  let pos, depth = cmds |> exec in
  Printf.printf "part1: (%d,%d) -> %d\n" pos depth (pos * depth);
  let { pos; depth } = cmds |> exec' in
  Printf.printf "part2: (%d,%d) -> %d\n" pos depth (pos * depth)
;;
