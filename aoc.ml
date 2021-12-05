let read () =
  let read_line () =
    try Some (read_line (), ()) with
    | End_of_file -> None
  in
  Seq.unfold read_line ()
;;

let rec transpose = function
  | [] -> []
  | [] :: ys -> transpose ys
  | ys -> List.map List.hd ys :: transpose (List.map List.tl ys)
;;
