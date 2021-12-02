let read () =
  let read_line () =
    try Some (read_line (), ()) with
    | End_of_file -> None
  in
  Seq.unfold read_line ()
;;
