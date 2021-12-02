let rec read () =
  try
    let line = read_line () in
    line :: read ()
  with
  | End_of_file -> []
;;
