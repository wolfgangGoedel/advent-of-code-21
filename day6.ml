let read_and_parse () = read_line () |> String.split_on_char ',' |> List.map int_of_string

let () =
  let days = 256 in
  let ages = Array.make 9 0 in
  let _ = read_and_parse () |> List.iter (fun age -> ages.(age) <- ages.(age) + 1) in
  for day = 1 to days do
    let zeros = ages.(0) in
    for age = 1 to 8 do
      ages.(age - 1) <- ages.(age)
    done;
    ages.(8) <- zeros;
    ages.(6) <- ages.(6) + zeros
  done;
  let nb_fish = Array.fold_left ( + ) 0 ages in
  Printf.printf "there are %i fish after %i days\n" nb_fish days
;;
