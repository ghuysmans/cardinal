module GV = Graph.Graphviz.Dot (Cardinal)

let read_lines () =
  let rec f acc =
    match
      try
        let l = read_line () in
        Some l
      with End_of_file ->
        None
    with
    | Some l -> f (l :: acc)
    | None -> List.rev acc
  in
  f []

module C = Graph.Path.Check (Cardinal) (* TODO check its implementation *)

let () =
  let t = read_lines () |> Cardinal.of_rows in
  GV.output_graph stdout t;
  print_newline ()
