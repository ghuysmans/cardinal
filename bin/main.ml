module GV = Graph.Graphviz.Dot (Cardinal)
module H = Hamilton.Bitfield (Cardinal) (Cardinal.Mark)

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

let () =
  let t = read_lines () |> Cardinal.of_rows in
  match Sys.argv with
  | [| _ |] ->
    GV.output_graph stdout t;
    print_newline ()
  | [| _; "-r" |] ->
    let open Cardinal in
    begin match H.decompose is_start t with
    | None -> print_endline "not found"
    | Some paths ->
      if valid_solution t paths then
        List.iter (fun p ->
          Printf.printf "path:";
          List.iter (fun x -> Printf.printf " %s" (vertex_name x)) p;
          Printf.printf "\n"
        ) paths
      else
        print_endline "invalid solution"
    end
  | _ ->
    Printf.eprintf "usage: %s [-r]\n" Sys.argv.(0);
    exit 1
