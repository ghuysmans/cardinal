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

module T = Graph.Traverse.Dfs (Cardinal)
module S = Set.Make (Cardinal.V)

let () =
  let t = read_lines () |> Cardinal.of_rows in
  match Sys.argv with
  | [| _ |] ->
    GV.output_graph stdout t;
    print_newline ()
  | [| _; "-r" |] ->
    let open Cardinal in
    let forest =
      fold_vertex (fun v l ->
        if is_start v then
          (v, T.fold_component S.add S.empty t v) :: l
        else
          l
      ) t []
    in
    iter_vertex (fun v -> Mark.set v 0) t;
    forest |> List.iter (fun (v, s) ->
      Printf.printf "R %s:" (vertex_name v);
      s |> S.iter (fun v' -> Mark.set v' 1; Printf.printf " %s" (vertex_name v'));
      Printf.printf "\n";
      let fixed =
        List.fold_left (fun acc (v', s') ->
          if v = v' then
            acc
          else
            S.diff acc s'
        ) s forest
      in
      Printf.printf "U %s:" (vertex_name v);
      fixed |> S.iter (fun v' -> Printf.printf " %s" (vertex_name v'));
      Printf.printf "\n"
    );
    t |> iter_vertex (fun v ->
      if Mark.get v = 0 then
        Printf.printf "unreachable: %s\n" (vertex_name v)
    )
  | _ ->
    Printf.eprintf "usage: %s [-r]\n" Sys.argv.(0);
    exit 1
