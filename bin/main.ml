module GV = Graph.Graphviz.Dot (Cardinal)
module H = Hamilton.Mark (Cardinal) (Cardinal.Mark)
open Cardinal

let input_lines ch =
  let rec f acc =
    match
      try
        let l = input_line ch in
        Some l
      with End_of_file ->
        None
    with
    | Some l -> f (l :: acc)
    | None -> List.rev acc
  in
  f []


let () =
  match Sys.argv with
  | [| _; "-g" |] ->
    input_lines stdin |> of_rows |> GV.output_graph stdout;
    print_newline ();
    exit 0
  | [| _; "-h" |] | [| _; "--help" |] ->
    Printf.printf "usage: %s -g | puzzle...\n" Sys.argv.(0);
    exit 1
  | _ ->
    let ct = Array.length Sys.argv - 1 in
    let ok = ref true in
    Printf.printf "1..%d\n" ct;
    for i = 1 to ct do
      let inp = Sys.argv.(i) in
      let report good s =
        Printf.printf "%sok - %s: %s\n" (if good then "" else "not ") inp s;
        ok := !ok && good
      in
      try
        let ch =
          match inp with
          | "-" -> stdin
          | fn -> open_in fn
        in
        let t = input_lines ch |> of_rows in
        begin match H.decompose is_start t with
        | None -> report false "no solution found"
        | Some paths ->
          dump ~prefix:"#" t;
          if valid_solution t paths then
            List.map (fun p -> List.map vertex_name p |> String.concat " ") paths |>
            String.concat ", " |>
            report true
          else
            report false "invalid solution"
        end
      with e ->
        report false (Printexc.to_string e)
    done;
    if not !ok then exit 2
