(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

module Mark(G : Graph.Coloring.GM) = struct

  let set i w x = G.Mark.set w (G.Mark.get w land lnot (1 lsl i) lor x lsl i)
  let clear g i = G.iter_vertex (fun w -> set i w 0) g

  let dfs g i start =
    clear g i;
    let rec visit v =
      if G.Mark.get v = 0 then begin
        set i v 1;
        G.iter_succ visit g v
      end
    in
    visit start

  let find_path (type a) (f : _ -> a option) g i start =
    let exception Exit of a option in
    clear g i;
    let rec visit p v =
      set i v 1;
      G.iter_succ
        (fun w ->
           if G.Mark.get w = 0 then
             let p' = w :: p in
             match f p' with
             | None -> visit p' w
             | Some _ as x -> raise (Exit x))
        g v;
      set i v 0
    in
    try visit [start] start; None
    with Exit x -> x

  let decompose f t =
    let ct = G.nb_vertex t in
    let xs =
      G.fold_vertex (fun v acc -> if f v then v :: acc else acc) t [] |>
      Array.of_list
      (* TODO sort? *)
    in
    let rec f paths i =
      find_path (fun p ->
        if i = Array.length xs - 1 then
          let accessible =
            (* TODO compute it earlier *)
            (* FIXME why not?
            fold_vertex (fun v acc ->
              acc + if Mark.get v = 0 then 0 else 1
            ) t 0
            *)
            List.concat (p :: paths) |>
            List.length
          in
          if accessible = ct then
            Some (p :: paths)
          else
            None
        else
          f (p :: paths) (i + 1)
      ) t i xs.(i)
    in
    f [] 0

end

