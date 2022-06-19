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

module type MARK_WITHOUT_CLEAR = sig
  type vertex
  val get : vertex -> int
  val set : vertex -> int -> unit
end

module Mark
    (G : Graph.Traverse.G)
    (M : MARK_WITHOUT_CLEAR with type vertex := G.V.t) =
struct

  let clear g i =
    G.iter_vertex (fun v -> if M.get v = i then M.set v 0) g

  let dfs g start i =
    clear g i;
    let rec visit v =
      if M.get v = 0 then begin
        M.set v i;
        G.iter_succ visit g v
      end
    in
    visit start

  let find_rev_path (type a) (f : _ -> a option) g start i =
    let exception Exit of a option in
    let rec visit p v =
      G.iter_succ
        (fun w ->
           if M.get w = 0 then
             let p' = w :: p in
             M.set w i;
             match f p' with
             | None -> visit p' w; M.set w 0
             | Some _ as x -> raise (Exit x))
        g v
    in
    try
      clear g i;
      M.set start i;
      visit [start] start;
      M.set start 0;
      None
    with Exit x -> x

  let find_path f g start i =
    match find_rev_path f g start i with
    | None -> None
    | Some p -> Some (List.rev p)

  let decompose f t =
    let ct, xs =
      G.fold_vertex (fun v (ct, xs) ->
        ct + 1, if f v then v :: xs else xs
      ) t (0, [])
    in
    let xs = Array.of_list xs in (* TODO sort? *)
    let rec f paths i =
      find_rev_path (fun p ->
        if i = Array.length xs - 1 then
          if List.(length (concat (p :: paths))) = ct then
            Some (p :: paths)
          else
            None
        else
          f (p :: paths) (i + 1)
      ) t xs.(i) (i + 1)
    in
    match f [] 0 with
    | None -> None
    | Some paths -> Some (List.map List.rev paths)

end

