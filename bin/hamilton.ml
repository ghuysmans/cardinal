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

module Mark
    (G : Graph.Traverse.G)
    (M : Graph.Sig.MARK with type graph := G.t and type vertex := G.V.t) =
struct

  let dfs g start =
    M.clear g;
    let rec visit v =
      if M.get v = 0 then begin
        M.set v 1;
        G.iter_succ visit g v
      end
    in
    visit start

  let find_rev_path (type a) (f : _ -> a option) g start =
    let exception Exit of a option in
    M.clear g;
    let rec visit p v =
      M.set v 1;
      G.iter_succ
        (fun w ->
           if M.get w = 0 then
             let p' = w :: p in
             match f p' with
             | None -> visit p' w
             | Some _ as x -> raise (Exit x))
        g v;
      M.set v 0
    in
    try visit [start] start; None
    with Exit x -> x

  let find_path f g start =
    match find_rev_path f g start with
    | None -> None
    | Some p -> Some (List.rev p)

end

module type MARK_WITHOUT_CLEAR = sig
  type vertex
  val get : vertex -> int
  val set : vertex -> int -> unit
end

module Bitfield
    (G : Graph.Traverse.G)
    (M : MARK_WITHOUT_CLEAR with type vertex := G.V.t) =
struct

  module F (P : sig val i : int end) = struct
    let get = M.get
    let set w x = M.set w (M.get w land lnot (1 lsl P.i) lor (x lsl P.i))
    let clear g = G.iter_vertex (fun w -> set w 0) g
  end

  let decompose f t =
    let ct, xs =
      G.fold_vertex (fun v (ct, xs) ->
        ct + 1, if f v then v :: xs else xs
      ) t (0, [])
    in
    let xs = Array.of_list xs in (* TODO sort? *)
    let rec f paths i =
      let module B = Mark (G) (F (struct let i = i end)) in
      B.find_rev_path (fun p ->
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
      ) t xs.(i)
    in
    match f [] 0 with
    | None -> None
    | Some paths -> Some (List.map List.rev paths)

end

