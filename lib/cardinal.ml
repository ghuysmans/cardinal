let is_directed = true

type t = (char * int) array array

module V = struct
  type nonrec t = {
    a: t;
    x: int;
    y: int;
  }

  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

let fold_vertex f a init =
  let acc = ref init in
  for y = 0 to Array.length a - 1 do
    for x = 0 to Array.length a.(0) - 1 do
      match fst a.(y).(x) with
      | ' ' -> ()
      | _ -> acc := f {V.a; x; y} !acc
    done
  done;
  !acc

let iter_vertex f a = fold_vertex (fun v () -> f v) a ()
let nb_vertex a = fold_vertex (fun _ -> succ) a 0

let fold_succ f a {V.x; y; _} init =
  let w, h = Array.length a.(0), Array.length a in
  let g dx dy acc =
    let x', y' = x + dx, y + dy in
    if x' >= 0 && x' < w && y' >= 0 && y' < h && fst a.(y).(x) <= fst a.(y').(x') then
      f {V.a; x = x'; y = y'} acc
    else
      acc
  in
  g 0 1 init |>
  g 1 0 |>
  g 0 (-1) |>
  g (-1) 0

let out_degree a v = fold_succ (fun _ -> succ) a v 0
let iter_succ f a v = fold_succ (fun v () -> f v) a v ()

module Mark = struct
  let get {V.a; x; y} = snd a.(y).(x)
  let set {V.a; x; y} m = a.(y).(x) <- fst a.(y).(x), m
end

let of_rows l =
  l |> List.map (fun x -> Array.init (String.length x) (fun i ->
    begin match x.[i] with
      | 'X' -> '0'
      | d -> d
    end,
    0
  )) |>
  Array.of_list

module E = struct
  type t = V.t * V.t
  let src = fst
  let dst = snd
end

let iter_edges_e f a =
  iter_vertex (fun v -> iter_succ (fun v' -> f (v, v')) a v) a

let graph_attributes _ = []

let default_vertex_attributes _ = []

let vertex_name {V.x; y; _} = Printf.sprintf "x%dy%d" x y

let vertex_attributes {V.a; x; y} =
  let d = fst a.(y).(x) in
  let l = [`Label (Printf.sprintf "%c@%d,%d" d x y)] in
  if d = '0' then
    `Fillcolor 0xFF0000 :: `Style `Filled :: l
  else
    l

let get_subgraph _ = None

let default_edge_attributes _ = []

let edge_attributes _ = []
