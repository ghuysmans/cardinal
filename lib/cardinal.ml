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

let connected {V.a; x; y} {V.a = a'; x = x'; y = y'} =
  a == a' && fst a.(y).(x) <= fst a.(y').(x')

let fold_succ f a ({V.x; y; _} as v) init =
  if a == v.a then
    let w, h = Array.length a.(0), Array.length a in
    let g dx dy acc =
      let v' = {V.a; x = x + dx; y = y + dy} in
      if v'.x >= 0 && v'.x < w && v'.y >= 0 && v'.y < h && connected v v' then
        f v' acc
      else
        acc
    in
    g 0 1 init |>
    g 1 0 |>
    g 0 (-1) |>
    g (-1) 0
  else
    init

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

let is_start {V.a; x; y} = fst a.(y).(x) = '0'

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

let valid_solution a paths =
  let open Mark in
  iter_vertex (fun v -> set v 0) a;
  paths |> List.for_all (function
    | [] -> true (* weird but ok *)
    | h :: t ->
      let test_and_set v = get v = 0 && (set v 1; true) in
      test_and_set h &&
      snd @@ List.fold_left (fun (v, ok) v' ->
        v',
        ok && connected v v' && test_and_set v'
      ) (h, true) t
  ) &&
  fold_vertex (fun v ok -> ok && get v = 1) a true

let dump ?(prefix="") a =
  let open ANSITerminal in
  let palette =
    Array.map (fun x -> Background x) [|
      Default;
      Red;
      Green;
      Yellow;
      Blue;
      Magenta;
      Cyan;
      White;
    |]
  in
  for y = 0 to Array.length a - 1 do
    printf [] "%s" prefix;
    for x = 0 to Array.length a.(0) - 1 do
      let ch, path = a.(y).(x) in
      printf [palette.(path)] "%c" ch
    done;
    printf [] "\n"
  done
