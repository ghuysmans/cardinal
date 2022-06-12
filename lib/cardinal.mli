include Graph.Coloring.GM
val of_rows : string list -> t

module E : sig
  type t
  val src : t -> V.t
  val dst : t -> V.t
end

open Graph.Graphviz.DotAttributes
val iter_edges_e : (E.t -> unit) -> t -> unit
val graph_attributes : t -> graph list
val default_vertex_attributes : t -> vertex list
val vertex_name : V.t -> string
val vertex_attributes : V.t -> vertex list
val get_subgraph : V.t -> subgraph option
val default_edge_attributes : t -> edge list
val edge_attributes : E.t -> edge list
