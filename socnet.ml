let fold_for i0 i1 f =
  let rec loop i v = if i >= i1 then v else loop (i + 1) (f v i) in
  loop i0

module type Builder = sig
  module G : Graph.Sig.G
  type st
  val empty : st -> st * G.t
  val copy : st -> G.t -> st * G.t
  val add_vertex : st -> G.t -> G.V.t -> st * G.t
  val add_edge : st -> G.t -> G.V.t -> G.V.t -> st * G.t
  val add_edge_e : st -> G.t -> G.E.t -> st * G.t
end

(* convert Graph.Builder to Builder *)
module EmptyBuilderMake(B : Graph.Builder.S) = struct
  module G = B.G
  type st = int
  let empty s = 0, (B.empty ())
  let copy s g = s, B.copy g
  let add_vertex s g v = s, B.add_vertex g v
  let add_edge s g v1 v2 = s, B.add_edge g v1 v2
  let add_edge_e s g e = s, B.add_edge_e g e
end


module Gen = struct
  
  module Make(B : Builder with type G.V.label = int) = struct

    let _propagate_edges b g v anchor =
      let b, g = B.add_edge b g v anchor in 
      let attach_to target (b,g) =
        if (Random.int 100) >= 50
        then
          B.add_edge b g v target
        else
          b,g
      in
      B.G.fold_succ attach_to g anchor (b,g)

    let graph ~vn bst =
      let b, g = B.empty bst in
      let a = Array.init vn B.G.V.create in
      let b, g = B.add_vertex b g a.(0) in
      let step (b, g) i =
        let v = a.(i) in
        let b, g = B.add_vertex b g v in
        let anchor = a.(Random.int i) in
        let b, g = _propagate_edges b g v anchor in
        b, g
      in
      fold_for 1 vn step (b,g)

  end

end


module TraceBuilder = struct
  module Make(B : Graph.Builder.INT) = struct
    module G = B.G
    let empty = B.empty
    let copy = B.copy
    let add_vertex g v =
      let g = B.add_vertex g v in
      let _ = Printf.printf "adding vertex: %d\n" (G.V.label v) in
      g
    let add_edge g v1 v2 =
      let g = B.add_edge g v1 v2 in
      let _ = Printf.printf "adding edge: %d -> %d\n" (G.V.label v1) (G.V.label v2) in
      g
    let add_edge_e g e =
      let g = B.add_edge_e g e in
      let _ = Printf.printf "adding edge(e) ...\n" in
      g
  end
end


module UbigraphBuilder = struct
  type ubi_t = <
    change_edge_style : int32 -> int32 -> int32;
    change_vertex_style : int32 -> int32 -> int32;
    clear : unit -> int32;
    new_edge : int32 -> int32 -> int32;
    new_edge_style : int32 -> int32;
    new_edge_style_w_id : int32 -> int32 -> int32;
    new_edge_w_id : int32 -> int32 -> int32 -> int32;
    new_vertex : unit -> int32;
    new_vertex_style : int32 -> int32;
    new_vertex_style_w_id : int32 -> int32 -> int32;
    new_vertex_w_id : int32 -> int32;
    remove_edge : int32 -> int32;
    remove_vertex : int32 -> int32;
    set_edge_attribute : int32 -> string -> string -> int32;
    set_edge_style_attribute : int32 -> string -> string -> int32;
    set_vertex_attribute : int32 -> string -> string -> int32;
    set_vertex_style_attribute : int32 -> string -> string -> int32
  >
  
  type state = {
    u: ubi_t;
    gtou: (int,Int32.t) Hashtbl.t;
  }
  
  let make_state url n =
    let c = new Ubigraph.client url in
    let u = c#ubigraph in
    {
      u = u;
      gtou = Hashtbl.create n;
    }


  module Make(B : Graph.Builder.INT) = struct
    module G = B.G

    type st = state

    let empty s =
      let _ = s.u#clear () in
      s, (B.empty ())

    let copy s g = s, B.copy g

    let add_vertex s g v =
      let x = s.u#new_vertex () in
      let _ = Printf.printf "ubi vertex id: %s\n" (Int32.to_string x) in
      let _ = s.u#set_vertex_attribute x "shape" "sphere" in
      let _ = s.u#set_vertex_attribute x "color" "#ffff00" in
      let _ = Hashtbl.add s.gtou (B.G.V.label v) x in
      s, B.add_vertex g v

    let add_edge s g v1 v2 =
      let uv1 = Hashtbl.find s.gtou (B.G.V.label v1) in
      let uv2 = Hashtbl.find s.gtou (B.G.V.label v2) in
      let e = s.u#new_edge uv1 uv2 in
      s, B.add_edge g v1 v2

    let add_edge_e s g e = s, B.add_edge_e g e
  end
end


let test () =
  let module GBuilder = Graph.Builder.I(Graph.Pack.Digraph) in
  let module TBuilder = TraceBuilder.Make(GBuilder) in
  let module SBuilder = EmptyBuilderMake(TBuilder) in
  let module Gen = Gen.Make(SBuilder) in
  let b, g = Gen.graph ~vn:26 0 in
  Graph.Pack.Digraph.display_with_gv g

let test2 () =
  let n = 86 in
  let module GBuilder = Graph.Builder.I(Graph.Pack.Digraph) in
  let module TBuilder = TraceBuilder.Make(GBuilder) in
  let module SBuilder = UbigraphBuilder.Make(TBuilder) in
  let module Gen = Gen.Make(SBuilder) in
  let s = UbigraphBuilder.make_state "http://localhost:20738/RPC2" n in
  let b, g = Gen.graph ~vn:n s in
  Graph.Pack.Digraph.display_with_gv g

let _ = 
  let _ = test () in
  let _ = test2 () in
  ()
