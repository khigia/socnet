let fold_for i0 i1 f =
  let rec loop i v = if i >= i1 then v else loop (i + 1) (f v i) in
  loop i0


module Gen = struct
  
  module Make(B : Graph.Builder.INT) = struct

    let _propagate_edges g v anchor =
      let g = B.add_edge g v anchor in 
      let attach_to target g =
        if (Random.int 100) >= 50
        then
          B.add_edge g v target
        else
          g
      in
      B.G.fold_succ attach_to g anchor g

    let graph ~vn () =
      let g = B.empty () in
      let a = Array.init vn B.G.V.create in
      let g = B.add_vertex g a.(0) in
      let step g i =
        let v = a.(i) in
        let g = B.add_vertex g v in
        let anchor = a.(Random.int i) in
        let g = _propagate_edges g v anchor in
        g
      in
      fold_for 1 vn step g

  end

  module P (G : Graph.Sig.P with type V.label = int) = Make(Graph.Builder.P(G))

  module I (G : Graph.Sig.I with type V.label = int) = Make(Graph.Builder.I(G))
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


let test () =
  let module Gen = Gen.I(Graph.Pack.Digraph) in
  let g = Gen.graph ~vn:26 () in
  Graph.Pack.Digraph.display_with_gv g

let test2 () =
  let module RealBuilder = Graph.Builder.I(Graph.Pack.Digraph) in
  let module WrappedBuilder = TraceBuilder.Make(RealBuilder) in
  let module Gen = Gen.Make(WrappedBuilder) in
  let g = Gen.graph ~vn:26 () in
  Graph.Pack.Digraph.display_with_gv g

let _ = test2 ()
