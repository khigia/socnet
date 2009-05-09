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



let test () =
  let module Gen = Gen.I(Graph.Pack.Digraph) in
  let g = Gen.graph ~vn:26 () in
  Graph.Pack.Digraph.display_with_gv g

let _ = test ()
