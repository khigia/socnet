let fold_for i0 i1 f =
  let rec loop i v = if i >= i1 then v else loop (i + 1) (f v i) in
  loop i0

(* wrap Graph.Builder to handle a state *)
module type Builder = sig
  module G : Graph.Sig.G
  type st
  val empty : st -> st * G.t
  val copy : st -> G.t -> st * G.t
  val add_vertex : st -> G.t -> G.V.t -> st * G.t
  val add_edge : st -> G.t -> G.V.t -> G.V.t -> st * G.t
  val add_edge_e : st -> G.t -> G.E.t -> st * G.t
end

(* convert Graph.Builder to Builder with empty state *)
module EmptyBuilderMake(B : Graph.Builder.S) = struct
  module G = B.G
  type st = int
  let empty s = 0, (B.empty ())
  let copy s g = s, B.copy g
  let add_vertex s g v = s, B.add_vertex g v
  let add_edge s g v1 v2 = s, B.add_edge g v1 v2
  let add_edge_e s g e = s, B.add_edge_e g e
end


(* generate random graph by adding node and propagating edges *)
module Gen = struct
  
  let fwd_p = 0.25
  let bck_p = 0.35
  let bi_p  = 0.40

  module IntSet = Set.Make(struct type t = int let compare = compare end)

  module Make(B : Builder with type G.V.label = int) = struct

    let rec _propagate_edges fwdp bckp bip seen deg b g v anchor =
      let odeg = B.G.out_degree g anchor in
      let ideg = B.G.in_degree g anchor in
      let deg = odeg + ideg in
      let mean = (1. /. (1. -. fwdp)) in
      let p = 1000. *. mean /. (float_of_int deg) in
      let r = bckp *. p in
      let ip = int_of_float p in
      let ir = int_of_float r in
      let attach_to prob target (s, b, g) =
        if v == target
        then
          s, b, g
        else
          if IntSet.mem (B.G.V.label target) s
          then
            s, b, g
          else
            if (Random.int 1000) < prob
            then
              let b, g = B.add_edge b g v target in
              let b, g = if (Random.float 1.0) < bip
                then
                  B.add_edge b g target v
                else
                  b, g
              in
              let s = IntSet.add (B.G.V.label target) s in
              _propagate_edges fwdp bckp bip s deg b g v target
            else
              s, b, g
      in
      let seen, b, g = B.G.fold_succ (attach_to ip) g anchor (seen, b, g) in
      let seen, b, g = B.G.fold_pred (attach_to ir) g anchor (seen, b, g) in
      seen, b, g

    let graph ?(fwdp=fwd_p) ?(bckp=bck_p) ?(bip=bi_p) ~vn bst =
      let b, g = B.empty bst in
      let a = Array.init vn B.G.V.create in
      let b, g = B.add_vertex b g a.(0) in
      let step (b, g) i =
        let v = a.(i) in
        let b, g = B.add_vertex b g v in
        let anchor = a.(Random.int i) in
        let b, g = B.add_edge b g v anchor in 
        let seen = IntSet.singleton (B.G.V.label anchor) in
        let seen, b, g = _propagate_edges fwdp bckp bip seen 0 b g v anchor in
        b, g
      in
      fold_for 1 vn step (b,g)

  end

end


(* builder calling ubigraph through xml rpc: duplicate graph in Graph and Ubigraph.
simple impl: the graph node do not contains the ubigraph ref but we keep track
through hashtbl
*)
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
      let _e = s.u#new_edge uv1 uv2 in
      s, B.add_edge g v1 v2

    let add_edge_e s g e = s, B.add_edge_e g e
  end
end


let _ =
  try
    let n = ref 12 in
    let gv = ref false in
    let ubi = ref false in
    let fwdp = ref 0.0 in fwdp := Gen.fwd_p;
    let bckp = ref 0.0 in bckp := Gen.bck_p;
    let bip = ref 0.0 in bip := Gen.bi_p;
    let _ = Arg.parse
      [
        ("-n", Arg.Int ((:=) n), "number of nodes");
        ("-gv", Arg.Set gv, "display with gv");
        ("-ubi", Arg.Set ubi, "display with ubigraph (localhost:20738)");
        ("-fwd", Arg.Set_float fwdp, "forward probability");
        ("-bck", Arg.Set_float bckp, "backward probability");
        ("-bi", Arg.Set_float bip, "bidirectional probability");
      ]
      ignore
      ""
    in
    let module G = Graph.Pack.Digraph in
    let module GBuilder = Graph.Builder.I(G) in
    let module RBuilder = EmptyBuilderMake(GBuilder) in
    let module RGen = Gen.Make(RBuilder) in
    let module UBuilder = UbigraphBuilder.Make(GBuilder) in
    let module UGen = Gen.Make(UBuilder) in
    let _ = Random.self_init () in
    let g = if !ubi
      then
        let s = UbigraphBuilder.make_state "http://localhost:20738/RPC2" !n in
        let b, g = UGen.graph ~fwdp:!fwdp ~bckp:!bckp ~bip:!bip ~vn:!n s in
        g
      else
        let s = 1 in
        let b, g = RGen.graph ~fwdp:!fwdp ~bckp:!bckp ~bip:!bip ~vn:!n s in
        g
    in
    let d = 0.85 in
    let h = (1. -. d) /. (float_of_int !n) in
    let pr pr1 pr2 v =
      let f pred acc =
        let pred_i = G.V.label pred in
        let pred_pr = pr1.(pred_i) in
        let pred_c = float_of_int (G.out_degree g pred) in
        acc +. (pred_pr /. pred_c)
      in
      let influence = G.fold_pred f g v 0.0 in
      let i = G.V.label v in
      let p = h +. d *. (influence) in
      Array.set pr2 i p
    in
    let rec iter n p1 p2 =
      if n > 0
      then
        let _ = G.iter_vertex (pr p1 p2) g in
        iter (n-1) p2 p1
      else
        p2
    in
    let pr1 = Array.make !n 1.0 in
    let pr2 = Array.make !n 0.0 in
    let p = iter 15 pr1 pr2 in
    let _ = Array.iteri (Printf.printf "%d %f\n") p in
    let _ = flush_all () in
    let _ = if !gv then G.display_with_gv g in
    ()
  with
    exn -> Printf.printf "ERROR:%s\n" (Printexc.to_string exn)
