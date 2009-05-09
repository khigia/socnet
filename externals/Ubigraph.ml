(*
 * XmlRpc Light, a small XmlRpc library based on Xml Light and Ocamlnet
 * Copyright (C) 2008 Dave Benjamin (dave@ramenlabs.com)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Automatically generated from an XML-RPC server
   by running the following command:

   ../genclient/genclient -i http://localhost:20738/RPC2
*)

exception Type_error of string

class client url =
  let rpc = new XmlRpc.client url in
object (self)
  method rpc = rpc

  method system = object
    method shutdown _0 =
      let result = rpc#call "system.shutdown" [`String _0] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method multicall _0 =
      let result = rpc#call "system.multicall" [`Array _0] in
      match result with
        | `Array r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method methodHelp _0 =
      let result = rpc#call "system.methodHelp" [`String _0] in
      match result with
        | `String r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method methodSignature _0 =
      let result = rpc#call "system.methodSignature" [`String _0] in
      match result with
        | `Array r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method listMethods () =
      let result = rpc#call "system.listMethods" [] in
      match result with
        | `Array r -> r
        | other -> raise (Type_error (XmlRpc.dump other))
  end

  method ubigraph = object
    method change_edge_style _0 _1 =
      let result = rpc#call "ubigraph.change_edge_style" [`Int32 _0; `Int32 _1] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method new_edge_style_w_id _0 _1 =
      let result = rpc#call "ubigraph.new_edge_style_w_id" [`Int32 _0; `Int32 _1] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method new_edge_style _0 =
      let result = rpc#call "ubigraph.new_edge_style" [`Int32 _0] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method set_edge_style_attribute _0 _1 _2 =
      let result = rpc#call "ubigraph.set_edge_style_attribute" [`Int32 _0; `String _1; `String _2] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method set_edge_attribute _0 _1 _2 =
      let result = rpc#call "ubigraph.set_edge_attribute" [`Int32 _0; `String _1; `String _2] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method change_vertex_style _0 _1 =
      let result = rpc#call "ubigraph.change_vertex_style" [`Int32 _0; `Int32 _1] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method new_vertex_style_w_id _0 _1 =
      let result = rpc#call "ubigraph.new_vertex_style_w_id" [`Int32 _0; `Int32 _1] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method new_vertex_style _0 =
      let result = rpc#call "ubigraph.new_vertex_style" [`Int32 _0] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method set_vertex_style_attribute _0 _1 _2 =
      let result = rpc#call "ubigraph.set_vertex_style_attribute" [`Int32 _0; `String _1; `String _2] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method set_vertex_attribute _0 _1 _2 =
      let result = rpc#call "ubigraph.set_vertex_attribute" [`Int32 _0; `String _1; `String _2] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method clear () =
      let result = rpc#call "ubigraph.clear" [] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method remove_edge _0 =
      let result = rpc#call "ubigraph.remove_edge" [`Int32 _0] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method remove_vertex _0 =
      let result = rpc#call "ubigraph.remove_vertex" [`Int32 _0] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method new_edge _0 _1 =
      let result = rpc#call "ubigraph.new_edge" [`Int32 _0; `Int32 _1] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method new_edge_w_id _0 _1 _2 =
      let result = rpc#call "ubigraph.new_edge_w_id" [`Int32 _0; `Int32 _1; `Int32 _2] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method new_vertex () =
      let result = rpc#call "ubigraph.new_vertex" [] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))

    method new_vertex_w_id _0 =
      let result = rpc#call "ubigraph.new_vertex_w_id" [`Int32 _0] in
      match result with
        | `Int r -> Int32.of_int r | `Int32 r -> r
        | other -> raise (Type_error (XmlRpc.dump other))
  end

end
