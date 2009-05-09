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

exception Type_error of string

class client : string ->
object
  method rpc : XmlRpc.client

  method ubigraph :
    < change_edge_style : int32 -> int32 -> int32;
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
    set_vertex_style_attribute : int32 -> string -> string -> int32 >

  method system :
    < listMethods : unit -> XmlRpc.value list;
    methodHelp : string -> string;
    methodSignature : string -> XmlRpc.value list;
    multicall : XmlRpc.value list -> XmlRpc.value list;
    shutdown : string -> int32 >
end
