
module Uuid : sig
  type t

  val v4_gen : Random.State.t -> unit -> t

  val show : t -> string

  val pp : Format.formatter -> t -> unit

  val make : string -> (t, string) result

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) result
end


module Email : sig
  (* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/ *)
  type t

  val pp :
    Ppx_deriving_runtime.Format.formatter -> t -> Ppx_deriving_runtime.unit

  val show : t -> Ppx_deriving_runtime.string

  val make : string -> (t, string) result

  val to_string : t -> string

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Entreprise : sig 
  type t = {
    id : Uuid.t;
    libelle : string;
    description : string;
    numero : string;(* faire un module expres pour verifier le format*)
    rue : string; (* possible de faire une verification avec une api on verra *)
    code_postal : int; (* faire un module expres pour verifier le format*)
    ville : string (* possible de faire une verification avec une api en fonction du CP on verra*)
  }
  [@@deriving make, show, yojson]
  val of_string : string -> t
  val to_yojson : t -> Yojson.Safe.t
  val ville_list: string list -> Yojson.Safe.t
  val to_yojson_as_list : t list -> Yojson.Safe.t
end

module Contrat : sig 
  type t = {
    id : Uuid.t;
    sigle : string; (* faire un module specifique*)
    description : string
  }
  [@@deriving make, show, yojson]
  val of_string : string -> t
  val to_yojson_as_list : t list -> Yojson.Safe.t
end

module Offre : sig
  type t = {
    id : Uuid.t;
    titre : string;
    description : string;
    created_at : Date.t;
    end_at : Date.t;
    
    entreprise : Entreprise.t;
    contrat : Contrat.t;
    contact : Email.t;
    duree : int option;
    membre_id : Uuid.t
  }
  [@@deriving make, show, yojson]
  
  val to_yojson : t -> Yojson.Safe.t
  val to_yojson_as_list : (int * t) list -> Yojson.Safe.t
  (*val from_string_child : entreprise_str:string ->
    contrat_str:string ->
    ?id:Uuid.t ->
    titre:string ->
    description:string ->
    created_at:Offre__Date.t ->
    end_at:Offre__Date.t ->
    contact:Email.t ->
    ?duree:int -> unit -> t
*)
  
    module Disable : sig
      type t = {
        id:Uuid.t;
        titre:string;
        membre_id:Uuid.t
      }[@@deriving make, show, yojson]
      val of_pair : Uuid.t * string * Uuid.t -> t
      val to_list_yojson : t list-> Yojson.Safe.t
    end
  
end

val empty_yojson:[> `Assoc of 'a list ]
