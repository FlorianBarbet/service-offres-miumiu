(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/ *)

module Email : sig
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
    id : int option;
    libelle : string;
    description : string;
    numero : string;(* faire un module expres pour verifier le format*)
    rue : string; (* possible de faire une verification avec une api on verra *)
    code_postal : string; (* faire un module expres pour verifier le format*)
    ville : string (* possible de faire une verification avec une api en fonction du CP on verra*)
  }
  [@@deriving make, show, yojson]
  val of_string : string -> t
end

module Contrat : sig 
  type t = {
    sigle : string; (* faire un module specifique*)
    description : string
  }
  [@@deriving make, show, yojson]
  val of_string : string -> t
end

module Offre : sig
  type t = {
    id : int option;
    titre : string;
    description : string;
    created_at : Date.t;
    end_at : Date.t;
    
    entreprise : Entreprise.t;
    contrat : Contrat.t;
    contact : Email.t;
    duree : int option
  }
  [@@deriving make, show, yojson]

end