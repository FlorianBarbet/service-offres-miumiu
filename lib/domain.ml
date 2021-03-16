
module Email = struct
  type t = Emile.address [@@deriving show]

  let make email =
    let maked_email = Emile.address_of_string email in
    match maked_email with
    | Ok address -> Ok address
    | Error _ -> Error "Invalid_Email"

  let to_string email = email |> Emile.address_to_string

  let to_yojson email = Yojson.Safe.from_string @@ show email

  let of_yojson json = make @@ Yojson.Safe.to_string json
end

module Entreprise = 
struct 
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

  let of_string str= let split_on_char = String.split_on_char '#' str in
    let id = int_of_string @@ List.nth split_on_char 0
    and libelle = List.nth split_on_char 1
    and description = List.nth split_on_char 2
    and numero = List.nth split_on_char 3
    and rue = List.nth split_on_char 4
    and code_postal = List.nth split_on_char 5
    and ville = List.nth split_on_char 6 in
   make ~id ~libelle ~description ~numero ~rue ~code_postal ~ville ()
end

module Contrat =
struct 
  type t = {
    sigle : string; (* faire un module specifique*)
    description : string
  }
  [@@deriving make, show, yojson]

  let of_string str= let split_on_char = String.split_on_char '#' str in
  let sigle = List.nth split_on_char 0
  and description = List.nth split_on_char 1 in
   make ~sigle ~description 

end

module Offre = struct
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