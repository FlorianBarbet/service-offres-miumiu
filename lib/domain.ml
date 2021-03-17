
let parser str= (*it's used to parse data from DB into our domain*)
  let get_lt_from_fold (lt,_)= (*let _ = print_endline @@ List.nth lt 0  in*) lt in  
  String.split_on_char ',' str 
  |> List.fold_left (fun (lt,sub) s -> 

    match String.get s 0 with
    | '"' when String.get s @@ String.length s -1 != '"' -> (lt,sub^(String.sub s 1 @@ String.length s -1))  
    | _ when String.get s @@ String.length s -1 = '"' -> 
      let _ = print_endline s in
      let rm_quote =( String.sub s (if String.get s 0 = '"' then 1 else 0) @@ String.length s -1) in
      let clean_txt = if String.get rm_quote @@ String.length rm_quote -1 = '"' then (String.sub rm_quote 0 @@ String.length rm_quote-1) else rm_quote in
      ((if sub <> "" then sub^", "^clean_txt else clean_txt)::lt,"")
    | _ when sub <> "" -> (lt,sub ^", " ^ s)
    | _ -> (s::lt,sub)) ([],"") |> get_lt_from_fold |> List.rev



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
    code_postal : int; (* faire un module expres pour verifier le format*)
    ville : string (* possible de faire une verification avec une api en fonction du CP on verra*)
  }
  [@@deriving make, show, yojson]

  let ville_list (lt:string list) = Yojson.Safe.from_string @@ Yojson.Safe.to_string @@`Assoc [ "villes",`List (List.map (fun e -> `String e) lt) ]

  let to_yojson entreprise =
    let from_option_int option_int = match option_int with 
    | Some i -> `Int i 
    | None -> `String "" in
     
    `Assoc [
       "id", from_option_int entreprise.id; 
       "libelle", `String entreprise.libelle;
       "description", `String entreprise.description;
       "numero", `String entreprise.numero;
       "rue", `String entreprise.rue;
       "code_postal", `Int entreprise.code_postal;
       "ville", `String entreprise.ville
    ]

  let of_string str= 
    let str_without_parenthesis = String.sub str 1 @@ (String.length str) - 2 in
      let split_on_char = parser str_without_parenthesis in
      let id = int_of_string @@ List.nth split_on_char 0
      and libelle = List.nth split_on_char 1
      and description = List.nth split_on_char 2
      and numero = List.nth split_on_char 3
      and rue = List.nth split_on_char 4
      and code_postal = int_of_string @@ List.nth split_on_char 5
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

  let to_yojson contrat =
    `Assoc [
       "sigle", `String contrat.sigle; 
       "description", `String contrat.description
    ]

  let of_string str= 
  let str_without_parenthesis = String.sub str 1 @@ (String.length str) - 2 in
  let split_on_char = parser str_without_parenthesis in
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

  let to_yojson offre =
     let from_option_int option_int = match option_int with 
     | Some i -> `Int i 
     | None -> `String "" in (* impossible de mettre `Null Yojson.Safe ne l'accepte pas en ecriture juste en lecture pour les to_XXX_option*)
     Yojson.Safe.from_string @@ Yojson.Safe.to_string @@
     `Assoc [
        "id", from_option_int offre.id; 
        "titre", `String offre.titre;
        "description", `String offre.description;
        "created_at", `String (Date.show  offre.created_at) ;
        "end_at",  `String (Date.show  offre.end_at);
        "entreprise",Entreprise.to_yojson offre.entreprise;
        "contrat", Contrat.to_yojson offre.contrat
      ] 

  let to_yojson_as_list offres = Yojson.Safe.from_string @@ Yojson.Safe.to_string @@ `Assoc 
  ( List.map (fun a -> (a.titre),to_yojson a) offres)


  let from_string_child ~entreprise_str ~contrat_str = 
    let entreprise = Entreprise.of_string entreprise_str
    and contrat = Contrat.of_string contrat_str in make ~entreprise ~contrat

    

end

let empty_yojson = `Assoc[]