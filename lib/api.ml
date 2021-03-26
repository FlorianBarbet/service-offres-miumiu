open Opium

(** Bind dependencies *)

module Connection = (val Infra.Database.connect ())
module PostgresRepository = Repository.Offre (Connection)

module RestConfiguration = ( Infra.RestConfiguration)
module RestRepository = Repository.Membre(RestConfiguration)
module OffreService = Service.Offre (PostgresRepository)
module MembreService = Service.Membre(RestRepository)

let set_logger () =
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  Logs.set_level Infra.Environment.log_level

let json_response_of_a_string name str ~status = Response.of_json (`Assoc [name, `String (str)]) ~status
(** Heartbeat route *)
let root req =
  Printf.sprintf "Welcome to offre server"
  |> Response.of_plain_text
  |> Lwt.return


(** Testing purpose route *)
let echo req =
  let open Lwt in
  req
  |> Request.to_json
  >>= fun json ->
  let body = Option.get json |> Yojson.Safe.to_string |> Body.of_string in
  Response.make ~body () |> Lwt.return

  
let check_auth action req=     
  let open Lwt in
  let open Yojson.Safe.Util in
  let jwt = Option.value (Request.header "Authorization" req) ~default:""  in
  MembreService.verify ~headers:(RestConfiguration.authorization jwt) >>= function
      | Error e ->
        json_response_of_a_string "error" e ~status:`Forbidden
        |> Lwt.return
      | Ok response ->
        let json = response |> Yojson.Safe.from_string in 
        let membre_id = json |> member "id" |> to_string in
        action req ~membre_id

let create_contrat req ~membre_id=
  let open Lwt in
  req
  |> Request.to_json
  >>= function
  | None -> Response.make ~status:`Bad_request () |> Lwt.return
  | Some json ->
      let open Yojson.Safe.Util in
      let sigle = json |> member "sigle" |> to_string |> String.uppercase_ascii
      and description = json |> member "description" |> to_string in
      OffreService.create_contrat ~sigle ~description
      >>= (function
      | Error e ->
        json_response_of_a_string "error" e ~status:`Forbidden
        |> Lwt.return
      | Ok _ -> Response.make ~status:`Created () |> Lwt.return)

let create_entreprise req ~membre_id=
  let open Lwt in
  req
  |> Request.to_json
  >>= function
  | None -> Response.make ~status:`Bad_request () |> Lwt.return
  | Some json ->
      let open Yojson.Safe.Util in
      
      let libelle = json |> member "libelle" |> to_string 
      and description = json |> member "description" |> to_string
      and numero = json |> member "numero" |> to_string |> String.uppercase_ascii
      and rue = json |> member "rue" |> to_string |> String.uppercase_ascii
      and code_postal = json |> member "code_postal" |> to_int
      and ville = json |> member "ville" |> to_string |> String.uppercase_ascii
      
     in
      OffreService.create_entreprise ~libelle ~description ~numero ~rue ~code_postal ~ville
      >>= (function
      | Error e ->
        json_response_of_a_string "error" e ~status:`Forbidden
        |> Lwt.return
      | Ok _ -> Response.make ~status:`Created () |> Lwt.return)


  let create req ~membre_id=
    let open Lwt in    
    req
    |> Request.to_json
    >>= function
    | None -> Response.make ~status:`Bad_request () |> Lwt.return
    | Some json ->
        let open Yojson.Safe.Util in
        
        let titre = json |> member "titre" |> to_string |> String.uppercase_ascii
        and description = json |> member "description" |> to_string
        and duree = json |> member "duree" |> to_int_option  
        and contact_str = json |> member "contact" |> to_string

        and created_at_str = json |> member "created_at" |> to_string
        and end_at_str = json |> member "end_at" |> to_string

        and entreprise = Router.param req "id_entreprise"
        and contrat = Router.param req "sigle_contrat"
        
        in
        OffreService.create ?duree ~titre ~description ~created_at_str ~end_at_str ~entreprise ~contrat ~contact_str ~membre_id
        >>= (function
        | Error e ->
          json_response_of_a_string "error" e ~status:`Forbidden
          |> Lwt.return
        | Ok _ -> Response.make ~status:`Created () |> Lwt.return)

  let update req ~membre_id =
    let open Lwt in
    req
    |> Request.to_json
    >>= function
    | None -> Response.make ~status:`Bad_request () |> Lwt.return
    | Some json ->
        let open Yojson.Safe.Util in
        
        let titre_opt = json |> member "titre" |> to_string_option
        and description_opt = json |> member "description" |> to_string_option
        and duree = json |> member "duree" |> to_int_option

        and created_at_str = json |> member "created_at" |> to_string_option
        and end_at_str = json |> member "end_at" |> to_string_option

        and entreprise_opt = json |> member "ent" |> to_string_option
        and contrat_opt = json |> member "ent" |> to_string_option

        and id = Router.param req "id_offre"
        in
        OffreService.update ?duree ?titre_opt ?description_opt ?created_at_str ?end_at_str ?entreprise_opt ?contrat_opt ~id ~membre_id
        >>= (function
        | Error e ->
          json_response_of_a_string "error" e ~status:`Forbidden
          |> Lwt.return
        | Ok _ -> Response.make ~status:`Created () |> Lwt.return)

  let delete req ~membre_id =
    let open Lwt in
    let open Yojson.Safe.Util in
    let id = Router.param req "id_offre" in
    OffreService.delete ~id ~membre_id
    >>= (function
    | Error e ->
      json_response_of_a_string "error" e ~status:`Forbidden
      |> Lwt.return
    | Ok res -> Response.make ~status:`OK () |> Lwt.return)
  
  let enable_offre req ~membre_id =
    let open Lwt in
    let open Yojson.Safe.Util in
    let id = Router.param req "id_offre"  in
    OffreService.enable_offre ~id ~membre_id
    >>= (function
    | Error e ->
      json_response_of_a_string "error" e ~status:`Forbidden
      |> Lwt.return
    | Ok res -> Response.make ~status:`OK () |> Lwt.return)

  let get_all_member_offre req ~membre_id =
    let open Lwt in
    let open Yojson.Safe.Util in
    OffreService.get_all_member_offre ~membre_id
    >>= (function
    | Error e ->
      json_response_of_a_string "error" e ~status:`Forbidden
      |> Lwt.return
    | Ok res -> Response.of_json res |> Lwt.return)

  let get_by_id req =
      let open Lwt in
      let open Yojson.Safe.Util in
      let id = Router.param req "id_offre"  in
      OffreService.get_by_id ~id
      >>= (function
      | Error e ->
        json_response_of_a_string "error" e ~status:`Forbidden
        |> Lwt.return
      | Ok res -> Response.of_json res |> Lwt.return)


  let get_by_ville req =
    let open Lwt in
    let open Yojson.Safe.Util in
    let ville = Router.param req "ville"  in
    OffreService.get_by_ville ~ville
    >>= (function
    | Error e ->
      json_response_of_a_string "error" e ~status:`Forbidden
      |> Lwt.return
    | Ok res -> Response.of_json res |> Lwt.return)

  let get_villes req =
    let open Lwt in
    let open Yojson.Safe.Util in

    OffreService.get_villes () 
    >>= (function
    | Error e ->
      json_response_of_a_string "error" e ~status:`Forbidden
      |> Lwt.return
    | Ok res -> Response.of_json res |> Lwt.return)
  
  let get_disable_offres req = 
    let open Lwt in
    let open Yojson.Safe.Util in
    
    OffreService.get_disable_offres () 
    >>= (function
    | Error e ->
      json_response_of_a_string "error" e ~status:`Forbidden
      |> Lwt.return
    | Ok res -> Response.of_json res |> Lwt.return)
  
  let get_entreprises req =
    let open Lwt in
    let open Yojson.Safe.Util in

    OffreService.get_entreprises () 
    >>= (function
    | Error e ->
      json_response_of_a_string "error" e ~status:`Forbidden
      |> Lwt.return
    | Ok res -> Response.of_json res |> Lwt.return)

  let get_contrats req =
    let open Lwt in
    let open Yojson.Safe.Util in

    OffreService.get_contrats () 
    >>= (function
    | Error e ->
      json_response_of_a_string "error" e ~status:`Forbidden
      |> Lwt.return
    | Ok res -> Response.of_json res |> Lwt.return)
  
  let routes =
    [ App.get "/" root
    ; App.post "/echo" echo
    ; App.post "/entreprise" (check_auth @@ create_entreprise)
    ; App.post "/contrat" (check_auth @@ create_contrat)
    ; App.post "/offre/:id_entreprise/:sigle_contrat" (check_auth @@ create)
    ; App.put "/offre/:id_offre" (check_auth @@ update)
    ; App.delete "/offre/:id_offre" (check_auth @@  delete )
    ; App.get "/offre/list/:ville" get_by_ville
    ; App.get "/offre/detail/:id_offre" get_by_id
    ; App.get "/offre/villes" get_villes
    ; App.get "/offre/disable" get_disable_offres
    ; App.get "/entreprise" get_entreprises
    ; App.get "/contrat" get_contrats
    ; App.put "/disable-offre/:id_offre" (check_auth @@ enable_offre)
    ; App.get "/offre" (check_auth @@ get_all_member_offre)
    ]


  let add_routes app = List.fold_left (fun app route -> route app) app routes

  let _ = print_endline @@ "Date server start : "^Date.show @@ Date.now ()

