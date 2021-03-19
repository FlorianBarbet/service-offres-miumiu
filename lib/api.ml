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
        let id = json |> member "id" |> to_string in
        action req ~id

let create_contrat req ~id=
  let open Lwt in
  req
  |> Request.to_json
  >>= function
  | None -> Response.make ~status:`Bad_request () |> Lwt.return
  | Some json ->
      let open Yojson.Safe.Util in
      let sigle = json |> member "sigle" |> to_string
      and description = json |> member "description" |> to_string in
      OffreService.create_contrat ~sigle ~description
      >>= (function
      | Error e ->
        json_response_of_a_string "error" e ~status:`Forbidden
        |> Lwt.return
      | Ok _ -> Response.make ~status:`Created () |> Lwt.return)

let create_entreprise req ~id=
  let open Lwt in
  req
  |> Request.to_json
  >>= function
  | None -> Response.make ~status:`Bad_request () |> Lwt.return
  | Some json ->
      let open Yojson.Safe.Util in
      
      let libelle = json |> member "libelle" |> to_string
      and description = json |> member "description" |> to_string
      and id = json |> member "id" |> to_int_option
      and numero = json |> member "numero" |> to_string
      and rue = json |> member "rue" |> to_string
      and code_postal = json |> member "code_postal" |> to_int
      and ville = json |> member "ville" |> to_string
      
     in
      OffreService.create_entreprise ?id ~libelle ~description ~numero ~rue ~code_postal ~ville
      >>= (function
      | Error e ->
        json_response_of_a_string "error" e ~status:`Forbidden
        |> Lwt.return
      | Ok _ -> Response.make ~status:`Created () |> Lwt.return)


  let get_contact action req ~id = 
    let open Lwt in    
    let jwt = Option.value (Request.header "Authorization" req) ~default:"" in
    MembreService.get_email_by_id ~id ~headers:(RestConfiguration.authorization jwt) 
    >>= function
    | Error e -> json_response_of_a_string "error" e ~status:`Forbidden |> Lwt.return
    | Ok response ->
      let open Yojson.Safe.Util in
      let json = response |> Yojson.Safe.from_string in 
      let email = json |> member "email" |> to_string in
      action req ~id ~email

  let create req ~id ~email=
    let open Lwt in    
    req
    |> Request.to_json
    >>= function
    | None -> Response.make ~status:`Bad_request () |> Lwt.return
    | Some json ->
        let open Yojson.Safe.Util in
        
        let titre = json |> member "titre" |> to_string
        and description = json |> member "description" |> to_string
        and duree = json |> member "duree" |> to_int_option  
        and contact_str = email

        and created_at_str = json |> member "created_at" |> to_string
        and end_at_str = json |> member "end_at" |> to_string

        and entreprise = Router.param req "id_entreprise" |> int_of_string
        and contrat = Router.param req "sigle_contrat"
        
        in
        OffreService.create ?duree ~titre ~description ~created_at_str ~end_at_str ~entreprise ~contrat ~contact_str
        >>= (function
        | Error e ->
          json_response_of_a_string "error" e ~status:`Forbidden
          |> Lwt.return
        | Ok _ -> Response.make ~status:`Created () |> Lwt.return)

  let update req ~id ~email =
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

        and entreprise_opt = json |> member "ent" |> to_int_option
        and contrat_opt = json |> member "ent" |> to_string_option

        and id = Router.param req "id_offre" |> int_of_string
        in
        OffreService.update ?duree ?titre_opt ?description_opt ?created_at_str ?end_at_str ?entreprise_opt ?contrat_opt ~id ~email
        >>= (function
        | Error e ->
          json_response_of_a_string "error" e ~status:`Forbidden
          |> Lwt.return
        | Ok _ -> Response.make ~status:`Created () |> Lwt.return)

  let delete req ~id ~email =
    let open Lwt in
    let open Yojson.Safe.Util in
    let id = Router.param req "id_offre" |> int_of_string in
    OffreService.delete ~id ~email
    >>= (function
    | Error e ->
      json_response_of_a_string "error" e ~status:`Forbidden
      |> Lwt.return
    | Ok res -> Response.make ~status:`OK () |> Lwt.return)
  
  let enable_offre req ~id ~email =
    let open Lwt in
    let open Yojson.Safe.Util in
    let id = Router.param req "id_offre" |> int_of_string in
    OffreService.enable_offre ~id ~email
    >>= (function
    | Error e ->
      json_response_of_a_string "error" e ~status:`Forbidden
      |> Lwt.return
    | Ok res -> Response.make ~status:`OK () |> Lwt.return)

  let get_by_id req =
      let open Lwt in
      let open Yojson.Safe.Util in
      let id = Router.param req "id_offre" |> int_of_string in
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
    let _ = print_endline ville in
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
    ; App.post "/offre/:id_entreprise/:sigle_contrat" (check_auth @@ get_contact @@ create)
    ; App.put "/offre/:id_offre" (check_auth @@ get_contact @@ update)
    ; App.delete "/offre/:id_offre" (check_auth @@ get_contact @@ delete )
    ; App.get "/offre/list/:ville" get_by_ville
    ; App.get "/offre/detail/:id_offre" get_by_id
    ; App.get "/offre/villes" get_villes
    ; App.get "/offre/disable" get_disable_offres
    ; App.get "/entreprise" get_entreprises
    ; App.get "/contrat" get_contrats
    ; App.put "/disable-offre/:id_offre" (check_auth @@ get_contact @@enable_offre)
    ]


  let add_routes app = List.fold_left (fun app route -> route app) app routes

