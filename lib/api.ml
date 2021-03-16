open Opium

(** Bind dependencies *)

module Connection = (val Infra.Database.connect ())
module PostgresRepository = Repository.Offre (Connection)
module OffreService = Service.Offre (PostgresRepository)

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
  let uuid = Router.param req "id" in
  let open Yojson.Safe.Util in
  (*let jwt = Option.value (Request.header "Authorization" req) ~default:""  in*)
  let json = ( req |> Request.to_json  ) in
  match (Error "not implemented") with
      | Error e ->
        json_response_of_a_string "Error" e ~status:`Forbidden
        |> Lwt.return
      | Ok _ -> action ~uuid ~json

let create_contrat req =
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
        json_response_of_a_string "Error" e ~status:`Forbidden
        |> Lwt.return
      | Ok _ -> Response.make ~status:`Created () |> Lwt.return)

let create_entreprise req =
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
        json_response_of_a_string "Error" e ~status:`Forbidden
        |> Lwt.return
      | Ok _ -> Response.make ~status:`Created () |> Lwt.return)




  let create req =
    let open Lwt in
    req
    |> Request.to_json
    >>= function
    | None -> Response.make ~status:`Bad_request () |> Lwt.return
    | Some json ->
        let open Yojson.Safe.Util in
        
        let titre = json |> member "libelle" |> to_string
        and description = json |> member "description" |> to_string
        and id = json |> member "id" |> to_int_option
        and duree = json |> member "duree" |> to_int_option
        
        and contact_str = json |> member "contact" |> to_string
        and created_at_str = json |> member "created_at" |> to_string
        and end_at_str = json |> member "end_at" |> to_string

        and entreprise = Router.param req "id_entreprise" |> int_of_string
        and contrat = Router.param req "sigle_contrat"
        
        in
        OffreService.create ?id ?duree ~titre ~description ~created_at_str ~end_at_str ~entreprise ~contrat ~contact_str
        >>= (function
        | Error e ->
          json_response_of_a_string "Error" e ~status:`Forbidden
          |> Lwt.return
        | Ok _ -> Response.make ~status:`Created () |> Lwt.return)

let get_by_id req =
    let open Lwt in
    let open Yojson.Safe.Util in
    let id = Router.param req "id_offre" |> int_of_string in
    OffreService.get_by_id ~id
    >>= (function
    | Error e ->
      json_response_of_a_string "Error" e ~status:`Forbidden
      |> Lwt.return
    | Ok res -> Response.of_json res |> Lwt.return)


let routes =
  [ App.get "/" root
  ; App.post "/echo" echo
  ; App.post "/entreprise" create_entreprise
  ; App.post "/contrat" create_contrat
  ; App.post "/offre/:id_entreprise/:sigle_contrat" create
  ; App.put "/offre/:id_offre" echo
  ; App.delete "/offre/:id_offre" echo
  ; App.get "/offre/list/:ville" echo
  ; App.get "/offre/detail/:id" get_by_id
  ]


let add_routes app = List.fold_left (fun app route -> route app) app routes

