open Opium

(** Bind dependencies *)

module Connection = (val Infra.Database.connect ())
module PostgresRepository = Repository.Offre (Connection)
module OffreServive = Service.Offre (PostgresRepository)

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



let routes =
  [ App.get "/" root
  ; App.post "/echo" echo
  ; App.post "/entreprise" echo
  ; App.post "/contrat" echo
  ; App.post "/offre/:id_entreprise/:sigle_contrat" echo
  ; App.put "/offre/:id_offre" echo
  ; App.get "/offre/list/:ville" echo
  ; App.get "/offre/detail/:id" echo
  ]


let add_routes app = List.fold_left (fun app route -> route app) app routes

