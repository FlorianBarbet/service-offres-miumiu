(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/ *)

module Environment = struct
  let random_seed = Random.State.make_self_init ()

  let app_name =
    try
      let name = Unix.getenv "APP_NAME_OFFRE" in
      if name <> "" then name else failwith "Empty APP_NAME is not allowed"
    with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : APP_NAME_OFFRE environment variable is not set, fallback \
             default value"
        in
        "offre.miage.rocks"

  let db_url =
    try Unix.getenv "POSTGRESQL_ADDON_HOST_OFFRE" with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : POSTGRESQL_ADDON_HOST_OFFRE environment variable is not set, \
             fallback to localhost - USE ONLY FOR DEV"
        in
        "127.0.0.1"


  let db_name =
    try Unix.getenv "POSTGRESQL_ADDON_DB_OFFRE" with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : POSTGRESQL_ADDON_DB_OFFRE environment variable is not set \
             fallback to offredb - USE ONLY FOR DEV"
        in
        "offredb"


  let db_port =
    try Unix.getenv "POSTGRESQL_ADDON_PORT_OFFRE" with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : POSTGRESQL_ADDON_PORT_OFFRE environment variable is not set \
             fallback to 5432 - USE ONLY FOR DEV"
        in
        "5432"


  let db_user =
    try Unix.getenv "POSTGRESQL_ADDON_USER_OFFRE" with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : POSTGRESQL_ADDON_USER_OFFRE environment variable is not set \
             fallback to postgres - USE ONLY FOR DEV"
        in
        "postgres"


  let db_password =
    try Unix.getenv "POSTGRESQL_ADDON_PASSWORD_OFFRE" with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : POSTGRESQL_ADDON_PASSWORD environment variable is not \
             set, fallback to empty - USE ONLY FOR DEV"
        in
        ""


  let db_uri =
    Printf.sprintf
      "postgresql://%s:%s@%s:%s/%s"
      db_user
      db_password
      db_url
      db_port
      db_name


  let log_level =
    try
      match Unix.getenv "LEVEL" with
      | "DEBUG" -> Some Logs.Debug
      | "INFO" -> Some Logs.Info
      | "WARN" -> Some Logs.Warning
      | "ERROR" -> Some Logs.Error
      | _ -> None
    with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : LEVEL environment variable is not set, fallback to DEBUG"
        in
        Some Logs.Debug

  let auth_uri = 
    let default_uri = "http://localhost:3000" in
    try Unix.getenv "AUTH_SERVICE_URI" with
    | Not_found ->
        let () =
          prerr_endline @@
            "[WARN] : AUTH_SERVICE_URI environment variable is not \
             set, fallback to "^default_uri^" - USE ONLY FOR DEV"
        in
        default_uri
  let front_uri = 
    let default_uri = "http://localhost:8080" in
    try Unix.getenv "FRONT_URI" with
    | Not_found ->
        let () =
          prerr_endline @@
            "[WARN] : FRONT_URI environment variable is not \
              set, fallback to "^default_uri^" - USE ONLY FOR DEV"
        in
        default_uri       
end

module Database = struct
  let connect () =
    let open Lwt.Infix in
    (let open Environment in
    Uri.of_string db_uri |> Caqti_lwt.connect)
    >>= Caqti_lwt.or_fail
    |> Lwt_main.run
end

module type REST = sig
  type body = [ `Empty
  | `Stream of string Lwt_stream.t
  | `String of string
  | `Strings of string list ] 

  type t = {uri:string;
            ctx:Cohttp_lwt_unix__Net.ctx option;
            requests:(string * 
            (?body:body 
                  -> ?headers:Cohttp.Header.t 
                  -> ?router_param:string list 
                  -> unit
                  -> (Cohttp.Response.t * body) 
            Lwt.t)) list;}

  val value : t
  val authorization : string -> Cohttp.Header.t
  val uri : string
  val ctx : Cohttp_lwt_unix__Net.ctx option
  val request : string  
                -> ?body:body 
                -> ?headers:Cohttp__Header.t 
                -> ?router_param:string list
                -> unit
                -> (Cohttp.Response.t * body) Lwt.t

end

module RestConfiguration : REST = struct
  type body = [ `Empty
  | `Stream of string Lwt_stream.t
  | `String of string
  | `Strings of string list ] 

  type t = {uri:string;
            ctx:Cohttp_lwt_unix__Net.ctx option;
            requests:
            (string * (?body:body -> ?headers:Cohttp.Header.t -> ?router_param:string list -> unit -> (Cohttp.Response.t * body) Lwt.t)) list;}

  let uri_format route ~router_param = 
    let open Environment in List.fold_left (fun acc rp -> acc^"/"^rp) (auth_uri^route) router_param

  let value =
    let open Environment in 
    let open Cohttp_lwt_unix in
    {uri=auth_uri;
    ctx=None;
    requests=[
      ("verify",
          fun ?(body=`Empty) ?(headers=Cohttp.Header.of_list []) ?(router_param=[]) () -> 
            let uri = ( auth_uri^"/verify" |> Uri.of_string ) in Client.post ~body ~headers @@ uri );
      ("root",
          fun ?(body=`Empty) ?(headers=Cohttp.Header.of_list []) ?(router_param=[]) () -> 
              let uri = Uri.of_string @@ auth_uri^"/" in Client.get ~headers @@ uri );
      ("get_member",
          fun ?(body=`Empty) ?(headers=Cohttp.Header.of_list []) ?(router_param=[]) () -> 
              let uri = Uri.of_string @@ uri_format "/member" ~router_param in Client.get ~headers @@ uri);
      ]}
  
  let authorization str = Cohttp.Header.of_list [("Authorization",str)]

  let uri = value.uri

  let ctx = value.ctx

  let request str = 
        let open Environment in 
    let open Cohttp_lwt_unix in
    let default = (fun ?(body=`Empty) ?(headers=Cohttp.Header.of_list []) ?(router_param=[]) () -> Client.get ~headers @@ Uri.of_string @@ auth_uri^"/") in
    Option.value ~default @@ List.fold_left (fun acc (k,v) -> if k = str then Some v else acc ) None value.requests

end

let print_debug str = Environment.log_level |> function | Some Logs.Debug -> print_endline @@ str | _ -> ()
