(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/ *)

module Environment = struct
  let app_name =
    try
      let name = Unix.getenv "APP_NAME" in
      if name <> "" then name else failwith "Empty APP_NAME is not allowed"
    with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : APP_NAME environment variable is not set, fallback \
             default value"
        in
        "auth.miage.rocks"


  let hash_seed =
    try Unix.getenv "SEED" with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : SEED environment variable is not set, fallback default \
             value - USE ONLY FOR DEV"
        in
        "The first ten million years were the worst"


  let random_seed = Random.State.make_self_init ()

  let jwt_secret =
    try Unix.getenv "JWT_SECRET" with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : JWT_SECRET environment variable is not set, fallback \
             default value - USE ONLY FOR DEV"
        in
        "So Long and Thanks For All The Fish"


  let db_url =
    try Unix.getenv "POSTGRESQL_ADDON_HOST" with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : POSTGRESQL_ADDON_HOST environment variable is not set, \
             fallback to localhost - USE ONLY FOR DEV"
        in
        "127.0.0.1"


  let db_name =
    try Unix.getenv "POSTGRESQL_ADDON_DB" with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : POSTGRESQL_ADDON_DB environment variable is not set \
             fallback to offredb - USE ONLY FOR DEV"
        in
        "offredb"


  let db_port =
    try Unix.getenv "POSTGRESQL_ADDON_PORT" with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : POSTGRESQL_ADDON_PORT environment variable is not set \
             fallback to 5432 - USE ONLY FOR DEV"
        in
        "5432"


  let db_user =
    try Unix.getenv "POSTGRESQL_ADDON_USER" with
    | Not_found ->
        let () =
          prerr_endline
            "[WARN] : POSTGRESQL_ADDON_USER environment variable is not set \
             fallback to postgres - USE ONLY FOR DEV"
        in
        "postgres"


  let db_password =
    try Unix.getenv "POSTGRESQL_ADDON_PASSWORD" with
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
end

module Database = struct
  let connect () =
    let open Lwt.Infix in
    (let open Environment in
    Uri.of_string db_uri |> Caqti_lwt.connect)
    >>= Caqti_lwt.or_fail
    |> Lwt_main.run
end
