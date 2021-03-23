(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/ *)
   
open Opium
open Offre
(** Build the Opium app *)
let app = App.empty 
|> App.port 3030 
|> App.cmd_name Infra.Environment.app_name 
|> App.middleware (Middleware.logger)
|> App.middleware (Middleware.allow_cors ~origins:["*"] ~expose:[ "*"]  ())
|> Api.add_routes


(** Run the application *)
let () =
  Api.set_logger ();
  match App.run_command' app with
  | `Ok (app : unit Lwt.t) -> Lwt_main.run app
  | `Error -> exit 1
  | `Not_running -> exit 0
