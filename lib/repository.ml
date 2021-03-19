(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/ *)

module E = Infra.Environment
module D = Domain



module type OFFRE = sig
  type ('res, 'err) query_result =
    ('res, ([> Caqti_error.call_or_retrieve ] as 'err)) result Lwt.t

  val create_contrat :
      sigle : string
      -> description : string
      -> (unit, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result

  val create_entreprise : 
       libelle : string
    -> description : string
    -> numero : string
    -> rue : string
    -> code_postal : int
    -> ville : string
    -> (unit, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result

  val create :
       titre : string
    -> description : string
    -> created_at : Date.t
    -> end_at : Date.t
    -> entreprise : int
    -> contrat : string   
    -> contact: D.Email.t
    -> duree : int option
    -> (unit, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result

  val update :
  
  titre : string
  -> description : string
  -> created_at : Date.t
  -> end_at : Date.t
  -> entreprise : int
  -> contrat : string   
  -> duree : int option
  -> id : int
  -> email : string
    -> (unit, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result

  val delete :
        id:int
        -> email:string
    ->  (unit, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result

  val get_by_id :
        id:int 
      -> (D.Offre.t option, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result
      
  val get_all_by_ville :
      ville : string 
    -> (D.Offre.t list, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result
  val get_villes : 
  unit -> (string list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

  val get_disable_offres :
  unit ->
    ((int * string * string) list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

  val get_entreprises :
  unit ->
    (Offre__Domain.Entreprise.t list, [> Caqti_error.call_or_retrieve ]) result
    Lwt.t

  val get_contrats :
  unit ->
    (Offre__Domain.Contrat.t list, [> Caqti_error.call_or_retrieve ]) result
    Lwt.t

end

module Offre (Connection : Caqti_lwt.CONNECTION) : OFFRE = struct
  let connection : (module Caqti_lwt.CONNECTION) = (module Connection)
  module Email = struct 
    type t = D.Email.t
    let t =
      let encode email = Ok (D.Email.show email) in
      let decode email = D.Email.make email in
      Caqti_type.(custom ~encode ~decode string)
  end

  module Date = struct
    type t = Date.t
    let t = 
      let encode date = let _ = print_endline @@ Date.show date in  Ok (Date.show date) in
      let decode date = let _ = print_endline date in Ok (Date.of_string date ) in
      Caqti_type.(custom ~encode ~decode string)
  end

(**)
  module Entreprise = struct
    type t = D.Entreprise.t
    let t = 
      let encode entreprise = Ok (D.Entreprise.show entreprise)in
      let decode entreprise = Ok (D.Entreprise.of_string entreprise) in
      Caqti_type.(custom ~encode ~decode string)
  end

  module Contrat = struct
    type t = D.Contrat.t
    let t = 
      let encode contrat = Ok (D.Contrat.show contrat)in
      let decode contrat = let _ = print_endline contrat in Ok(D.Contrat.of_string contrat) in
      Caqti_type.(custom ~encode ~decode string)
  end

  type ('res, 'err) query_result =
    ('res, ([> Caqti_error.call_or_retrieve ] as 'err)) result Lwt.t

    let create_contrat = 
      connection
      |> [%rapper
           execute
          {sql|
          INSERT INTO "Contrat" (sigle,description) 
          VALUES  (%string{sigle}, %string{description})
          |sql}]


    let create_entreprise = 
      connection
      |> [%rapper
           execute
          {sql|
          INSERT INTO "Entreprise" ( libelle, description, numero, rue, code_postal, ville) 
          VALUES  (
                   %string{libelle}, 
                   %string{description}, 
                   %string{numero}, 
                   %string{rue}, 
                   %int{code_postal}, 
                   %string{ville})
          |sql}]

    let create =
      connection
      |> [%rapper
           execute
          {sql|
          INSERT INTO "Offre" ( titre, description, created_at, end_at, id_entreprise, type_contrat, contact, duree) 
          VALUES  (
                   %string{titre}, 
                   %string{description}, 
                   %Date{created_at}, 
                   %Date{end_at}, 
                   %int{entreprise}, 
                   %string{contrat}, 
                   %Email{contact}, 
                   %int?{duree})
          |sql}]

    let update = 
      connection
      |> [%rapper
           execute
           {sql|
            UPDATE "Offre" SET titre = %string{titre},
                              description = %string{description}, 
                              created_at = %Date{created_at}, 
                              end_at = %Date{end_at}, 
                              id_entreprise = %int{entreprise}, 
                              type_contrat = %string{contrat},  
                              duree = %int?{duree}
            WHERE id = %int{id} AND contact = %string{email}
           |sql}]

    let delete = 
      connection
      |> [%rapper
        execute
        {sql| UPDATE "Offre" SET active=false WHERE id = %int{id} AND contact = %string{email} |sql}]
    let enable_offre = 
        connection
        |> [%rapper
          execute
          {sql| UPDATE "Offre" SET active=true WHERE id = %int{id} AND end_at > NOW() |sql}]
  
    let get_by_id = 
      connection
      |>
      let open D.Offre in
      [%rapper get_opt
        {sql|
          SELECT offre.@int?{id}, 
                 offre.@string{titre},
                 offre.@string{description}, 
                 offre.@Date{created_at}, 
                 offre.@Date{end_at},
                 
                 @Entreprise{entreprise},
                 @Contrat{contrat},
                 offre.@Email{contact},
                 offre.@int?{duree}
          FROM "Offre" offre
          JOIN "Entreprise" entreprise ON entreprise.id = offre.id_entreprise
          JOIN "Contrat" contrat ON contrat.sigle = offre.type_contrat
          WHERE offre.id = %int{id} AND offre.active AND offre.end_at > NOW()
        |sql} 
        record_out]
         (* fun ~id 
         ~titre 
         ~description 
         ~created_at 
         ~end_at 
         ~entreprise 
         ~contrat 
         ~contact 
         ~duree 
         -> Ok (D.Offre.make ~id ~titre ~description ~created_at ~end_at ~entreprise ~contrat ~contact ?duree ())*)
    let get_all_by_ville = 
    connection
    |>
    let open D.Offre in
    [%rapper get_many
      {sql|
          SELECT offre.@int?{id}, 
                 offre.@string{titre},
                 offre.@string{description}, 
                 offre.@Date{created_at}, 
                 offre.@Date{end_at},
                 
                 @Entreprise{entreprise},
                 @Contrat{contrat},
                 offre.@Email{contact},
                 offre.@int?{duree}
          FROM "Offre" offre
          JOIN "Entreprise" entreprise ON entreprise.id = offre.id_entreprise
          JOIN "Contrat" contrat ON contrat.sigle = offre.type_contrat
          WHERE entreprise.ville = %string{ville} AND offre.active AND offre.end_at > NOW()
        |sql} 
        record_out]
    
    let get_villes = 
      connection
      |>
      [%rapper get_many
        {sql|
            SELECT @string{ville} FROM "Entreprise"
          |sql}]  
          
    let get_disable_offres = 
      connection
      |>
      [%rapper get_many
        {sql|
            SELECT @int{id}, @string{titre}, @string{contact} FROM "Offre" WHERE end_at > NOW() AND not active
          |sql}]

    let get_entreprises =
      connection
      |>
      [%rapper get_many
        {sql|
            SELECT @Entreprise{entreprise} FROM "Entreprise" entreprise
          |sql}]

    let get_contrats = 
      connection
      |>
      [%rapper get_many
        {sql|
            SELECT @Contrat{contrat} FROM "Contrat" contrat
          |sql}]

end


module type MEMBRE = sig
  val verify : headers:Cohttp__Header.t -> (string, string) result Lwt.t
  val get_email_by_id : id:string -> headers:Cohttp__Header.t -> (string, string) result Lwt.t
end

module Membre(R:Infra.REST) : MEMBRE = struct
  open Lwt
  open Cohttp
  open Cohttp_lwt_unix


  let request name ~headers ~body ~router_param = 
    R.request name ~headers ~body ~router_param ()
    >>= fun (resp,body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      let str_body = body |> Cohttp_lwt.Body.to_string |> Lwt_main.run in
      match code with 
      |200 -> Lwt.return @@ Ok (str_body)
      |_ ->   Lwt.return @@ Error (str_body) 

  let verify ~headers = request "verify" ~headers ~body:`Empty ~router_param:[]
    
  let get_email_by_id ~id ~headers = request "get_member" ~headers ~body:`Empty ~router_param:[id]


  let () = (* to debug and verify link with auth service *)
    match E.log_level with
    | Some Logs.Debug ->
          let before_print = R.request "root"  ~body:`Empty ~headers:(Cohttp.Header.of_list []) ()
          >>= fun (resp, body) ->
          let code = resp |> Response.status |> Code.code_of_status in
          Printf.printf "Response code: %d\n" code;
          body |> Cohttp_lwt.Body.to_string >|= fun body ->
          Printf.printf "Body of length: %d\n" (String.length body);
          body in
              let _ = print_endline "\n===== Test Link to Auth =====" in
              let body = Lwt_main.run before_print in
              let _ = print_endline ("Received body -> " ^ body)  in
              print_endline "===== Test Link to Auth =====\n"
    | _ -> ()
      


end