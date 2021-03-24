(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/ *)

module E = Infra.Environment
module D = Domain



module type OFFRE = sig
  type ('res, 'err) query_result =
    ('res, ([> Caqti_error.call_or_retrieve ] as 'err)) result Lwt.t

  val create_contrat :
      id : D.Uuid.t
      -> sigle : string
      -> description : string
      -> (unit, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result

  val create_entreprise : 
       id : D.Uuid.t
    -> libelle : string
    -> description : string
    -> numero : string
    -> rue : string
    -> code_postal : int
    -> ville : string
    -> (unit, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result

  val create :
    id : D.Uuid.t
    -> titre : string
    -> description : string
    -> created_at : Date.t
    -> end_at : Date.t
    -> entreprise : D.Uuid.t
    -> contrat : D.Uuid.t   
    -> contact: D.Email.t
    -> duree : int option
    -> membre_id : D.Uuid.t
    -> (unit, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result

  val update :
  titre : string
  -> description : string
  -> created_at : Date.t
  -> end_at : Date.t
  -> entreprise : D.Uuid.t
  -> contrat : D.Uuid.t   
  -> duree : int option
  -> id : D.Uuid.t
  -> membre_id:D.Uuid.t
    -> (unit, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result

  val delete :
        id:D.Uuid.t
        -> membre_id:D.Uuid.t
    ->  (unit, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result

  val get_by_id :
        id:D.Uuid.t
      -> (D.Offre.t option, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result
      
  val get_all_by_ville :
      ville : string 
    -> (D.Offre.t list, ([> Caqti_error.call_or_retrieve ] as 'err)) query_result
  val get_villes : 
  unit -> (string list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

  val get_disable_offres :
  unit ->
    ((D.Uuid.t * string * D.Uuid.t) list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

  val get_entreprises :
  unit ->
    (Offre__Domain.Entreprise.t list, [> Caqti_error.call_or_retrieve ]) result
    Lwt.t

  val get_contrats :
  unit ->
    (Offre__Domain.Contrat.t list, [> Caqti_error.call_or_retrieve ]) result
    Lwt.t

  val enable_offre : 
  id:D.Uuid.t ->
    membre_id:D.Uuid.t -> (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

  val get_all_member_offre : 
  membre_id:Offre__Domain.Uuid.t ->
(Offre__Domain.Offre.t list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

  val get_offre_entreprise : 
  entreprise_id:Offre__Domain.Uuid.t list->
    ((int*D.Uuid.t) list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

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

  module Uuid = struct
    type t = D.Uuid.t

    let t =
      let encode uuid = Ok (D.Uuid.show uuid) in
      let decode uuid = D.Uuid.make uuid in
      Caqti_type.(custom ~encode ~decode string)
  end


  type ('res, 'err) query_result =
    ('res, ([> Caqti_error.call_or_retrieve ] as 'err)) result Lwt.t

    let create_contrat = 
      connection
      |> [%rapper
           execute
          {sql|
          INSERT INTO "Contrat" (id,sigle,description) 
          VALUES  (%Uuid{id}, %string{sigle}, %string{description})
          |sql}]


    let create_entreprise = 
      connection
      |> [%rapper
           execute
          {sql|
          INSERT INTO "Entreprise" (id, libelle, description, numero, rue, code_postal, ville) 
          VALUES  (
                   %Uuid{id},
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
          INSERT INTO "Offre" (id, titre, description, created_at, end_at, id_entreprise, id_contrat, contact, duree, membre_id) 
          VALUES  (%Uuid{id},
                   %string{titre}, 
                   %string{description}, 
                   %Date{created_at}, 
                   %Date{end_at}, 
                   %Uuid{entreprise}, 
                   %Uuid{contrat}, 
                   %Email{contact}, 
                   %int?{duree},
                   %Uuid{membre_id})
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
                              id_entreprise = %Uuid{entreprise}, 
                              id_contrat = %Uuid{contrat},  
                              duree = %int?{duree}
            WHERE id = %Uuid{id} AND membre_id = %Uuid{membre_id}
           |sql}]

    let delete = 
      connection
      |> [%rapper
        execute
        {sql| UPDATE "Offre" SET active=false WHERE id = %Uuid{id} AND membre_id = %Uuid{membre_id} |sql}]
    let enable_offre = 
        connection
        |> [%rapper
          execute
          {sql| UPDATE "Offre" SET active=true WHERE id = %Uuid{id} AND membre_id = %Uuid{membre_id} AND end_at > NOW() |sql}]
  
    let get_by_id = 
      connection
      |>
      let open D.Offre in
      [%rapper get_opt
        {sql|
          SELECT offre.@Uuid{id}, 
                 offre.@string{titre},
                 offre.@string{description}, 
                 offre.@Date{created_at}, 
                 offre.@Date{end_at},
                 
                 @Entreprise{entreprise},
                 @Contrat{contrat},
                 offre.@Email{contact},
                 offre.@int?{duree},
                 offre.@Uuid{membre_id}
          FROM "Offre" offre
          JOIN "Entreprise" entreprise ON entreprise.id = offre.id_entreprise
          JOIN "Contrat" contrat ON contrat.id = offre.id_contrat
          WHERE offre.id = %Uuid{id} AND offre.active AND offre.end_at > NOW()
        |sql} 
        record_out]
    let get_all_by_ville = 
    connection
    |>
    let open D.Offre in
    [%rapper get_many
      {sql|
          SELECT offre.@Uuid{id}, 
                 offre.@string{titre},
                 offre.@string{description}, 
                 offre.@Date{created_at}, 
                 offre.@Date{end_at},
                 
                 @Entreprise{entreprise},
                 @Contrat{contrat},
                 offre.@Email{contact},
                 offre.@int?{duree},
                 offre.@Uuid{membre_id}
          FROM "Offre" offre
          JOIN "Entreprise" entreprise ON entreprise.id = offre.id_entreprise
          JOIN "Contrat" contrat ON contrat.id = offre.id_contrat
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
            SELECT @Uuid{id}, @string{titre}, @Uuid{membre_id} FROM "Offre" WHERE end_at > NOW() AND not active
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

    let get_all_member_offre = 
      connection
      |>
      let open D.Offre in
      [%rapper get_many
        {sql|
            SELECT offre.@Uuid{id}, 
                   offre.@string{titre},
                   offre.@string{description}, 
                   offre.@Date{created_at}, 
                   offre.@Date{end_at},
                   
                   @Entreprise{entreprise},
                   @Contrat{contrat},
                   offre.@Email{contact},
                   offre.@int?{duree},
                   offre.@Uuid{membre_id}
            FROM "Offre" offre
            JOIN "Entreprise" entreprise ON entreprise.id = offre.id_entreprise
            JOIN "Contrat" contrat ON contrat.id = offre.id_contrat
            WHERE offre.membre_id = %Uuid{membre_id} AND offre.active AND offre.end_at > NOW()
          |sql} 
          record_out]

    let get_offre_entreprise =
      connection
      |>
      let open D.Offre in
      [%rapper get_many
        {sql|
            SELECT @int{count(*)}, entreprise.@Uuid{id}
            FROM "Offre" offre
            JOIN "Entreprise" entreprise ON entreprise.id = offre.id_entreprise
            WHERE offre.active AND offre.end_at > NOW() AND entreprise.id IN (%list{%Uuid{entreprise_id}}) 
            GROUP BY entreprise.id
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
      body |> Cohttp_lwt.Body.to_string 
      >>= function str ->         
      match code with 
      |200 ->  Lwt.return_ok  str
      |_ ->   Lwt.return_error  str

  let verify ~headers = 
    let body_str = {| {"jwt":"|}^(Option.value ~default:"" @@ Cohttp.Header.get headers "Authorization")^{|"} |} in
    
    request "verify" ~headers ~body:(`String body_str)  ~router_param:[]
    
  let get_email_by_id ~id ~headers = request "get_member" ~headers ~body:`Empty ~router_param:[id]


  let () =  (*to debug and verify link with auth service *)
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