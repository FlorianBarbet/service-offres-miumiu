(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/ *)
open! Util
module D = Domain
module E = Infra.Environment

module Offre (OffreRepository : Repository.OFFRE) = struct
  
  let uuid_traitement next = function
                            | Ok e -> next e 
                            | Error e -> Lwt.return_error "UUID malformed"

  let date_verification next= function
  | (d1,d2) when (Date.compare d1 d2 >= 0) -> next ()
  | _ -> Lwt.return_error "Date 1 is not before Date 2"
  let create_contrat ~sigle ~description =
    let open Lwt in
    let id = D.Uuid.v4_gen E.random_seed () in
    OffreRepository.create_contrat ~id ~sigle ~description
    >>= (function
        | Ok db_result -> Lwt.return_ok ()
        | Error result -> 
          let _ = print_endline (Caqti_error.show result) in Lwt.return_error "Unable to create contrat")

  let create_entreprise ~libelle ~description ~numero ~rue ~code_postal ~ville  =
    let open Lwt in
    let id = D.Uuid.v4_gen E.random_seed () in
    OffreRepository.create_entreprise ~id ~libelle ~description ~numero ~rue ~code_postal ~ville
    >>= (function
        | Ok db_result -> Lwt.return_ok ()
        | Error result -> 
          let _ = print_endline (Caqti_error.show result) in Lwt.return_error @@ "Unable to create entreprise")

  let create ?duree ~titre ~description ~created_at_str ~end_at_str ~entreprise ~contrat ~contact_str ~membre_id  =
  D.Uuid.make membre_id |> uuid_traitement (fun membre_id ->
    D.Uuid.make entreprise |> uuid_traitement (fun entreprise ->
      D.Uuid.make contrat |> uuid_traitement (fun contrat -> 
      let created_at = Date.of_string created_at_str 
      and end_at = Date.of_string end_at_str
      in
      (end_at,created_at) |> date_verification (fun () ->
        match D.Email.make contact_str with
        | Error e -> Lwt.return_error e
        | Ok contact -> 
          let open Lwt in
          let id = D.Uuid.v4_gen E.random_seed () in
          (OffreRepository.create ~id ~titre ~description ~created_at ~end_at ~entreprise ~contrat ~contact ~duree ~membre_id
          >>= (function
            | Ok db_result -> Lwt.return_ok ()
            | Error result -> 
              let _ = print_endline (Caqti_error.show result) in Lwt.return_error "Unable to create offre"))
    ))))

  let get_by_id ~id =
    let open Lwt in
    D.Uuid.make id |> uuid_traitement (fun id ->
      OffreRepository.get_by_id ~id
      >>= (function
      | Ok db_result ->
        let offre = match db_result with | Some v -> D.Offre.to_yojson v | None -> D.empty_yojson in
        let _ = Infra.print_debug @@ Yojson.Safe.show offre in Lwt.return_ok (offre )
      | Error result -> 
        let _ = print_endline (Caqti_error.show result) in Lwt.return_error "An error has occurs")
    )
  let update ?duree ?titre_opt ?description_opt ?created_at_str ?end_at_str ?entreprise_opt ?contrat_opt ~id ~membre_id =
      let open Lwt in
      D.Uuid.make id |> uuid_traitement (fun id ->
        D.Uuid.make membre_id |> uuid_traitement (fun membre_id ->
          OffreRepository.get_by_id ~id 
          >>= (function
          | Error result -> 
              let _ = print_endline (Caqti_error.show result) in Lwt.return_error "An error has occurs"
          | Ok db_result -> 
            match db_result with 
            | None -> Lwt.return_error @@ "Canno't found with id="^D.Uuid.show id
            | Some offre ->  
                let opt_traitement = function 
                                      | (None,d) -> Ok d
                                      | (Some c,_) -> D.Uuid.make c in
                let offre_entreprise_id = offre.entreprise.id
                and offre_contrat_id = offre.contrat.id in
                let created_at =  Option.value ~default:offre.created_at @@ Option.map (fun e -> Date.of_string e) created_at_str 
                and end_at =      Option.value ~default:offre.end_at @@ Option.map (fun e -> Date.of_string e) end_at_str 
                and titre =       Option.value ~default:offre.titre @@ titre_opt 
                and description = Option.value ~default:offre.description @@ description_opt
                and entreprise_r = (entreprise_opt,offre_entreprise_id) |> opt_traitement            
                and contrat_r = (contrat_opt,offre_contrat_id) |> opt_traitement
              in
              match (entreprise_r,contrat_r) with
              | (Error entreprise,Error contrat) -> Lwt.return_error @@ "Erreurs : \n-Entreprise id : "^entreprise^"\n-Contrat id : "^contrat
              | (Error entreprise,_) ->             Lwt.return_error @@ "Entreprise id : "^entreprise
              | (_,Error contrat) ->                Lwt.return_error @@ "Contrat id : "^contrat
              | (Ok entreprise, Ok contrat) ->
                    OffreRepository.update  ~titre ~description ~created_at ~end_at ~entreprise ~contrat ~duree ~id ~membre_id
                    >>= (function
                        | Ok db_result -> Lwt.return_ok ()
                        | Error result -> 
                          let _ = print_endline (Caqti_error.show result) in Lwt.return_error "Unable to update offre")
          )
      ))


  let delete ~id ~membre_id=
    let open Lwt in
    D.Uuid.make id |> uuid_traitement (fun id ->
      D.Uuid.make membre_id |> uuid_traitement (fun membre_id->
        OffreRepository.delete ~id ~membre_id
        >>= (function
        | Ok db_result -> Lwt.return_ok ()
        | Error result -> 
          let _ = print_endline (Caqti_error.show result) in Lwt.return_error "Unable to delete")
      )
    )

  let offre_nb_ent_list (lt:D.Offre.t list) = 
  let open Lwt in
   
      let entreprise_id = List.map (fun (e:D.Offre.t) -> e.entreprise.id ) lt in
      OffreRepository.get_offre_entreprise ~entreprise_id  >>=
      (
      function
      | Ok count ->Lwt.return @@ List.map 
      (fun (o:D.Offre.t) ->
        let find = Option.value ~default:(0,o.entreprise.id) @@ List.find_opt (fun (c,(e:D.Uuid.t))->e=o.entreprise.id) count in
        let extract (c,_) = c in
        (extract find,o)) @@ lt
      | Error result ->
        let _ = print_endline (Caqti_error.show result) in Lwt.return @@ List.map (fun o -> (0,o)) lt
      )
    

  let get_by_ville ~ville =
    let open Lwt in
    
    OffreRepository.get_all_by_ville ~ville
    >>= (function
    | Ok (db_result:D.Offre.t list) ->
      offre_nb_ent_list db_result
      >>=
      (function
      |  lt -> let offre_list = D.Offre.to_yojson_as_list lt in
      let _ = print_endline @@ Yojson.Safe.show @@ D.Offre.to_yojson @@List.nth db_result 0 in Lwt.return_ok (offre_list)
      )
      | Error result ->
      let _ = print_endline (Caqti_error.show result) in Lwt.return_error "An error has occurs")   

  let get_villes () = 
    let open Lwt in
    
    OffreRepository.get_villes ()
    >>= (function
    | Ok db_result -> let lt = D.Entreprise.ville_list db_result in
      let _ = print_endline @@ Yojson.Safe.show @@ lt in Lwt.return_ok (lt)
    | Error result ->
      let _ = print_endline (Caqti_error.show result) in Lwt.return_error "An error has occurs")

  let get_disable_offres ~membre_id =
    let open Lwt in
    D.Uuid.make membre_id |> uuid_traitement (fun membre_id->
      OffreRepository.get_disable_offres ~membre_id
      >>= (function
      | Ok db_result -> 
        let convert = List.map (fun p -> D.Offre.Disable.of_nuplet p) db_result in
        let lt = D.Offre.Disable.to_list_yojson @@ convert  in
        let _ = print_endline @@ Yojson.Safe.show @@ lt in Lwt.return_ok (lt)
      | Error result ->
        let _ = print_endline (Caqti_error.show result) in Lwt.return_error "An error has occurs")
    )

  let get_entreprises () =
    let open Lwt in
    OffreRepository.get_entreprises ()
    >>= (function
    | Ok db_result -> let lt = D.Entreprise.to_yojson_as_list db_result in
      let _ = print_endline @@ Yojson.Safe.show @@ lt in Lwt.return_ok (lt)
    | Error result ->
      let _ = print_endline (Caqti_error.show result) in Lwt.return_error "An error has occurs")
  let get_contrats () =
    let open Lwt in
    OffreRepository.get_contrats ()
    >>= (function
    | Ok db_result -> let lt = D.Contrat.to_yojson_as_list db_result in
      let _ = print_endline @@ Yojson.Safe.show @@ lt in Lwt.return_ok (lt)
    | Error result ->
      let _ = print_endline (Caqti_error.show result) in Lwt.return_error "An error has occurs")
  let enable_offre ~id ~membre_id =
    let open Lwt in
    D.Uuid.make id |> uuid_traitement (fun id ->
      D.Uuid.make membre_id |> uuid_traitement (fun membre_id ->
      OffreRepository.enable_offre ~id ~membre_id
      >>= (function
      | Ok _ -> Lwt.return_ok ()
      | Error result ->
        let _ = print_endline (Caqti_error.show result) in Lwt.return_error@@ "Unable to enable offre with id "^D.Uuid.show id)
    ))

  let get_all_member_offre ~membre_id =
    let open Lwt in
    D.Uuid.make membre_id |> uuid_traitement (fun membre_id ->
        OffreRepository.get_all_member_offre ~membre_id
        >>= (function
        | Ok db_result ->
          let without_counter = List.map (fun (o:D.Offre.t) -> (0,o)) db_result in
          let offre_list =  D.Offre.to_yojson_as_list without_counter in
          let _ = print_endline @@ Yojson.Safe.show @@ D.Offre.to_yojson @@List.nth db_result 0 in Lwt.return_ok (offre_list)
        | Error result ->
          let _ = print_endline (Caqti_error.show result) in Lwt.return_error "An error has occurs") 
    )  
end

module Membre (MembreRepository : Repository.MEMBRE) = struct
  let verify ~headers = 
    let open Lwt in
    
    MembreRepository.verify ~headers 
    >>= (function
    | Ok r -> Lwt.return_ok r
    | Error e -> let _ = print_endline e in Lwt.return_error e )
end
  
