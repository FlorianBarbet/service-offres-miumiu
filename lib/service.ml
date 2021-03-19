(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/ *)
open! Util
module D = Domain
module E = Infra.Environment

module Offre (OffreRepository : Repository.OFFRE) = struct
  
  let create_contrat ~sigle ~description =
    let open Lwt in
    OffreRepository.create_contrat ~sigle ~description
    >>= (function
        | Ok db_result -> Lwt.return_ok ()
        | Error result -> 
          let _ = print_endline (Caqti_error.show result) in Lwt.return_error "Unable to create contrat")

  let create_entreprise ?id ~libelle ~description ~numero ~rue ~code_postal ~ville  =
    let open Lwt in
    OffreRepository.create_entreprise ~libelle ~description ~numero ~rue ~code_postal ~ville
    >>= (function
        | Ok db_result -> Lwt.return_ok ()
        | Error result -> 
          let _ = print_endline (Caqti_error.show result) in Lwt.return_error "Unable to create entreprise")

  let create ?duree ~titre ~description ~created_at_str ~end_at_str ~entreprise ~contrat ~contact_str   =
    let created_at = Date.of_string created_at_str 
    and end_at = Date.of_string end_at_str
   in
      match D.Email.make contact_str with
      | Error e -> Lwt.return_error e
      | Ok contact -> 
        let open Lwt in

        (OffreRepository.create ~titre ~description ~created_at ~end_at ~entreprise ~contrat ~contact ~duree
        >>= (function
          | Ok db_result -> Lwt.return_ok ()
          | Error result -> 
            let _ = print_endline (Caqti_error.show result) in Lwt.return_error "Unable to create offre"))

  let get_by_id ~id =
    let open Lwt in
    OffreRepository.get_by_id ~id
    >>= (function
    | Ok db_result ->
      let offre = match db_result with | Some v -> D.Offre.to_yojson v | None -> D.empty_yojson in
      (*let _ = print_endline @@ Yojson.Safe.show offre in*) Lwt.return_ok (offre )
    | Error result -> 
      let _ = print_endline (Caqti_error.show result) in Lwt.return_error "An error has occurs")

  let update ?duree ?titre_opt ?description_opt ?created_at_str ?end_at_str ?entreprise_opt ?contrat_opt ~id ~email =
      let open Lwt in
      OffreRepository.get_by_id ~id 
      >>= (function
      | Error result -> 
          let _ = print_endline (Caqti_error.show result) in Lwt.return_error "An error has occurs"
      | Ok db_result -> 
        match db_result with 
        | Some offre ->  
            let offre_entreprise_id = Option.get offre.entreprise.id
            and offre_contrat_sigle = offre.contrat.sigle in
            let created_at = Option.value ~default:offre.created_at @@ Option.map (fun e -> Date.of_string e) created_at_str 
            and end_at = Option.value ~default:offre.end_at @@ Option.map (fun e -> Date.of_string e) end_at_str 
            and titre = Option.value titre_opt ~default:offre.titre 
            and description = Option.value ~default:offre.description description_opt
            and entreprise = Option.value ~default:offre_entreprise_id  entreprise_opt
            and contrat = Option.value ~default:offre_contrat_sigle contrat_opt
          in
          OffreRepository.update  ~titre ~description ~created_at ~end_at ~entreprise ~contrat ~duree ~id ~email
          >>= (function
              | Ok db_result -> Lwt.return_ok ()
              | Error result -> 
                let _ = print_endline (Caqti_error.show result) in Lwt.return_error "Unable to update offre")
        | None -> Lwt.return_error @@ "Canno't found with id="^string_of_int id
      )


  let delete ~id ~email=
    let open Lwt in
      OffreRepository.delete ~id ~email
      >>= (function
      | Ok db_result -> Lwt.return_ok ()
      | Error result -> 
        let _ = print_endline (Caqti_error.show result) in Lwt.return_error "Unable to delete")

  let get_by_ville ~ville =
    let open Lwt in
    
    OffreRepository.get_all_by_ville ~ville
    >>= (function
    | Ok db_result ->
      let offre_list =  D.Offre.to_yojson_as_list db_result in
      let _ = print_endline @@ Yojson.Safe.show @@ D.Offre.to_yojson @@List.nth db_result 0 in Lwt.return_ok (offre_list)
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

  let get_disable_offres () =
    let open Lwt in
    
    OffreRepository.get_disable_offres ()
    >>= (function
    | Ok db_result -> 
      let convert = List.map (fun p -> D.Offre.Disable.of_pair p) db_result in
      let lt = D.Offre.Disable.to_list_yojson @@ convert  in
      let _ = print_endline @@ Yojson.Safe.show @@ lt in Lwt.return_ok (lt)
    | Error result ->
      let _ = print_endline (Caqti_error.show result) in Lwt.return_error "An error has occurs")

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
  let enable_offre ~id ~email =
    let open Lwt in
    OffreRepository.enable_offre ~id ~email
    >>= (function
    | Ok _ -> Lwt.return_ok ()
    | Error result ->
      let _ = print_endline (Caqti_error.show result) in Lwt.return_error@@ "Unable to enable offre with id "^string_of_int id)
end

module Membre (MembreRepository : Repository.MEMBRE) = struct
  let verify ~headers = 
    let open Lwt in
    MembreRepository.verify ~headers 
    >>= (function
    | Ok r -> Lwt.return_ok r
    | Error e -> let _ = print_endline e in Lwt.return_error e )

  let get_email_by_id ~id ~headers =
    let open Lwt in
    MembreRepository.verify ~headers 
    >>= (function
    | Ok r -> Lwt.return_ok r
    | Error e -> let _ = print_endline e in Lwt.return_error e )
end
  
