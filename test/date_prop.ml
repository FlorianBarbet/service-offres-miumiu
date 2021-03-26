open Offre.Date
open Generator.Service

let date_number_of_month_invariant = 
  QCheck.Test.make
  ~count:1000
  ~name:"Number of month should always be >= 28"
  date_arbitrary
  (fun date_opt -> 
    let open Offre.Date.Month in
    match date_opt with
    | None -> true
    | Some date -> 
      let _ = print_endline @@ Offre.Date.show date 
      and month = date.month |> function 
      | PLAIN m ->  translate_month_type @@ (informations ~years:date.year m).end_at_day
      | NUMERIC a -> a in month >= 28 && month <= 31)

  let date_make_analogy =
    QCheck.Test.make
    ~count:1000
    ~name:"Date:from_triple should be analogy of 'of_string'"
    date_arbitrary
    (fun date_opt -> 
      
      match date_opt with
      | None -> true
      | Some date -> 
        let dd = date.day
        and mm = date.month
        and yyyy = date.year
        in
        let of_string_v = of_string@@show@@date
        and from_triple_v = Option.get@@from_triple@@(dd,mm,yyyy) in

         (show @@ of_string_v) = (show @@ from_triple_v) )

let date_diff_injection =
  QCheck.Test.make
  ~count:1000
  ~name:"Date:d1 and d2 applying on diff to now should be differents"
  two_date_arbitrary
  (fun (d1_opt,d2_opt) -> 
    
    match (d1_opt,d2_opt) with
    | Some d1, Some d2 when (show@@d1 <> show@@d2) -> 
      let now = now() in
      (diff d1 now) <> (diff d2 now) 
    | _ -> true
  )

let date_show_and_of_string_inverse =
  QCheck.Test.make
  ~count:1000
  ~name:"Date:of_string and show should be inverses"
  date_arbitrary
  (fun date_opt -> 
    
    match date_opt with
    | Some date  -> 
     (show@@ date) = show @@of_string @@ show date
    | _ -> true
  )
          
let date_prop_set =
  List.map
    QCheck_alcotest.to_alcotest
    [
    date_number_of_month_invariant
    ; date_make_analogy
    ; date_diff_injection
    ; date_show_and_of_string_inverse
    ]