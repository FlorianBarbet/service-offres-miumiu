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

let date_prop_set =
  List.map
    QCheck_alcotest.to_alcotest
    [
    date_number_of_month_invariant
    ;  
    ]