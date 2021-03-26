open QCheck.Gen
open Offre

module type OService = sig
  val date_arbitrary : Offre__Date.t option QCheck.arbitrary
  val two_date_arbitrary : (Offre__Date.t option * Offre__Date.t option) QCheck.arbitrary

end

module Service:OService = struct

  let date_gen = 
    let open Date in
    let open Date.Month in
    let year = QCheck.Gen.int
    and month = oneofl @@ List.map (fun m -> PLAIN m) 
    [ JANUARY
    ; FEBRUARY
    ; MARCH
    ; APRIL
    ; MAY
    ; JUNE
    ; JULY
    ; AUGUST
    ; SEPTEMBER
    ; OCTOBER
    ; NOVEMBER
    ; DECEMBER]
    and day = int_range 1 31 in 
    triple
      day  month year >|= fun triple -> Date.from_triple triple
    let date_print =
      function 
      | None -> "No Date."
      | Some d -> Date.show d
    let date_arbitrary = QCheck.make ~print:date_print date_gen
    let two_date_arbitrary = QCheck.pair date_arbitrary date_arbitrary
end