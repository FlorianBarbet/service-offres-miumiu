open Unix

module Month = struct
  type t = 
  | JANUARY
  | FEBRUARY
  | MARCH
  | APRIL
  | MAY
  | JUNE
  | JULY
  | AUGUST
  | SEPTEMBER
  | OCTOBER
  | NOVEMBER
  | DECEMBER [@@deriving show, yojson]

  type month_type = 
  | PLEIN 
  | CREUX 
  | BIS of int

  let translate_month_type = function
  | PLEIN -> 31 
  | CREUX -> 30
  | BIS year when (0 = year mod  4  && 0 <> year mod  100) || 0 = year mod 400 -> 29
  | BIS _ -> 28 
  
  type infos = {num : int; str : string; end_at_day : month_type}
  let informations ?(years=1970) = function 
  | JANUARY   -> {num=1;  str="Jan."; end_at_day=PLEIN}
  | FEBRUARY  -> {num=2;  str="Feb."; end_at_day=BIS years}
  | MARCH     -> {num=3;  str="Mar."; end_at_day=PLEIN}
  | APRIL     -> {num=4;  str="Apr."; end_at_day=CREUX}
  | MAY       -> {num=5;  str="May."; end_at_day=PLEIN}
  | JUNE      -> {num=6;  str="Jun."; end_at_day=CREUX}
  | JULY      -> {num=7;  str="Jul."; end_at_day=PLEIN}
  | AUGUST    -> {num=8;  str="Aug."; end_at_day=PLEIN}
  | SEPTEMBER -> {num=9;  str="Sep."; end_at_day=CREUX}
  | OCTOBER   -> {num=10; str="Oct."; end_at_day=PLEIN}
  | NOVEMBER  -> {num=11; str="Nov."; end_at_day=CREUX}
  | DECEMBER  -> {num=12; str="Dec."; end_at_day=PLEIN}

  let int_of_month_option = Option.map (fun e -> Option.(informations e).num )
  let int_of_month n = (informations n).num 
  let string_of_month_option = Option.map (fun e -> (informations e).str)
  let string_of_month str = (informations str).str

  let month_of_int =  function 
  | 1 -> Some JANUARY
  | 2 -> Some FEBRUARY
  | 3 -> Some MARCH
  | 4 -> Some APRIL
  | 5 -> Some MAY
  | 6 -> Some JUNE
  | 7 -> Some JULY
  | 8 -> Some AUGUST
  | 9 -> Some SEPTEMBER
  | 10 -> Some OCTOBER
  | 11 -> Some NOVEMBER
  | 12 -> Some DECEMBER
  | _ -> None

  let compare m1 m2 = 
    compare (int_of_month m1) (int_of_month m2)

end


type month_types = 
| PLAIN of Month.t
| NUMERIC of int [@@deriving show, yojson]

let month_translator = function
  | PLAIN m -> Some m
  | NUMERIC i -> Month.month_of_int i

let int_of_month_types = function
  | NUMERIC i ->  i
  | PLAIN m -> Month.int_of_month m
  

type t = {
  day : int;
  month : month_types;
  year : int
}[@@deriving show, yojson]
let day_validator date  = match (date.month |> month_translator) with
| Some m ->  ( m |> Month.informations ~years:date.year ).end_at_day |> Month.translate_month_type >= date.day
| None -> false

let list_of_date l = 
  let  day   = List.nth l 2
  and  month = List.nth l 1
  and  year  = List.nth l 0
    in {day;month = NUMERIC month ;year}
    
let make date = if day_validator date then Some date else None

let of_string str = (*let _ = print_endline str in*) str |> String.split_on_char '-' |> List.map int_of_string |> list_of_date

let string_of_date date = 
  let dd = string_of_int @@ date.day
  and mm = string_of_int @@ int_of_month_types @@ date.month
  and yyyy = string_of_int @@ date.year in
  let mm = match String.length mm with 
            | e when e < 2 -> "0"^mm
            | _ -> mm in
  let _ = print_endline @@ "INTO DATE MODULE"^yyyy^"-"^mm^"-"^dd in
    yyyy^"-"^mm^"-"^dd
let show = string_of_date
let pp ppf date = Format.pp_print_string ppf (show date)

let now_format () =
  let current_millis = Unix.time () 
  and one_day_by_ms = 1000. *.3600. *. 24.
  and average_month_by_day = 30.4375
  and one_year_by_day = 365.2421898 in
  let y =  (( current_millis /. one_day_by_ms) /. one_year_by_day) in
  let current_millis_after_year = ( y *. one_year_by_day *. one_day_by_ms) -. current_millis in
  let month = current_millis_after_year /. one_day_by_ms /. average_month_by_day in
  let current_millis_after_month = (month *. average_month_by_day *. one_day_by_ms) -. current_millis_after_year in
  let day = current_millis_after_month /. one_day_by_ms in
  {day = int_of_float day;month = NUMERIC (int_of_float month);year = int_of_float y}


let now () = make @@ now_format ()

let compare date1 date2 = 
  match compare date1.year date2.year with
  | 0 -> (match compare (int_of_month_types date1.month) (int_of_month_types date2.month) with
         | 0 ->  compare date1.day date2.day  
         | compare_month -> compare_month)
  | compare_year -> compare_year

module WeekDay = struct 
  type t = 
  | MONDAY 
  | TUESDAY
  | WEDNESDAY
  | THURSDAY 
  | FRIDAY 
  | SATURDAY 
  | SUNDAY 

  let determine date = 
    let num = int_of_month_types @@ date.month in
    let c = (14-num)/12 in 
    let y = date.year - c and m = num + 12 * c - 2 in
    let j = (date.day + y +y/4 -y/100+y/400+(31*m)/12) mod 7 in (*formula thanks to wikipedia*)
    match j with
    |1 -> MONDAY
    |2 -> TUESDAY
    |3 -> WEDNESDAY
    |4 -> THURSDAY 
    |5 -> FRIDAY 
    |6 -> SATURDAY
    |_ -> SUNDAY (* mod 7 => 0 up to 6 so Sunday is for 0*)

  let string_of_week_day = function
  | MONDAY    -> "Monday"
  | TUESDAY   -> "Tuesday"
  | WEDNESDAY -> "Wednesday"
  | THURSDAY  -> "Thursday"
  | FRIDAY    -> "Friday"
  | SATURDAY  -> "Saturday"
  | SUNDAY    -> "Sunday"

end