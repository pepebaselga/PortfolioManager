open Csv
open Str
module Date = struct
  type date = {month : int; day :int; year :int}
  let check_date (d: date): bool =
    let check_helper (d: date) =
      let long day = (day > 1) && (day <= 31) in
      let short day = (day > 1) && (day <= 30) in
      match d.month with
      | 2 -> (d.day > 1) && (d.day <= 29)
      | 11 -> short d.day
      | 9 -> short d.day
      | 12 -> long d.day
      | 10 -> long d.day
      | i -> if (i mod 2 = 1) then long d.day else short d.day in
    match d.year with
    |x -> if (x < 2000)||(x > 2023) then false else check_helper d
  let make_date month day year =
      { month = month; day = day; year = year }
  let of_string (s : string) = let (lst : string list) = 
    Str.split (Str.regexp "/") s in 
    {month = int_of_string (List.nth lst 0); 
    day = int_of_string (List.nth lst 1); 
    year = int_of_string (List.nth lst 2)}

  let to_string (d : date) =
    (string_of_int d.month) ^ "/" ^ (string_of_int d.day) ^ "/" ^ (string_of_int d.year)
  let pp_date (d: date): string =
    Printf.sprintf "Date: %i/%i/%i" d.month d.day d.year;
end
module Candlestick = struct
  type cs = {
    openp : float;
    highp : float;
    lowp : float;
    closep : float;
  }
  type cs_color = 
  | Green
  | Red
  | Grey

  let make_cs o h l c = 
  {openp = o;
  highp = h;
  lowp = l;
  closep = c;}

  let to_string (cs : cs) =
    (string_of_float cs.openp) ^ " | " ^ (string_of_float cs.highp) ^ " | " ^ (string_of_float cs.lowp) ^ " | " ^ (string_of_float cs.closep)
  let eod_change (cs : cs) : float =
    cs.closep -. cs.openp

  let get_color (cs : cs) : cs_color =
    if eod_change cs > 0. then Green 
    else if eod_change cs < 0. then Red 
    else Grey
end
module Stock = struct
  type stock = (Date.date * Candlestick.cs) list
  
let rec yf_helper (csv : string list list) : stock =
  match csv with
  | [] -> []
  | [h] -> [Date.of_string(List.nth h 0), 
          Candlestick.make_cs 
            (float_of_string (List.nth h 1))
            (float_of_string (List.nth h 2))
            (float_of_string (List.nth h 3))
            (float_of_string (List.nth h 4))]
  | h::t -> yf_helper [h] @ yf_helper t
let yf_to_stock  (csv : string list list) =
  match csv with
  | h::t -> yf_helper t
  | [] -> yf_helper []
let rec to_string (st : stock) =
  match st with
  | [] -> ""
  | [d,cs] -> Date.to_string d ^ ": " ^ Candlestick.to_string cs
  | h :: t -> to_string [h] ^ "\n" ^ to_string t 
(* let get_diff stock day1 day2 =
  List.fold_left (fun x ->) *)
  (* match stock with
  | [] -> raise (failwith "No data for this date")
  | [h] ->  *)
end