open Csv
open Str
module Date = struct
  type date = {month : int; day : int; year : int}
  type comp = LT | GT | EQ
  let check_date (d: date): bool =
    let check_helper (d: date) =
      let long day = (day >= 1) && (day <= 31) in
      let short day = (day >= 1) && (day <= 30) in
      match d.month with
      | 2 -> if (d.year mod 4) = 0 then (d.day >= 1) && (d.day <= 29) 
      else (d.day >= 1) && (d.day <= 28)
      | 11 -> short d.day
      | 9 -> short d.day
      | 12 -> long d.day
      | 10 -> long d.day
      | 8 -> long d.day
      | i -> if (i mod 2 = 1) then long d.day else short d.day in
    match d.year with
    |x -> if (x < 2000)||(x > 2023) then false else check_helper d
    let make_date month day year =
      { month = month; day = day; year = year }
      let ( <=^ ) (day1 : date) (day2: date) : bool =
      match day1.year <= day2.year with
      | false -> false
      | true -> if day1.year < day2.year then true else
        match day1.month <= day2.month with
        | false -> false
        | true -> if day1.month < day2.month then true else
          match day1.day <= day2.day with
          | false -> false
          | true -> true
    let ( >=^ ) (day1 : date) (day2: date) : bool =
      match day1.year >= day2.year with
      | false -> false
      | true -> if day1.year > day2.year then true else
        match day1.month >= day2.month with
        | false -> false
        | true -> if day1.month > day2.month then true else
          match day1.day >= day2.day with
          | false -> false
          | true -> true
    let ( =^ ) (day1 : date) (day2 : date) : bool =
      (day1 <=^ day2) && (day1 >=^ day2)
    let ( <^ ) (day1 : date) (day2: date) : bool =
      (day1 <=^ day2) && (not (day1 =^ day2))
    let ( >^ ) (day1 : date) (day2: date) : bool =
      (day1 >=^ day2) && (not (day1 =^ day2))
    let compare (day1 : date) (day2 : date) =
      let lt = day1 <^ day2 in
      let gt = day1 >^ day2 in
      if lt then LT else if gt then GT else EQ
    let date_comp_helper (str : string) =
      match str with
      | "LT" -> LT
      | "GT" -> GT
      | "EQ" -> EQ
      | _ -> raise (failwith "inexhaustive pattern matching")
  let of_string (s : string) = let (lst : string list) =   
    Str.split (Str.regexp "/") s in let year = (int_of_string (List.nth lst 2)) in
    let year1 = if year < 1000 then year + 2000 else year in
    {month = int_of_string (List.nth lst 0); 
    day = int_of_string (List.nth lst 1); 
    year = year1}
  let to_string (d : date) =
    if (check_date d = false) then raise (failwith "invalid date") else
    let year = if d.year < 1000 then d.year+2000 else d.year in
    (string_of_int d.month) ^ "/" ^ (string_of_int d.day) ^ "/" ^ (string_of_int year)
  let pp_date (d: date): string =
    if (check_date d = false) then raise (failwith "invalid date") else
    Printf.sprintf "%i/%i/%i" d.month d.day d.year;
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
  type stock = (string * (Date.date * Candlestick.cs) list)
  
let rec yf_helper (csv : string list list) : (Date.date * Candlestick.cs) list =
  match csv with
  | [] -> []
  | [h] -> [Date.of_string(List.nth h 0), 
          Candlestick.make_cs 
            (float_of_string (List.nth h 1))
            (float_of_string (List.nth h 2))
            (float_of_string (List.nth h 3))
            (float_of_string (List.nth h 4))]
  | h::t -> yf_helper [h] @ yf_helper t
let yf_to_stock  (ticker: string) (csv : string list list): stock =
  let candle = 
  match csv with
  | h::t -> yf_helper t
  | [] -> yf_helper [] in 
  (ticker, candle)

let rec to_string_helper (candle : (Date.date * Candlestick.cs) list) = 
    match candle with
    | [] -> ""
    | [d,cs] -> Date.to_string d ^ ": " ^ Candlestick.to_string cs
    | h :: t -> to_string_helper [h] ^ "\n" ^ to_string_helper t 
let rec to_string (st : stock): string =
  let name,candle = st in
  let p1 = to_string_helper candle in
  name^": "^p1

let rec find_date_helper (candle : (Date.date * Candlestick.cs) list) (day1 : Date.date) =
  match candle with
  | [] -> let printt = "No data for this date: "^Date.pp_date day1 in 
  raise (failwith printt)
  | (d,cs)::t -> if Date.compare day1 d = EQ then cs else find_date_helper t day1

let  find_date (stock : stock) (day1 : Date.date) =
  let name,candle = stock in
  let p1 = find_date_helper candle day1 in 
  (name, p1)

  let rec get_dollar_diff (stock : stock) day1 day2 =
  let _ = assert (Date.check_date day1);
          assert (Date.check_date day2); 
          assert ((Date.compare day1 day2) = LT);
  in 
  let _,d1 = find_date stock day1 in 
  let _,d2 = find_date stock day2 in
  d2.closep -. d1.closep
  let rec get_percent_diff (stock : stock) day1 day2 =
    let _ = assert (Date.check_date day1);
            assert (Date.check_date day2);
            assert ((Date.compare day1 day2) = LT);
    in
    let _,d1 = find_date stock day1 in 
    let _,d2 = find_date stock day2 in
    (d2.closep -. d1.closep) /. d1.closep
end

