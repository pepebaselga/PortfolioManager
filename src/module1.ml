open Csv
open Printf
let aapl_csv : string list list = Csv.load "src/AAPL.csv"
let () = Csv.print_readable aapl_csv

(* type date = Use CalendarLib or implement ourselves *)
module Candlestick = struct
  type cs = {
    openp : float;
    closep : float;
    highp : float;
    lowp : float;
  }
end
module Stock = struct
  type cs = {
    openp : float;
    closep : float;
    highp : float;
    lowp : float;
  }
  type stock = Candlestick.cs list
  let rec csv_to_stock (csv : string list list) =  
    match csv with
      | [] -> []
      | [h] -> [{openp = (float_of_string (List.nth h 1));
                closep = (float_of_string (List.nth h 2));
                highp = (float_of_string (List.nth h 3));
                lowp = (float_of_string (List.nth h 4));}]
      | h::t -> csv_to_stock [h] @ csv_to_stock t


end
type candlestick = {
      openp : float;
      closep : float;
      highp : float;
      lowp : float;
    }

  