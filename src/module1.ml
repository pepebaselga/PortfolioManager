open Csv
open Printf
let aapl_csv : string list list = Csv.load "src/AAPL.csv"
let () = Csv.print_readable aapl_csv

(* type date = Use CalendarLib or implement ourselves *)

type candlestick = {
      open_price : float;
      close_price : float;
      high_price : float;
      low_price : float;
    }
type stock = (int * candlestick) list