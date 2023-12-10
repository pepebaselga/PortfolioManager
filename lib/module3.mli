(* module3.ml *)
open Module1
module Index : sig
(* type for an index of stocks *)
type index = (string * Stock.stock) list
(* the folder that the csv files are located in*)
val folder_path : string
(* a function that gets the filenames from folder_path folder *)
val filenames : string -> string list
(* creates a stock type of the csv data *)
val datacreation : string -> Stock.stock
(* loads all csvs in folder_path folder*)
val load_data : Stock.stock list
(* loads specifc csv*)
val load_specific : string -> Stock.stock
end
