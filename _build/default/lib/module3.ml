open Csv
open Sys
open Module1
module Index = struct
  type index = (string * Stock.stock) list
  let folder_path = "/Users/pepebaselga/cs3110/PortfolioManager/index/"
  let filenames path =
    try
      let files = Array.to_list (Sys.readdir path) in
      let filtered_files = List.filter (fun f -> f <> "." && f <> "..") files in
      filtered_files
    with
    | Sys_error msg -> failwith ("Error reading directory: " ^ msg)
  let datacreation (csv_name:string) = 
    let csv_name = csv_name in
    let csv_data = Csv.load (folder_path^csv_name) in
    Stock.yf_to_stock csv_name csv_data
  let load_data =
    let filenames = filenames folder_path in
    List.map (fun s -> datacreation (s)) filenames 
  let load_specific (stock:string) = 
    let file = filenames folder_path in 
    let specific =  List.find (fun x -> if x = stock then true else false) file in
    datacreation specific


end