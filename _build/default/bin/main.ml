open PortfolioManager
open Module1
open Module2
exception Quit
type portfolio = Asset.portfolio
type stock = Stock.stock
type cs = Candlestick.cs
let empty_portfolio : portfolio list = [[]]
let init_portfolio : portfolio list = empty_portfolio
let find_asset stockname =
  let stock_of_asset = Asset.find_asset stockname in
  stock_of_asset
let get_color_to_string (candle : cs) =
  let color = Candlestick.get_color candle in
  match color with
  | Red -> "Red"
  | Green -> "Green"
  | Grey -> "Grey"
let make_date (date : string) =
  (* let date_split = String.split_on_char '/' date in
  let inted_date_list = List.map int_of_string date_split in
  let final_date =
    match inted_date_list with
    | month :: day :: year :: [] -> Date.make_date month day year
    | _ -> failwith "invalid date"
  in final_date *)
  Date.of_string date

  let rec make_string_of_list lst =
    match lst with
    | [] -> ""
    | one :: two :: [] -> one ^ "; " ^ two
    | h :: t -> h ^ "; " ^ make_string_of_list t
let rec make_candle_list csdlist =
  match csdlist with
  | [] -> []
  | (_, candle) :: t -> candle :: make_candle_list t
let consolidate_sector (name, sector) =
  name ^ " : " ^ sector
let consolidate_string_float (string, float) =
  string ^ " : " ^ string_of_float float
(* evaluate user input *)
let rec eval (current_portfolio : portfolio list) ?(pi = "0") input: (portfolio list * string) =
  match input |> String.split_on_char ' ' |> List.filter (( <> ) "") with
  | ["#makeportfolio"] -> make_portfolio current_portfolio
  | [ "#quit" ] -> raise Quit
  | [ "#portfolio" ; pi] -> portfolio current_portfolio pi
  | [ "#stock"; "stockname"; p;date ] -> stock current_portfolio pi "stockname" date
  | [ "#candle"; name ; pi] -> candle current_portfolio pi name
  | [ "#random"; pi] -> random current_portfolio pi
  | [ "#buy"; pi; name; quantity; price; date_purchased; sector; ] ->
    add current_portfolio pi name quantity price date_purchased sector
  | [ "#colors"; name ; pi ] -> colors current_portfolio pi name
  | [ "#eods"; name; pi ] -> eods current_portfolio pi name
  | [ "#value" ; pi ] -> value_report current_portfolio pi 
  | [ "#sector"; pi ] -> sector_report current_portfolio pi 
  | [ "#percent" ; pi;date ] -> percent_report current_portfolio pi date
  | [ "#dollar" ; pi;date ] -> dollar_report current_portfolio pi date
  | [ "#value"; name; date ; pi ] -> particular_value current_portfolio pi name date
  | [ "#quantity"; name; date ] -> particular_quantity current_portfolio pi name date
  | [ "#bestpercent" ; pi; date] -> best_percent current_portfolio pi date
  | [ "#worstpercent"; pi; date ] -> worst_percent current_portfolio pi date
  | [ "#bestdollar"; pi ; date] -> best_dollar current_portfolio pi date
  | [ "#worstdollar"; pi ; date] -> worst_dollar current_portfolio pi date
  | [ "#portfoliovalue"; pi; date ] -> portfolio_value current_portfolio pi date
  | [ "#totalquantity"; asset; date; pi ] -> total_portfolio_quantity asset date current_portfolio pi
  | [ "#totaldollar"; asset; date ; pi] -> total_dollar_change asset date current_portfolio pi
  | [ "#totalpercent"; asset; date; pi ] -> total_percent_change asset date current_portfolio pi
  | [ "#buy?"; asset; date; pi ] -> buy asset date current_portfolio pi
  | [ "#sell?"; asset; date; pi ] -> sell asset date current_portfolio pi
  | _ -> eval current_portfolio "Incorrect inputs entered. Try again. Look to the instructions for more clear guidance."
and make_portfolio portfoliolist = 
  let pi = (List.length (portfoliolist@[[]])) - 1 in
  let added_string_activity =
    "You created a new empty portfolio, and its reference number is: " ^ 
    string_of_int(pi) ^". Use this value to access this particular portfolio." 
  in
  ((portfoliolist@[[]]),added_string_activity )
and random portfoliolist pi =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in 
  (* choose random asset in stock and give report (eods and candlestick) *)
  let random_asset = List.nth current_portfolio 0 in
  (portfoliolist, Asset.asset_to_string random_asset)
and add portfoliolist pi name quantity price date sector =
  if int_of_string pi < (List.length portfoliolist) then 
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  (* new asset :: current_portfolio *)
  (* using date, make asset, add to portfolio *)
  (* "You added _ on */**/**** to your portfolio" *)
  let date_split = String.split_on_char '/' date in
  let inted_date_list = List.map int_of_string date_split in
  let final_date =
    match inted_date_list with
    | month :: day :: year :: [] -> Date.make_date month day year
    | _ -> failwith "invalid date"
  in let asset = Asset.make_asset
  name (float_of_string quantity) (float_of_string price) final_date sector
  in let added_string_activity =
  "You bought " ^ quantity ^ " shares of " ^ name ^ " for " ^ price ^
  " in portfolio "^(pi)^" on " ^ (Date.to_string final_date) ^ "." in
  (List.map (fun x -> if x == current_portfolio then asset::x else x) portfoliolist, added_string_activity)
  else 
    (portfoliolist, "Portfolio index is out of range, the max is: "^string_of_int((List.length portfoliolist) - 1)^".")

 
and candle portfoliolist pi stockname =
  let (_, csdlist) = Asset.find_asset stockname in
  let candle_list = make_candle_list csdlist in
  let candle_string = List.map Candlestick.to_string candle_list in
  let stringed_list = make_string_of_list candle_string in
  let final_string = "The list of candlesticks for stock "
  ^ stockname ^ " is: " ^ stringed_list in
  (portfoliolist, final_string)
and eods portfoliolist pi stockname =
  let (_, csdlist) = Asset.find_asset stockname in
  let candle_list = make_candle_list csdlist in
  let eod_list = List.map Candlestick.eod_change candle_list in
  let stringed_list = List.map string_of_float eod_list in
  let final_list = make_string_of_list stringed_list in
  let final_string = "The list of the colors for the candlesticks for stock "
  ^ stockname ^ " is: " ^ final_list in
  (portfoliolist, final_string)
and colors portfoliolist pi stockname =
  let (_, csdlist) = Asset.find_asset stockname in
  let candle_list = make_candle_list csdlist in
  let color_list = List.map get_color_to_string candle_list in
  let stringed_list = make_string_of_list color_list in
  let final_string = "The list of the colors for the candlesticks for stock "
  ^ stockname ^ " is: " ^ stringed_list in
  (portfoliolist, final_string)
and value_report portfoliolist pi =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  (* report of current portfolio *)
  (* use colors to rank each asset in list by best/worst *)
  (* overall report tells you best and worst asset *)
  (* if consistently green, tells you to stay invested or consider investing more *)
  (* if consistently red, tells you to consider selling *)
  (portfoliolist, "to be implemented")
and sector_report portfoliolist pi =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let sectors = Asset.find_list_sectors current_portfolio in
  let joined_list = List.map consolidate_sector sectors in
  let sector_string = make_string_of_list joined_list in
  let final_string = "Your sector report: " ^ sector_string in
  (portfoliolist, final_string)
and percent_report portfoliolist pi date=
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let final_date = make_date date in
  let percent_list = Asset.perasset_percentc current_portfolio final_date in
  let joined_list = List.map consolidate_string_float percent_list in
  let percent_string = make_string_of_list joined_list in
  let final_string = "Your report by percent change: " ^ percent_string in
  (portfoliolist, final_string)
and dollar_report portfoliolist pi date=
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let final_date = make_date date in
  let dollar_list = Asset.perasset_dollarc current_portfolio final_date in
  let joined_list = List.map consolidate_string_float dollar_list in
  let dollar_string = make_string_of_list joined_list in
  let final_string = "Your report by dollar change: " ^ dollar_string in
  (portfoliolist,final_string)
and portfolio portfoliolist pi =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let print_string =
  if current_portfolio = [] then
    "Your portfolio is empty. Add some assets with #add to get started!"
  else
    Asset.portfolio_to_string current_portfolio in
    (portfoliolist, print_string ^ "/n To see your portfolio value, type #portfoliovalue;; /n")
and portfolio_value portfoliolist pi date =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let final_date = make_date date in
  let pval = Asset.total_portfolio_value current_portfolio final_date in
  let final_string =
  "Your total portfolio value for the given date is $" ^
  string_of_float pval ^ "." in
  (portfoliolist,final_string)
and stock portfoliolist pi stockname date =
  let final_date = make_date date in
  let stock = Asset.find_asset stockname in
  let (_, candle) = Stock.find_date stock final_date in
  let candle_string = Candlestick.to_string candle in
  let final_string = "The stock's candlestick report for the given date is: "
  ^ candle_string in
  (portfoliolist, final_string)
and buy name date portfoliolist pi =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in

  (portfoliolist, name ^ date ^ "to be implemented")
and sell name date portfoliolist pi =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in

  (portfoliolist, name ^ date ^ "to be implemented")
and total_percent_change name date portfoliolist pi =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let final_date = make_date date in
  let total_change = Asset.total_asset_pc current_portfolio final_date in
  let final_string =
    "Your total percent change for your portfolio on the given date is "
    ^ string_of_float total_change in
  (portfoliolist,final_string)
and total_dollar_change name date portfoliolist pi =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let final_date = make_date date in
  let total_change = Asset.total_dollarc current_portfolio final_date in
  let final_string =
    "Your total dollar change for your portfolio on the given date is "
    ^ string_of_float total_change in
    (portfoliolist,final_string)
and total_portfolio_quantity name date portfoliolist pi =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let asset_list = Asset.total_asset_quantity current_portfolio in
  let joined_list = List.map consolidate_string_float asset_list in
  let asset_quantity = make_string_of_list joined_list in
  let final_string = "Your total asset quantity is " ^ asset_quantity in
  (portfoliolist,final_string)
and particular_quantity portfoliolist pi name date =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let final_date = make_date date  in
  let stock_val = Asset.particular_stock_quantity current_portfolio name final_date 
in let print_string = "The value for " ^ name ^ " on " ^ date ^ " is "
^ string_of_float stock_val
in 
  (portfoliolist, print_string)
and particular_value portfoliolist pi name date =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let final_date = make_date date  in
  let stock_val = Asset.particular_stock_value current_portfolio name final_date
in let print_string = "The value for " ^ name ^ " on " ^ date ^ " is "
^ string_of_float stock_val
in
  (portfoliolist, print_string)
and worst_dollar portfoliolist pi date =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let final_date = make_date date in
  let (name, change) = Asset.worst_dollar_asset current_portfolio final_date
in let print_string = "Your worst asset (by dollar change) is " ^ name ^
". Its dollar change was $" ^ string_of_float change ^ "."
in
  (portfoliolist, print_string)
and worst_percent portfoliolist pi date=
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let final_date = make_date date in
  let (name, change) = Asset.worst_percent_asset current_portfolio final_date
in let print_string = "Your worst asset (by percent change) is " ^ name ^ ".
Its percent change was " ^ string_of_float change ^ "%."
in
  (portfoliolist,print_string)
and best_dollar portfoliolist pi date=
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let final_date = make_date date in
  let (name, change) = Asset.best_dollar_asset current_portfolio final_date
in let print_string = "Your best asset (by dollar change) is " ^ name ^
". Its dollar change was $" ^ string_of_float change ^ "."
in

  (portfoliolist,print_string)
and best_percent portfoliolist pi date =
  let current_portfolio = List.nth portfoliolist (int_of_string pi) in
  let final_date = make_date date in
  let (name, change) = Asset.best_percent_asset current_portfolio final_date
in let print_string = "Your best asset (by percent change) is " ^ name ^ ".
Its percent change was " ^ string_of_float change ^ "%."
in 
  (portfoliolist,print_string)
  (* collect user input, terminated by ;; *)
let rec collect acc =
  let input = read_line () in
  if Str.string_match (Str.regexp {|^.*;;|}) input 0 then
    let input = String.sub input 0 (String.length input - 2) in
    acc ^ input
  else collect (acc ^ input)
(* read-eval-print loop *)
let rec repl (portfoliolist : portfolio list) : unit =
  try
    print_string "# ";
    let input = collect "" in
    let output_portfoliolist, output = eval portfoliolist input in
    print_endline output;
    repl output_portfoliolist
  with Quit -> ()
let _ =
  print_endline "\nPortfolio Manager: Your Personal Assistant\n";
  print_endline "Welcome.";
  print_endline "An empty personal portfolio has been created for you.";
  print_endline "We will keep track of your activity!\n";
  print_endline "Follow the instructions below to interact!";
  print_endline "Type \"#quit\" to quit interacting";
  print_endline "Type \"#stock\" \"stock_name\" to see information about \"stock_name\"";
  print_endline "Type \"#add name(string) quantity(float) price(float) date(mm/dd/yyyy)
  sector(string)\" to add a new stock to your portfolio.";
  print_endline "Type \"#candle stock_name\" to see the candlestick data for the given stock";
  print_endline "Type \"#random\" to see a random asset in your portfolio.";
  print_endline "Type \"#colors name\" to see a compiled list of the candlestick colors for the given stock";
  print_endline "Type \"#eods name\" to see a compiled list of the end of day values for the given stock";
  print_endline "Type \"#weight\" to see the ????";
  print_endline "Type \"#sector\" to see a compiled list of the sectors in your portfolio";
  print_endline "Type \"#percent\" to see the total percent change for your portfolio";
  print_endline "Type \"#dollar\" to see the total dollar change for your portfolio";
  print_endline "Type \"#value stock_name date(mm/dd/yyyy)\" to see the particular value for the given stock name";
  print_endline "Type \"#quantity stock_name date(mm/dd/yyyy)\" to see the particular quantity for the given stock name";
  print_endline "Type \"#bestpercent\" to see the best asset by percent change in your portfolio";
  print_endline "Type \"#worstpercent\" to see the worst asset by percent change in your portfolio";
  print_endline "Type \"#bestdollar\" to see the best asset by dollar change in your portfolio";
  print_endline "Type \"#worstdollar\" to see the worst asset by dollar change in your portfolio";
  print_endline "Type \"#portfoliovalue\" to see your overall portfolio value";
  print_endline "Type \"#totalquantity stock_name date(mm/dd/yyyy)\" to see the total quantity of the given stock in your portfolio";
  print_endline "Type \"#dollar stock_name date(mm/dd/yyyy)\" to see dollar change of the given stock on the given date";
  print_endline "Type \"#percent stock_name date(mm/dd/yyyy)\" to see percent change of the given stock on the given date";
  print_endline "Type \"#buy? stock_name date(mm/dd/yyyy)\" to inquire if you should buy the given stock";
  print_endline "Type \"#sell? stock_name date(mm/dd/yyyy)\" to inquire if you should sell the given stock";
  repl init_portfolio
let () = print_endline "Thank you. Come again soon, goodbye!";