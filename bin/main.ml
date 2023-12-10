open PortfolioManager
open Module1
open Module2
exception Quit
type portfolio = Asset.portfolio
type stock = Stock.stock
type cs = Candlestick.cs
let empty_portfolio : portfolio = []
let init_portfolio : portfolio = empty_portfolio
let find_asset stockname =
  let stock_of_asset = Asset.find_asset stockname in
  stock_of_asset
let get_color_to_string (candle : cs) =
  let color = Candlestick.get_color candle in
  match color with
  | Red -> "Red"
  | Green -> "Green"
  | Grey -> "Grey"
(* evaluate user input *)
let rec eval (current_portfolio : portfolio) input =
  match input |> String.split_on_char ' ' |> List.filter (( <> ) "") with
  | [ "#quit" ] -> raise Quit
  | [ "#portfolio" ] -> portfolio current_portfolio
  | [ "#stock"; "stockname" ] -> stock current_portfolio "stockname"
  | [ "#candle"; name ] -> candle current_portfolio name
  | [ "#random" ] -> random current_portfolio
  | [ "#add"; name; quantity; price; date_purchased; sector ] ->
    add current_portfolio name quantity price date_purchased sector
  | [ "#colors"; name ] -> colors current_portfolio name
  | [ "#eods"; name ] -> eods current_portfolio name
  | [ "#weight" ] -> weight_report current_portfolio
  | [ "#value" ] -> value_report current_portfolio
  | [ "#sector" ] -> sector_report current_portfolio
  | [ "#percent" ] -> percent_report current_portfolio
  | [ "#dollar" ] -> dollar_report current_portfolio
  | [ "#value"; name; date ] -> particular_value current_portfolio name date
  | [ "#quantity"; name; date ] -> particular_quantity current_portfolio name date
  | [ "#bestpercent" ] -> best_percent current_portfolio
  | [ "#worstpercent" ] -> worst_percent current_portfolio
  | [ "#bestdollar" ] -> best_dollar current_portfolio
  | [ "#worstdollar" ] -> worst_dollar current_portfolio
  | [ "#portfoliovalue" ] -> portfolio_value current_portfolio
  | [ "#totalquantity"; asset; date ] -> total_portfolio_quantity asset date current_portfolio
  | [ "#totaldollar"; asset; date ] -> total_dollar_change asset date current_portfolio
  | [ "#totalpercent"; asset; date ] -> total_percent_change asset date current_portfolio
  | [ "#buy?"; asset; date ] -> buy asset date current_portfolio
  | [ "#sell?"; asset; date ] -> sell asset date current_portfolio
  | _ -> eval empty_portfolio "#quit"
and random current_portfolio =
  (* choose random asset in stock and give report (eods and candlestick) *)
  let random_asset = List.nth current_portfolio 0 in
  (current_portfolio, Asset.asset_to_string random_asset)
and add current_portfolio name quantity price date sector =
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
  " on " ^ (Date.to_string final_date) ^ "." in
  (asset :: current_portfolio, added_string_activity)
and candle current_portfolio stockname =
  (* get stock name, find stock in list by name *)
  (* take out candlestick by stock type *)
  (* candlestick eods together in a list *)
  (* head of list is "percentage of pos/neg" *)
  (current_portfolio, stockname ^ "to be implemented")
and eods current_portfolio stockname =
  (* get stock name, find stock in list by name *)
  (* take out candlestick by stock type *)
  (* candlestick eods together in a list *)
  (* head of list is "percentage of pos/neg" *)
  (current_portfolio, stockname ^ "to be implemented")
and colors current_portfolio stockname =
  (* get stock name, find stock in list by name *)
  (* take out candlestick from the stock type *)
  (* candlestick colors together in a list *)
  (* head of list is "percentage of colors" *)
  (current_portfolio, stockname ^ "to be implemented")
and weight_report current_portfolio =
  (* report of current portfolio *)
  (* use colors to rank each asset in list by best/worst *)
  (* overall report tells you best and worst asset *)
  (* if consistently green, tells you to stay invested or consider investing more *)
  (* if consistently red, tells you to consider selling *)
  (current_portfolio, "to be implemented")
and value_report current_portfolio =
  (* report of current portfolio *)
  (* use colors to rank each asset in list by best/worst *)
  (* overall report tells you best and worst asset *)
  (* if consistently green, tells you to stay invested or consider investing more *)
  (* if consistently red, tells you to consider selling *)
  (current_portfolio, "to be implemented")
and sector_report current_portfolio =
  (* report of current portfolio *)
  (* give list of sectors *)
  (* gives percentage rundown *)
  (* all in a string *)
  (current_portfolio, "to be implemented")
and percent_report current_portfolio =
  (* percent change report for each asset in portfolio *)
  (* tied together in a list first *)
  (* concatenated all to a string *)
  (current_portfolio, "to be implemented")
and dollar_report current_portfolio =
  (* dollar change report for each asset in portfolio *)
  (* tied together in a list first *)
  (* concatenated all to a string *)
  (current_portfolio, "to be implemented")
and portfolio current_portfolio =
  let print_string =
  if current_portfolio = [] then
    "Your portfolio is empty. Add some assets with #add to get started!"
  else
    Asset.portfolio_to_string current_portfolio in
    (current_portfolio, print_string ^ "/n To see your portfolio value, type #portfoliovalue;; /n")
and portfolio_value current_portfolio =
  (current_portfolio, "to be implemented")
and stock current_portfolio stockname =
  (* use to give advice before someone buys it *)
  (* search stock by stock name *)
  (* give stock report with stock to string *)
  (* based on stock to strings, suggest to buy/not buy *)
  (current_portfolio, stockname ^ "to be implemented")
and buy name date current_portfolio =
  (current_portfolio, name ^ date ^ "to be implemented")
and sell name date current_portfolio =
  (current_portfolio, name ^ date ^ "to be implemented")
and total_percent_change name date current_portfolio =
  (current_portfolio, name ^ date ^ "to be implemented")
and total_dollar_change name date current_portfolio =
    (current_portfolio, name ^ date ^ "to be implemented")
and total_portfolio_quantity name date current_portfolio =
  (current_portfolio, name ^ date ^ "to be implemented")
and particular_quantity current_portfolio name date =
  (current_portfolio, name ^ date ^ "to be implemented")
and particular_value current_portfolio name date =
  (current_portfolio, name ^ date ^ "to be implemented")
and worst_dollar current_portfolio =
  (current_portfolio, "to be implemented")
and worst_percent current_portfolio =
  (current_portfolio, "to be implemented")
and best_dollar current_portfolio =
  (current_portfolio, "to be implemented")
and best_percent current_portfolio =
  (current_portfolio, "to be implemented")
  (* collect user input, terminated by ;; *)
let rec collect acc =
  let input = read_line () in
  if Str.string_match (Str.regexp {|^.*;;|}) input 0 then
    let input = String.sub input 0 (String.length input - 2) in
    acc ^ input
  else collect (acc ^ input)
(* read-eval-print loop *)
let rec repl (current_portfolio : portfolio) : unit =
  try
    print_string "# ";
    let input = collect "" in
    let output_portfolio, output = eval current_portfolio input in
    print_endline output;
    repl output_portfolio
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