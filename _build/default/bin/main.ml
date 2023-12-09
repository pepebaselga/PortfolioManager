(* open PortfolioManager
open Module1
exception Quit

type stock = Stock.stock
type cs = Candlestick.cs
type portfolio = stock list
let init_portfolio = []
let empty_portfolio = []
let get_color (candle : cs) =
  let color = Candlestick.get_color candle in
  match color with
  | Red -> "Red"
  | Green -> "Green"
  | Grey -> "Grey"
(* evaluate user input *)
let rec eval (current_portfolio : portfolio) input =
  match input |> String.split_on_char ' ' |> List.filter (( <> ) "") with
  | [ "#quit" ] -> raise Quit
  | [ "#portfolio" ] -> (current_portfolio, "STRING OF PORTFOLIO") (* return the current portfolio for the user *)
  | [ "#stock"; "stockname" ] -> (current_portfolio, "STRING OF STOCK") (* return the stock with name and date of the stock *)
  | [ "#stockreport"; "stockname" ] -> (current_portfolio, "STRING OF STOCK") (* return the stock report *)
  | [ "#candle"; "stockname"] -> (current_portfolio, candle current_portfolio) (* returns the candlestick report for the given stock *)
  | [ "#random" ] -> (current_portfolio, random current_portfolio)
  | [ "#add" ] -> add current_portfolio
  | [ "#colors" ] -> (current_portfolio, "return the colors in a list") (* run analyzing functions on the portfolio *)
  | [ "#eods" ] -> (current_portfolio, "return the eods in a list")
  | [ "#csv"; "csvinput" ] -> (current_portfolio, "MAKE CSV: ()") (* process the csv file; pass it in *)
  | _ -> eval empty_portfolio "#quit"
and random current_portfolio =
  let random_num = Random.int (List.length current_portfolio) in
   let random_stock = List.nth current_portfolio random_num in
   "random_stock" ^
   "THIS IS THE STOCK'S NAME" (* ^ random_stock *)
    (* then do to string of this stock *)
and add current_portfolio =
  (* new stock :: current_portfolio *)
  (current_portfolio,
  "Added new stock of name: NEW STOCK'S NAME")
and candle current_portfolio (* stock name *) =
  (* getting head for now, add implementation to get stock name *)
  let stock_list = List.hd current_portfolio in
  let stock' = List.hd stock_list in
  let candlestick = snd stock' in
  let candle_color = get_color candlestick in
  let candle_change = Candlestick.eod_change candlestick in
  let string_change = string_of_float candle_change in
  candle_color ^ string_change
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
  print_endline "Type \"#portfolio\" to see your current portfolio";
  print_endline "Type \"#stock\" \"stock_name\" to see information about \"stock_name\"";
  print_endline "Type \"#random\" to see a random stock.";
  print_endline "Type \"#report\" to see how your portfolio is doing!";
  print_endline "Type \"#add\" to add a new stock to your portfolio.";
  print_endline "Type \"#stockreport\" \"stock_name\" to see a report on the stock's data.";
  repl init_portfolio
let () = print_endline "Thank you. Goodbye!";  *)