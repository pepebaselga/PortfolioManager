open OUnit2
open PortfolioManager
open Module1

let vd1 : Date.date = Date.make_date 1 2 2023
let vd2 = Date.make_date 2 28 2022
let vd3 = Date.make_date 4 19 2004
let vd4 = Date.make_date 6 11 2006
let vd5 = Date.make_date 11 24 2009
let nvd1 = Date.make_date 1 2 3
let nvd2 = Date.make_date 2 28 20000
let nvd3 = Date.make_date 2 30 2023
let nvd4 = Date.make_date 11 31 2023
let cs1 : Candlestick.cs = Candlestick.make_cs 1.1 1.3 0.9 1.2
let cs2 : Candlestick.cs = Candlestick.make_cs 1.4 1.7 0.8 1.0
let cs3 : Candlestick.cs = Candlestick.make_cs 1.2 1.7 0.8 1.2

let grey_cs = Candlestick.get_color (Candlestick.make_cs 10.1 1.3 0.9 10.1)
let green_cs = Candlestick.get_color (Candlestick.make_cs 1.1 1.3 0.9 10.1)
let red_cs = Candlestick.get_color (Candlestick.make_cs 10.1 1.3 0.9 1.1)

let csv1a = [["Date"; "Open";	"High";	"Low"; "Close"];
            ["12/4/22"; "1.1"; "1.3"; "0.9"; "1.2" ];
            ["12/5/22"; "1.2"; "1.6"; "1.0"; "1.4" ]]
let csv1b = [["Date"; "Open";	"High";	"Low"; "Close"];
            ["12/4/22"; "1.1"; "1.3"; "0.9"; "1.2" ];
            ["12/5/22"; "1.2"; "1.6"; "1.0"; "1.4" ]]

let csv2 = [["Date"; "Open";	"High";	"Low"; "Close"];
            ["12/4/22"; "1.1"; "1.3"; "0.9"; "1.2" ];
            ["12/5/22"; "1.2"; "1.6"; "1.0"; "1.3" ]]
let csv2str = "12/4/2022: 1.1 | 1.3 | 0.9 | 1.2
12/5/2022: 1.2 | 1.6 | 1. | 1.3"
let csv3 = [["12/4/22"; "1.1"; "1.3"; "0.9"; "1.2" ]]

let date_tests =
  [
    ("check valid date - 1/30/2022" >:: fun _ ->
      assert_equal true
      (Date.check_date vd1) 
    );
    ( "check valid date - 2/28/2022" >:: fun _ ->
      assert_equal true
      (Date.check_date vd2)
    );
    ( "check valid date - 5/31/2023" >:: fun _ ->
      assert_equal false
      (Date.check_date nvd1)
    );
    ( "check invalid date - 2/31/2022" >:: fun _ ->
      assert_equal false
      (Date.check_date nvd2)
    );
    ( "check valid date - 13/20/2022" >:: fun _ ->
      assert_equal false
      (Date.check_date nvd3)
    );
    ( "check valid date - 5/31/2001" >:: fun _ ->
      assert_equal false
      (Date.check_date nvd4)
    );
    ( "compare dates less than" >:: fun _ -> 
      assert_equal (Date.date_comp_helper "LT") (Date.compare vd4 vd5)
    );    
    ( "compare dates greater than" >:: fun _ -> 
      assert_equal (Date.date_comp_helper "GT") (Date.compare vd5 vd3)
    );
    ( "compare dates equal" >:: fun _ -> 
      assert_equal (Date.date_comp_helper "EQ") (Date.compare vd5 (Date.of_string "9/24/2022"))
    );
    ( "dates to string" >:: fun _ -> 
      assert_equal ("11/24/2009") (Date.to_string vd5) ~printer: (fun x -> x)
    );

  ]
  let candle_tests =
  [
    ("candlestick to_string" >:: fun _ ->
    assert_equal (Candlestick.to_string cs1) ("1.1 | 1.3 | 0.9 | 1.2") ~printer: (fun x -> x));
    ("candlestick eod change (profit)" >:: fun _ ->
    assert_equal (1.2 -. 1.1) (Candlestick.eod_change cs1) ~printer: (fun x -> string_of_float x));
    ("candlestick eod change (loss)" >:: fun _ ->
    assert_equal (1.0 -. 1.4) (Candlestick.eod_change cs2) ~printer: (fun x -> string_of_float x));
    ("candlestick eod change (even)" >:: fun _ ->
    assert_equal (1.2 -. 1.2) (Candlestick.eod_change cs3) ~printer: (fun x -> string_of_float x));
    ("candlestick color (even)" >:: fun _ ->
    assert_equal (grey_cs) (Candlestick.get_color cs3));
    ("candlestick color (loss)" >:: fun _ ->
    assert_equal (red_cs) (Candlestick.get_color cs2));
    ("candlestick color (profit)" >:: fun _ ->
    assert_equal (green_cs) (Candlestick.get_color cs1));
    
        
  ]
  let stock_tests =
    [
      ("csv to stock" >:: fun _ ->
      assert_equal (Stock.yf_to_stock csv1a) (Stock.yf_to_stock csv1b) ~printer: (fun x -> Stock.to_string x));
      ("csv to stock/to_string" >:: fun _ ->
      assert_equal (Stock.to_string(Stock.yf_to_stock csv2)) (csv2str) ~printer: (fun x -> x));
  
    ]
let portfolio_tests =
  [
  ]
let suite =
  "test suite"
  >::: List.flatten [date_tests; candle_tests; stock_tests; portfolio_tests]
let () = run_test_tt_main suite