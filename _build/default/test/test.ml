open OUnit2
open PortfolioManager
open Module1
open Module2
open Module3
open Module4

let vd1 = Date.make_date 1 2 2023
let vd2 = Date.make_date 2 28 2022
let vd3 = Date.make_date 4 19 2004
let vd4 = Date.make_date 6 11 2006
let vd5 = Date.make_date 11 24 2009
let vd6 = Date.make_date 12 31 2021
let nvd1 = Date.make_date 1 2 3
let nvd2 = Date.make_date 2 28 20000
let nvd3 = Date.make_date 2 30 2023
let nvd4 = Date.make_date 11 31 2023
let nvd5 = Date.make_date 9 31 2021
let lvd1 = Date.make_date 2 29 2020
let lnv1 = Date.make_date 2 29 2022
let cs1 : Candlestick.cs = Candlestick.make_cs 1.1 1.3 0.9 1.2
let cs2 : Candlestick.cs = Candlestick.make_cs 1.4 1.7 0.8 1.0
let cs3 : Candlestick.cs = Candlestick.make_cs 1.2 1.7 0.8 1.2
let grey_cs = Candlestick.get_color (Candlestick.make_cs 10.1 1.3 0.9 10.1)
let green_cs = Candlestick.get_color (Candlestick.make_cs 1.1 1.3 0.9 10.1)
let red_cs = Candlestick.get_color (Candlestick.make_cs 10.1 1.3 0.9 1.1)

let csv1a =
  [
    [ "Date"; "Open"; "High"; "Low"; "Close" ];
    [ "12/4/22"; "1.1"; "1.3"; "0.9"; "1.2" ];
    [ "12/5/22"; "1.2"; "1.6"; "1.0"; "1.4" ];
  ]

let csv1b =
  [
    [ "Date"; "Open"; "High"; "Low"; "Close" ];
    [ "12/4/22"; "1.1"; "1.3"; "0.9"; "1.2" ];
    [ "12/5/22"; "1.2"; "1.6"; "1.0"; "1.4" ];
  ]

let csv2 =
  [
    [ "Date"; "Open"; "High"; "Low"; "Close" ];
    [ "12/4/22"; "1.1"; "1.3"; "0.9"; "1.2" ];
    [ "12/5/22"; "1.2"; "1.6"; "1.0"; "1.3" ];
  ]

let csv2str =
  "12/4/2022: 1.1 | 1.3 | 0.9 | 1.2\n12/5/2022: 1.2 | 1.6 | 1. | 1.3"

let csv3 = [ [ "12/4/22"; "1.1"; "1.3"; "0.9"; "1.2" ] ]

let date_tests =
  [
    ( "check valid date - 1/30/2022" >:: fun _ ->
      assert_equal true (Date.check_date vd1) );
    ( "check valid date - 2/28/2022" >:: fun _ ->
      assert_equal true (Date.check_date vd2) );
    ( "check valid date - 5/31/2023" >:: fun _ ->
      assert_equal false (Date.check_date nvd1) );
    ( "check invalid date - 2/31/2022" >:: fun _ ->
      assert_equal false (Date.check_date nvd2) );
    ( "check valid date - 13/20/2022" >:: fun _ ->
      assert_equal false (Date.check_date nvd3) );
    ( "check valid date - 5/31/2001" >:: fun _ ->
      assert_equal false (Date.check_date nvd4) );
    ( "check valid date leap- 2/29/2020" >:: fun _ ->
      assert_equal true (Date.check_date lvd1) );
    ( "check valid invalid-date leap- 2/29/2022" >:: fun _ ->
      assert_equal false (Date.check_date lnv1) );
    ( "compare dates less than" >:: fun _ ->
      assert_equal (Date.date_comp_helper "LT") (Date.compare vd4 vd5) );
    ( "compare dates greater than" >:: fun _ ->
      assert_equal (Date.date_comp_helper "GT") (Date.compare vd5 vd3) );
    ( "compare dates equal" >:: fun _ ->
      assert_equal
        (Date.date_comp_helper "EQ")
        (Date.compare vd5 (Date.of_string "11/24/2009")) );
    ( "dates to string" >:: fun _ ->
      assert_equal "11/24/2009" (Date.to_string vd5) ~printer:(fun x -> x) );
    ( "check pp_date - 1/2/2023" >:: fun _ ->
      assert_equal "1/2/2023" (Date.pp_date vd1) );
    ( "check pp_date - 2/28/2022" >:: fun _ ->
      assert_equal "2/28/2022" (Date.pp_date vd2) );
    ( "check pp_date- 12/31/2021" >:: fun _ ->
      assert_equal "12/31/2021" (Date.pp_date vd6) );
    ( "check pp_date leap- 2/29/2020" >:: fun _ ->
      assert_equal "2/29/2020" (Date.pp_date lvd1) );
    ( "check pp_date - invalid-date leap" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date lnv1) );
    ( "check pp_date - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date nvd1) );
    ( "check pp_date - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date nvd2) );
    ( "check pp_date - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date nvd3) );
    ( "check pp_date - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date nvd4) );
    ( "check pp_date - invalid-date " >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date nvd5) );
    ( "check to_string - 1/2/2023" >:: fun _ ->
      assert_equal "1/2/2023" (Date.to_string vd1) );
    ( "check pp_date - 2/28/2022" >:: fun _ ->
      assert_equal "2/28/2022" (Date.to_string vd2) );
    ( "check pp_date- 12/31/2021" >:: fun _ ->
      assert_equal "12/31/2021" (Date.to_string vd6) );
    ( "check to_string leap- 2/29/2020" >:: fun _ ->
      assert_equal "2/29/2020" (Date.to_string lvd1) );
    ( "check to_string - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string nvd1) );
    ( "check to_string - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string nvd2) );
    ( "check to_string - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string nvd3) );
    ( "check to_string - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string nvd4) );
    ( "check to_string - invalid-date leap" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string lnv1) );
    ( "check to_string - invalid-date " >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string nvd5) );
    ( "check of_string vd1" >:: fun _ ->
      assert_equal vd1 (Date.of_string "1/2/2023") );
    ( "check of_string vd2" >:: fun _ ->
      assert_equal vd2 (Date.of_string "2/28/2022") );
    ( "check of_string vd3" >:: fun _ ->
      assert_equal vd6 (Date.of_string "12/31/2021") );
  ]

let candle_tests =
  [
    ( "candlestick to_string" >:: fun _ ->
      assert_equal (Candlestick.to_string cs1) "1.1 | 1.3 | 0.9 | 1.2"
        ~printer:(fun x -> x) );
    ( "candlestick eod change (profit)" >:: fun _ ->
      assert_equal (1.2 -. 1.1) (Candlestick.eod_change cs1) ~printer:(fun x ->
          string_of_float x) );
    ( "candlestick eod change (loss)" >:: fun _ ->
      assert_equal (1.0 -. 1.4) (Candlestick.eod_change cs2) ~printer:(fun x ->
          string_of_float x) );
    ( "candlestick eod change (even)" >:: fun _ ->
      assert_equal (1.2 -. 1.2) (Candlestick.eod_change cs3) ~printer:(fun x ->
          string_of_float x) );
    ( "candlestick color (even)" >:: fun _ ->
      assert_equal grey_cs (Candlestick.get_color cs3) );
    ( "candlestick color (loss)" >:: fun _ ->
      assert_equal red_cs (Candlestick.get_color cs2) );
    ( "candlestick color (profit)" >:: fun _ ->
      assert_equal green_cs (Candlestick.get_color cs1) );
  ]

let stock_tests =
  [
    ( "csv to stock" >:: fun _ ->
      assert_equal (Stock.yf_to_stock "APPL" csv1a)
        (Stock.yf_to_stock "APPL" csv1b) ~printer:(fun x -> Stock.to_string x)
    );
    ( "csv to stock/to_string" >:: fun _ ->
      assert_equal
        (Stock.to_string (Stock.yf_to_stock "APPL" csv2))
        ("APPL: " ^ csv2str)
        ~printer:(fun x -> x) );
  ]

let portfolio_tests =
  [
    ( "test make_asset" >:: fun _ ->
      let example =
        Asset.make_asset "Example Company" 100.0 50.0 (Date.make_date 1 1 2023)
          "Technology"
      in
      assert_equal Asset.example_asset example ~printer:Asset.asset_to_string );
    ( "add_asset" >:: fun _ ->
      let new_asset =
        Asset.make_asset "Microsoft" 100.0 50.0 (Date.make_date 1 1 2023)
          "Technology"
      in
      let portfolio = Asset.add_asset [] new_asset in
      assert_equal [ new_asset ] portfolio ~printer:Asset.portfolio_to_string );
    ( "remove_asset" >:: fun _ ->
      let portfolio =
        Asset.remove_asset [ Asset.example_asset ] "Example Company"
          (Date.make_date 1 1 2023)
      in
      assert_equal [] portfolio ~printer:Asset.portfolio_to_string );
    ( "update_asset_quantity" >:: fun _ ->
      let updated_portfolio =
        Asset.update_asset_quantity [ Asset.example_asset ] "Example Company"
          200.0
      in
      let expected_asset = { Asset.example_asset with quantity = 200.0 } in
      assert_equal [ expected_asset ] updated_portfolio
        ~printer:Asset.portfolio_to_string );
    ( "add_non_existent_asset" >:: fun _ ->
      let non_existent_asset =
        Asset.make_asset "Non-existent" 50.0 100.0 (Date.make_date 1 1 2022)
          "Unknown"
      in
      let portfolio = Asset.add_asset [] non_existent_asset in
      assert_equal [ non_existent_asset ] portfolio
        ~printer:Asset.portfolio_to_string );
    ( "remove_non_existent_asset_empty_portfolio" >:: fun _ ->
      let portfolio =
        Asset.remove_asset [] "Non-existent" (Date.make_date 1 1 2023)
      in
      assert_equal [] portfolio ~printer:Asset.portfolio_to_string );
    ( "remove_non_existent_asset_non_empty_portfolio" >:: fun _ ->
      let portfolio =
        Asset.remove_asset [ Asset.example_asset ] "Non-existent"
          (Date.make_date 1 1 2023)
      in
      assert_equal [ Asset.example_asset ] portfolio
        ~printer:Asset.portfolio_to_string );
    ( "update_quantity_non_existent_asset" >:: fun _ ->
      let portfolio =
        Asset.update_asset_quantity [ Asset.example_asset ] "Non-existent" 300.0
      in
      assert_equal [ Asset.example_asset ] portfolio
        ~printer:Asset.portfolio_to_string );
    ( "add_non_existent_asset" >:: fun _ ->
      let non_existent_asset =
        Asset.make_asset "Non-existent" 50.0 100.0 (Date.make_date 1 1 2022)
          "Unknown"
      in
      let portfolio = Asset.add_asset [] non_existent_asset in
      assert_equal [ non_existent_asset ] portfolio
        ~printer:Asset.portfolio_to_string );
    ( "remove_non_existent_asset_empty_portfolio" >:: fun _ ->
      let portfolio =
        Asset.remove_asset [] "Non-existent" (Date.make_date 1 1 2023)
      in
      assert_equal [] portfolio ~printer:Asset.portfolio_to_string );
    ( "remove_non_existent_asset_non_empty_portfolio" >:: fun _ ->
      let portfolio =
        Asset.remove_asset [ Asset.example_asset ] "Non-existent"
          (Date.make_date 1 1 2023)
      in
      assert_equal [ Asset.example_asset ] portfolio
        ~printer:Asset.portfolio_to_string );
    ( "update_quantity_non_existent_asset" >:: fun _ ->
      let portfolio =
        Asset.update_asset_quantity [ Asset.example_asset ] "Non-existent" 300.0
      in
      assert_equal [ Asset.example_asset ] portfolio
        ~printer:Asset.portfolio_to_string );
    ( "asset_to_string" >:: fun _ ->
      let test_asset =
        Asset.make_asset "Test Asset" 150.0 75.0 (Date.make_date 1 1 2023)
          "Finance"
      in
      let expected_string =
        "[Name: Test Asset, Quantity: 150.00, Purchase Price: 75.00, Date \
         Purchased: 1/1/2023, Sector: Finance]"
      in
      assert_equal expected_string (Asset.asset_to_string test_asset)
        ~printer:(fun x -> x) );
    ( "portfolio_to_string_single_asset" >:: fun _ ->
      let test_asset =
        Asset.make_asset "Test Asset" 150.0 75.0 (Date.make_date 1 1 2023)
          "Finance"
      in
      let test_portfolio = [ test_asset ] in
      let expected_string =
        "[Name: Test Asset, Quantity: 150.00, Purchase Price: 75.00, Date \
         Purchased: 1/1/2023, Sector: Finance]"
      in
      assert_equal expected_string (Asset.portfolio_to_string test_portfolio)
        ~printer:(fun x -> x) );
    ( "portfolio_to_string_multiple_assets" >:: fun _ ->
      let asset1 =
        Asset.make_asset "Asset One" 100.0 50.0 (Date.make_date 1 1 2023) "Tech"
      in
      let asset2 =
        Asset.make_asset "Asset Two" 200.0 100.0 (Date.make_date 2 1 2023)
          "Health"
      in
      let test_portfolio = [ asset1; asset2 ] in
      let expected_string =
        "[Name: Asset One, Quantity: 100.00, Purchase Price: 50.00, Date \
         Purchased: 1/1/2023, Sector: Tech], [Name: Asset Two, Quantity: \
         200.00, Purchase Price: 100.00, Date Purchased: 2/1/2023, Sector: \
         Health]"
      in
      assert_equal expected_string (Asset.portfolio_to_string test_portfolio)
        ~printer:(fun x -> x);
      assert_equal
        (Stock.to_string (Index.datacreation "PEP.csv"))
        (Stock.to_string (Index.datacreation "PEP.csv"))
        ~printer:(fun x -> x) );
    ( "testing portfolio funcs" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 12 9 2022) "F&B"
      in
      let a2 =
        Asset.make_asset "NYT" 1.0 33.0 (Date.make_date 12 4 2023) "News"
      in
      let p2 = Asset.add_asset p1 a1 in
      let finalp = Asset.add_asset p2 a2 in
      let str, float_val =
        Asset.best_dollar_asset finalp (Date.make_date 12 8 2023)
      in
      assert_equal (string_of_float float_val) (string_of_float float_val)
        ~printer:(fun x -> x);
      assert_equal 2.0
        (Asset.particular_stock_quantity finalp "PEP" (Date.make_date 12 8 2023))
        ~printer:(fun x -> string_of_float x) );
    ( "testing best_dollar_asset" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 1 4 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "NYT" 1.0 33.0 (Date.make_date 12 4 2023) "News"
      in
      let p2 = Asset.add_asset p1 a1 in
      let finalp = Asset.add_asset p2 a2 in
      let str, float_val =
        Asset.best_dollar_asset finalp (Date.make_date 12 8 2023)
      in
      assert_equal (string_of_float float_val) (string_of_float float_val)
        ~printer:(fun x -> x) );
    ( "testing worst_dollar_asset" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 2 6 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 3 6 2023) "F&B"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 4 6 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      let str, float_val =
        Asset.worst_dollar_asset finalp (Date.make_date 12 8 2023)
      in
      assert_equal (string_of_float float_val) (string_of_float float_val)
        ~printer:(fun x -> x) );
    ( "testing worst_percent_asset" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 4 12 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 6 15 2023) "F&B"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 9 5 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      let str, float_val =
        Asset.worst_percent_asset finalp (Date.make_date 12 6 2023)
      in
      assert_equal (string_of_float float_val) (string_of_float float_val)
        ~printer:(fun x -> x) );
    ( "testing total_portfolio_value multiple values" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 1 5 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 8 3 2023) "F&B"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 2 5 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (Asset.total_portfolio_value finalp (Date.make_date 12 4 2023)))
        "555.089995"
        ~printer:(fun x -> x) );
    ( "testing total_portfolio_value 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 6 9 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (Asset.total_portfolio_value p2 (Date.make_date 12 22 2022)))
        "362.179992"
        ~printer:(fun x -> x) );
    ( "testing total_dollarc multiple values" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 8 14 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 9 15 2023) "F&B"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 5 16 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (Asset.total_dollarc finalp (Date.make_date 12 8 2023)))
        "-39.520007"
        ~printer:(fun x -> x) );
    ( "testing total_dollarc 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 4 18 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float (Asset.total_dollarc p2 (Date.make_date 9 5 2023)))
        "-21.339996"
        ~printer:(fun x -> x) );
    ( "testing total_asset_pc multiple values" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 6 1 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 4 12 2023) "F&B"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 2 3 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (Asset.total_asset_pc finalp (Date.make_date 8 14 2023)))
        "0.214146148085"
        ~printer:(fun x -> x) );
    ( "testing total_asset_pc 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 7 10 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float (Asset.total_asset_pc p2 (Date.make_date 9 15 2023)))
        "-0.0241997120315"
        ~printer:(fun x -> x) );
    ( "testing perasset_dollarc multiple values" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 1 12 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 2 17 2023) "F&B"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 9 5 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (List.fold_left
              (fun sum (_, change) -> sum +. change)
              0.0
              (Asset.perasset_dollarc finalp (Date.make_date 12 8 2023))))
        "-27.95002"
        ~printer:(fun x -> x) );
    ( "testing perasset_dollarc 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 8 28 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (List.fold_left
              (fun sum (_, change) -> sum +. change)
              0.0
              (Asset.perasset_dollarc p2 (Date.make_date 9 5 2023))))
        "-12.179992"
        ~printer:(fun x -> x) );
    ( "testing perasset_percentc multiple values" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 9 5 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 2 17 2023) "F&B"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 4 12 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (List.fold_left
              (fun sum (_, change) -> sum +. change)
              0.0
              (Asset.perasset_percentc finalp (Date.make_date 12 8 2023))))
        "0.0819937319483"
        ~printer:(fun x -> x) );
    ( "testing perasset_dollarc 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 6 1 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (List.fold_left
              (fun sum (_, change) -> sum +. change)
              0.0
              (Asset.perasset_percentc p2 (Date.make_date 9 5 2023))))
        "-0.0440748554358"
        ~printer:(fun x -> x) );
    ( "testing perasset_percentc multiple values" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 9 15 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 3 17 2023) "F&B"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 4 5 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (List.fold_left
              (fun sum (_, change) -> sum +. change)
              0.0
              (Asset.perasset_percentc finalp (Date.make_date 9 19 2023))))
        "0.0985662739527"
        ~printer:(fun x -> x) );
    ( "testing perasset_dollarc 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 9 18 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (List.fold_left
              (fun sum (_, change) -> sum +. change)
              0.0
              (Asset.perasset_percentc p2 (Date.make_date 10 19 2023))))
        "-0.104916972212"
        ~printer:(fun x -> x) );
    ( "testing total_asset_quantity multiple values" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 1 18 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 1 12 2023) "F&B"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 3 1 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (List.fold_left
              (fun sum (_, change) -> sum +. change)
              0.0
              (Asset.total_asset_quantity finalp)))
        "4."
        ~printer:(fun x -> x) );
    ( "testing total_asset_quantity 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 9 5 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (List.fold_left
              (fun sum (_, change) -> sum +. change)
              0.0
              (Asset.total_asset_quantity p2)))
        "2."
        ~printer:(fun x -> x) );
    ( "testing find_list_sectors multiple values" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 5 5 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 9 15 2023) "F&B"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 2 12 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (List.fold_left
           (fun sum (first, second) -> sum ^ first ^ ": " ^ second ^ "; ")
           ""
           (Asset.find_list_sectors finalp))
        "PEP: F&B; PEP: F&B; NYT: F&B; "
        ~printer:(fun x -> x) );
    ( "testing find_list_sectors 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 3 3 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (List.fold_left
           (fun sum (first, second) -> sum ^ first ^ ": " ^ second ^ "; ")
           ""
           (Asset.find_list_sectors p2))
        "PEP: F&B; "
        ~printer:(fun x -> x) );
    ( "testing find_sector_pc" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 1 30 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 1 9 2023) "Tech"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 2 4 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (Asset.find_sector_pc finalp "Tech" (Date.make_date 2 9 2023)))
        "-0.0365248587908"
        ~printer:(fun x -> x) );
    ( "testing find_sector_pc 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 4 14 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (Asset.find_sector_pc p2 "F&B" (Date.make_date 4 17 2023)))
        "0.00512234769556"
        ~printer:(fun x -> x) );
    ( "testing find_restofsectors_pc" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 12 5 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 11 3 2023) "Tech"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 10 9 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (Asset.find_restofsectors_pc finalp "Tech" (Date.make_date 12 8 2023)))
        "0.10142217469"
        ~printer:(fun x -> x) );
    ( "testing find_restofsectors_pc 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 12 4 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (Asset.find_restofsectors_pc p2 "F&B" (Date.make_date 5 6 2023)))
        "0."
        ~printer:(fun x -> x) );
    ( "testing find_sectors_dc" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 9 15 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 9 19 2023) "Tech"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 3 15 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (Asset.find_sector_dc finalp "Tech" (Date.make_date 12 8 2023)))
        "-12.640014"
        ~printer:(fun x -> x) );
    ( "testing find_sectors_dc 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 3 28 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (Asset.find_sector_dc p2 "F&B" (Date.make_date 3 29 2023)))
        "2.48001"
        ~printer:(fun x -> x) );
    ( "testing find_restofsectors_dc" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 3 1 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 4 2 2023) "Tech"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 6 5 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (Asset.find_restofsectors_dc finalp "Tech" (Date.make_date 12 1 2023)))
        "8.279998"
        ~printer:(fun x -> x) );
    ( "testing find_restofsectors_dc 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 1 2 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (Asset.find_restofsectors_dc p2 "F&B" (Date.make_date 5 6 2023)))
        "0."
        ~printer:(fun x -> x) );
    ( "testing find_multisector_pc" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 1 4 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 1 17 2023) "Tech"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 3 6 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (Asset.find_multisector_pc finalp [ "Tech"; "F&B" ]
              (Date.make_date 5 11 2023)))
        "0.137069022596"
        ~printer:(fun x -> x) );
    ( "testing find_multisector_pc 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 1 9 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (Asset.find_multisector_pc p2 [ "F&B" ] (Date.make_date 9 15 2023)))
        "0.00284388554236"
        ~printer:(fun x -> x) );
    ( "testing find_multisector_dc" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 5 5 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 6 1 2023) "Tech"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 1 4 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (Asset.find_multisector_dc finalp [ "Tech"; "F&B" ]
              (Date.make_date 8 9 2023)))
        "1.889985"
        ~printer:(fun x -> x) );
    ( "testing find_multisector_dc 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 11 7 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (Asset.find_multisector_dc p2 [ "F&B" ] (Date.make_date 12 6 2023)))
        "0.330002"
        ~printer:(fun x -> x) );
    ( "testing particular_stock_value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 12 5 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 12 12 2023) "Tech"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 3 19 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (Asset.particular_stock_value finalp "PEP" (Date.make_date 12 5 2023)))
        "503.820006"
        ~printer:(fun x -> x) );
    ( "testing particular_stock_value 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 1 8 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (Asset.particular_stock_value p2 "PEP" (Date.make_date 12 6 2023)))
        "335.01999"
        ~printer:(fun x -> x) );
    ( "testing particular_stock_quantity" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 6 5 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 7 12 2023) "Tech"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 1 28 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let finalp = Asset.add_asset p3 a3 in
      assert_equal
        (string_of_float
           (Asset.particular_stock_quantity finalp "PEP"
              (Date.make_date 1 5 2023)))
        "3."
        ~printer:(fun x -> x) );
    ( "testing particular_stock_quantity 1 value" >:: fun _ ->
      let a1 =
        Asset.make_asset "PEP" 1.0 100.0 (Date.make_date 6 5 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "NYT" 1.0 50.0 (Date.make_date 6 5 2023) "F&B"
      in
      let a3 =
        Asset.make_asset "ABNB" 1.0 10.0 (Date.make_date 6 5 2023) "F&B"
      in
      let p1 = [ a1; a2 ] in
      let p2 = [ a1; a2; a3 ] in
      let id, value =
        Asset.best_portfolio_value [ p1; p2 ] (Date.make_date 6 5 2023)
      in
      assert_equal
        ("(" ^ string_of_int id ^ "," ^ string_of_float value ^ ")")
        "(2,220.640003)"
        ~printer:(fun x -> x) );
    ( "testing particular_stock_quantity 1 value" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 12 5 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      assert_equal
        (string_of_float
           (Asset.particular_stock_quantity p2 "PEP" (Date.make_date 5 6 2023)))
        "2."
        ~printer:(fun x -> x) );
    ( "testing particular_stock_quantity in different orders" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 6 5 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 7 12 2023) "Tech"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 1 28 2023) "F&B"
      in
      let a4 =
        Asset.make_asset "NYT" 4.0 177.0 (Date.make_date 1 28 2023) "F&B"
      in
      let a5 =
        Asset.make_asset "NYT" 8.0 177.0 (Date.make_date 1 28 2023) "F&B"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let p4 = Asset.add_asset p3 a3 in
      let p5 = Asset.add_asset p4 a4 in
      let finalp = Asset.add_asset p5 a5 in
      assert_equal
        (string_of_float
           (Asset.particular_stock_quantity finalp "NYT"
              (Date.make_date 1 5 2023)))
        "13."
        ~printer:(fun x -> x) );
    ( "testing find_multisector_pc multiple sectors" >:: fun _ ->
      let (p1 : Asset.portfolio) = [] in
      let a1 =
        Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 1 4 2023) "F&B"
      in
      let a2 =
        Asset.make_asset "PEP" 1.0 177.0 (Date.make_date 1 17 2023) "Tech"
      in
      let a3 =
        Asset.make_asset "NYT" 1.0 177.0 (Date.make_date 3 9 2023) "Music"
      in
      let p2 = Asset.add_asset p1 a1 in
      let p3 = Asset.add_asset p2 a2 in
      let p4 = Asset.add_asset p3 a3 in
      let finalp = Asset.add_asset p4 a3 in
      assert_equal
        (string_of_float
           (Asset.find_multisector_pc finalp [ "Tech"; "Art" ]
              (Date.make_date 5 11 2023)))
        "0.109508112115"
        ~printer:(fun x -> x) );
  ]

let pd1 = Date.make_date 2 1 2022
let pd2 = Date.make_date 2 2 2022
let pd3 = Date.make_date 2 3 2022
let rhammer_cs : Candlestick.cs = Candlestick.make_cs 42.0 31.5 45.7 40.0
let doji_cs : Candlestick.cs = Candlestick.make_cs 50.0 121.0 5.0 49.5
let hammer_cs : Candlestick.cs = Candlestick.make_cs 40.0 45.0 30.0 42.0
let marubozu_bear_cs : Candlestick.cs = Candlestick.make_cs 50.0 50.5 29.0 29.5
let marubozu_bull_cs : Candlestick.cs = Candlestick.make_cs 29.5 50.5 29.0 50.0

let marubozu_neut_cs : Candlestick.cs =
  Candlestick.make_cs 500.5 501.0 500.0 500.5

let engulfing_bull_cs1 : Candlestick.cs =
  Candlestick.make_cs 93.6 94.5 89.0 90.3

let engulfing_bull_cs2 : Candlestick.cs =
  Candlestick.make_cs 89.2 94.5 89.0 93.8

let tweezertop_cs1 : Candlestick.cs = Candlestick.make_cs 50.6 57.1 50.0 55.1
let tweezertop_cs2 : Candlestick.cs = Candlestick.make_cs 50.6 57.0 50.0 55.1
let hammer : Stock.stock = ("Hammer", [ (pd3, hammer_cs) ])
let rev_hammer : Stock.stock = ("RevHammer", [ (pd3, rhammer_cs) ])
let doji : Stock.stock = ("Doji", [ (pd3, doji_cs) ])
let marubozu_bull : Stock.stock = ("Marubozu", [ (pd3, marubozu_bull_cs) ])
let marubozu_bear : Stock.stock = ("Marubozu", [ (pd3, marubozu_bear_cs) ])
let marubozu_neut : Stock.stock = ("Marubozu", [ (pd3, marubozu_neut_cs) ])
let no_pattern : Stock.stock = ("NoPattern", [ (pd3, cs2) ])

let engulfing_bull : Stock.stock =
  ("EngulfingBull", [ (pd2, engulfing_bull_cs1); (pd3, engulfing_bull_cs2) ])

let buysell_tests =
  [
    ( "testing hammer pattern" >:: fun _ ->
      assert_equal (BuySell.make_res Buy pd3 "Hammer")
        (BuySell.find_single_patterns hammer) ~printer:(fun x ->
          BuySell.res_to_string x) );
    ( "testing reverse hammer pattern" >:: fun _ ->
      assert_equal (BuySell.make_res Buy pd3 "RevHammer")
        (BuySell.find_single_patterns rev_hammer) ~printer:(fun x ->
          BuySell.res_to_string x) );
    ( "testing doji pattern" >:: fun _ ->
      assert_equal (BuySell.make_res Sell pd3 "Doji")
        (BuySell.find_single_patterns doji) ~printer:(fun x ->
          BuySell.res_to_string x) );
    ( "testing marubozu pattern (bullish)" >:: fun _ ->
      assert_equal (BuySell.make_res Buy pd3 "Marubozu(Bullish)")
        (BuySell.find_single_patterns marubozu_bull) ~printer:(fun x ->
          BuySell.res_to_string x) );
    ( "testing marubozu pattern (bearish)" >:: fun _ ->
      assert_equal (BuySell.make_res Sell pd3 "Marubozu(Bearish)")
        (BuySell.find_single_patterns marubozu_bear) ~printer:(fun x ->
          BuySell.res_to_string x) );
    ( "testing marubozu pattern (neutral)" >:: fun _ ->
      assert_equal (BuySell.make_res Neut pd3 "NoPattern")
        (BuySell.find_single_patterns marubozu_neut) ~printer:(fun x ->
          BuySell.res_to_string x) );
    ( "testing no pattern" >:: fun _ ->
      assert_equal (BuySell.make_res Neut pd3 "NoPattern")
        (BuySell.find_single_patterns no_pattern) ~printer:(fun x ->
          BuySell.res_to_string x) );
    ( "testing bullish engulfingg" >:: fun _ ->
      assert_equal (BuySell.make_res Buy pd3 "Engulfing(Bullish)")
        (BuySell.find_duo_patterns engulfing_bull) ~printer:(fun x ->
          BuySell.res_to_string x) );
  ]

let suite =
  "test suite"
  >::: List.flatten [ date_tests; candle_tests; stock_tests; portfolio_tests ]

let () = run_test_tt_main suite
