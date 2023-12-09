open OUnit2
open PortfolioManager
open Module1
open Module2

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
    ("check valid date leap- 2/29/2020" >:: fun _ ->
      assert_equal true
      (Date.check_date lvd1)
    );
    ("check valid invalid-date leap- 2/29/2022" >:: fun _ ->
      assert_equal false
      (Date.check_date lnv1)
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
    ( "check pp_date - 1/2/2023" >:: fun _ ->
      assert_equal "1/2/2023"
      (Date.pp_date vd1)
    );
    ( "check pp_date - 2/28/2022" >:: fun _ ->
      assert_equal "2/28/2022"
      (Date.pp_date vd2)
    );
    ("check pp_date- 12/31/2021" >:: fun _ ->
      assert_equal "12/31/2021"
      (Date.pp_date vd6)
    );
    ("check pp_date leap- 2/29/2020" >:: fun _ ->
      assert_equal "2/29/2020"
      (Date.pp_date lvd1)
    );
    ( "check pp_date - invalid-date leap" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date lnv1)
    );
    ( "check pp_date - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date nvd1)
    );
    ( "check pp_date - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date nvd2)
    );
    ( "check pp_date - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date nvd3)
    );
    ( "check pp_date - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date nvd4)
    );
    ( "check pp_date - invalid-date " >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.pp_date nvd5)
    );
    ( "check to_string - 1/2/2023" >:: fun _ ->
      assert_equal "1/2/2023" (Date.to_string vd1)
    );
    ( "check pp_date - 2/28/2022" >:: fun _ ->
      assert_equal "2/28/2022"
      (Date.to_string vd2)
    );
    ("check pp_date- 12/31/2021" >:: fun _ ->
      assert_equal "12/31/2021"
      (Date.to_string vd6)
    );
    ("check to_string leap- 2/29/2020" >:: fun _ ->
      assert_equal "2/29/2020"
      (Date.to_string lvd1)
    );
    ( "check to_string - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string nvd1)
    );
    ( "check to_string - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string nvd2)
    );
    ( "check to_string - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string nvd3)
    );
    ( "check to_string - invalid-date" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string nvd4)
    );
    ( "check to_string - invalid-date leap" >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string lnv1)
    );
    ( "check to_string - invalid-date " >:: fun _ ->
      assert_raises (Failure "invalid date") (fun () -> Date.to_string nvd5)
    );
    ("check of_string vd1">:: fun _ -> assert_equal vd1 (Date.of_string ("1/2/2023")));
    ("check of_string vd2">:: fun _ -> assert_equal vd2 (Date.of_string ("2/28/2022")));
    ("check of_string vd3">:: fun _ -> assert_equal vd6 (Date.of_string ("12/31/2021")));

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
      assert_equal (Stock.yf_to_stock "APPL" csv1a) (Stock.yf_to_stock "APPL" csv1b) ~printer: (fun x -> Stock.to_string x));
      ("csv to stock/to_string" >:: fun _ ->
      assert_equal (Stock.to_string(Stock.yf_to_stock "APPL" csv2)) ("APPL: "^(csv2str)) ~printer: (fun x -> x));
  
    ]

let portfolio_tests =
  [
   (* Test for add_asset function *)
   "test make_asset" >:: (fun _ ->
    let example = Asset.make_asset "Example Company" 100.0 50.0 (Date.make_date 1 1 2023) "Technology" in
    assert_equal Asset.example_asset example ~printer:Asset.asset_to_string
    );
  "add_asset" >:: (fun _ ->
    let new_asset = Asset.make_asset "Microsoft" 100.0 50.0 (Date.make_date 1 1 2023) "Technology" in
    let portfolio = Asset.add_asset [] new_asset in
    assert_equal [new_asset] portfolio ~printer:Asset.portfolio_to_string
  );
  (* Test for remove_asset function *)
  "remove_asset" >:: (fun _ ->
    let portfolio = Asset.remove_asset [Asset.example_asset] "Example Company" in
    assert_equal [] portfolio ~printer:Asset.portfolio_to_string
  );
  (* Test for update_asset_quantity function *)
  "update_asset_quantity" >:: (fun _ ->
    let updated_portfolio = Asset.update_asset_quantity [Asset.example_asset] "Example Company" 200.0 in
    let expected_asset = {Asset.example_asset with quantity = 200.0} in
    assert_equal [expected_asset] updated_portfolio ~printer:Asset.portfolio_to_string
  );
      (* Test adding a non-existent asset *)
  "add_non_existent_asset" >:: (fun _ ->
    let non_existent_asset = Asset.make_asset "Non-existent" 50.0 100.0 (Date.make_date 1 1 2022) "Unknown" in
    let portfolio = Asset.add_asset [] non_existent_asset in
    assert_equal [non_existent_asset] portfolio ~printer:Asset.portfolio_to_string
  );
  (* Test removing a non-existent asset from an empty portfolio *)
  "remove_non_existent_asset_empty_portfolio" >:: (fun _ ->
    let portfolio = Asset.remove_asset [] "Non-existent" in
    assert_equal [] portfolio ~printer:Asset.portfolio_to_string
  );
  (* Test removing a non-existent asset from a non-empty portfolio *)
  "remove_non_existent_asset_non_empty_portfolio" >:: (fun _ ->
    let portfolio = Asset.remove_asset [Asset.example_asset] "Non-existent" in
    assert_equal [Asset.example_asset] portfolio ~printer:Asset.portfolio_to_string
  );
  (* Test updating the quantity of a non-existent asset *)
  "update_quantity_non_existent_asset" >:: (fun _ ->
    let portfolio = Asset.update_asset_quantity [Asset.example_asset] "Non-existent" 300.0 in
    assert_equal [Asset.example_asset] portfolio ~printer:Asset.portfolio_to_string
  );
  (* Test adding a non-existent asset *)
  "add_non_existent_asset" >:: (fun _ ->
    let non_existent_asset = Asset.make_asset "Non-existent" 50.0 100.0 (Date.make_date 1 1 2022) "Unknown" in
    let portfolio = Asset.add_asset [] non_existent_asset in
    assert_equal [non_existent_asset] portfolio ~printer:Asset.portfolio_to_string
  );
    (* Test removing a non-existent asset from an empty portfolio *)
  "remove_non_existent_asset_empty_portfolio" >:: (fun _ ->
    let portfolio = Asset.remove_asset [] "Non-existent" in
    assert_equal [] portfolio ~printer:Asset.portfolio_to_string
  );
    (* Test removing a non-existent asset from a non-empty portfolio *)
  "remove_non_existent_asset_non_empty_portfolio" >:: (fun _ ->
    let portfolio = Asset.remove_asset [Asset.example_asset] "Non-existent" in
    assert_equal [Asset.example_asset] portfolio ~printer:Asset.portfolio_to_string
  );
    (* Test updating the quantity of a non-existent asset *)
  "update_quantity_non_existent_asset" >:: (fun _ ->
    let portfolio = Asset.update_asset_quantity [Asset.example_asset] "Non-existent" 300.0 in
    assert_equal [Asset.example_asset] portfolio ~printer:Asset.portfolio_to_string
  );
  (* Test asset_to_string function *)
  "asset_to_string" >:: (fun _ ->
    let test_asset = Asset.make_asset "Test Asset" 150.0 75.0 (Date.make_date 1 1 2023) "Finance" in
    let expected_string = "[Name: Test Asset, Quantity: 150.00, Purchase Price: 75.00, Date Purchased: 1/1/2023, Sector: Finance]" in
    assert_equal expected_string (Asset.asset_to_string test_asset) ~printer:(fun x -> x)
  );
  (* Test portfolio_to_string function with a single asset *)
  "portfolio_to_string_single_asset" >:: (fun _ ->
    let test_asset = Asset.make_asset "Test Asset" 150.0 75.0 (Date.make_date 1 1 2023) "Finance" in
    let test_portfolio = [test_asset] in
    let expected_string = "[Name: Test Asset, Quantity: 150.00, Purchase Price: 75.00, Date Purchased: 1/1/2023, Sector: Finance]" in
    assert_equal expected_string (Asset.portfolio_to_string test_portfolio) ~printer:(fun x -> x)
  );
  (* Test portfolio_to_string function with multiple assets *)
  "portfolio_to_string_multiple_assets" >:: (fun _ ->
    let asset1 = Asset.make_asset "Asset One" 100.0 50.0 (Date.make_date 1 1 2023) "Tech" in
    let asset2 = Asset.make_asset "Asset Two" 200.0 100.0 (Date.make_date 2 1 2023) "Health" in
    let test_portfolio = [asset1; asset2] in
    let expected_string = "[Name: Asset One, Quantity: 100.00, Purchase Price: 50.00, Date Purchased: 1/1/2023, Sector: Tech], [Name: Asset Two, Quantity: 200.00, Purchase Price: 100.00, Date Purchased: 2/1/2023, Sector: Health]" in
    assert_equal expected_string (Asset.portfolio_to_string test_portfolio) ~printer:(fun x -> x);
    assert_equal (Stock.to_string (Stock.datacreation "PEP")) (Stock.to_string (Stock.datacreation "PEP")) ~printer: (fun x -> x) 
  );
  "testing portfolio funcs">:: (fun _ -> 
    let (p1: Asset.portfolio) = [] in
    let a1 = Asset.make_asset "PEP" 2.0 177.0 (Date.make_date 1 5 2022) "F&B" in
    let a2 = (Asset.make_asset "NYT" 1.0 33.0 (Date.make_date 12 2 2023) "News") in
    let p2 = (Asset.add_asset p1 a1) in
    let finalp = Asset.add_asset p2 a2 in
    assert_equal (string_of_float (Asset.best_dollar_asset finalp (Date.make_date 12 8 2023)))
    (string_of_float (Asset.best_dollar_asset finalp (Date.make_date 12 8 2023))) ~printer: (fun x -> x);
    assert_equal 2.0 (Asset.particular_stock_quantity finalp "PEP" (Date.make_date 12 8 2023)) ~printer: (fun x -> string_of_float x);
    );
  ]
let suite =
  "test suite"
  >::: List.flatten [date_tests; candle_tests; stock_tests; portfolio_tests;]
let () = run_test_tt_main suite


