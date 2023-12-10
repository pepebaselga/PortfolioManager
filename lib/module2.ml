open Module1
open Module3
open Date 



module Asset = struct
  let stock_data: Stock.stock list = Index.load_data 

  type asset = {
    name: string;              (* Asset name *)
    quantity: float;           (* Quantity of the asset owned *)
    purchase_price: float;     (* Purchase price of the asset *)
    date_purchased: date;    (* Current market price of the asset *)
    sector: string;            (* Sector the asset belongs to *)
  }
  (* Prrtfolio type is a list of assets *)
  type portfolio = asset list
  (* Example of creating an asset *)
  let example_asset = {
    name = "Example Company";
    quantity = 100.0;
    purchase_price = 50.0;
    date_purchased =  make_date 1 1 2023;
    sector = "Technology";
  }
  (* Example of creating a portfolio with the example asset *)
  let example_portfolio = [example_asset]
  (* Add an asset to the portfolio *)
  
  (*Test*)
  let assets_in_portfolio (portfolio : portfolio) = 
    List.map (fun x -> x.name) portfolio
  let portfolio_mem (portfolio : portfolio) (asset : asset) = 
    let names = assets_in_portfolio portfolio in
    List.mem asset.name names 
  (*Test*)
  let update_asset_quantity (portfolio : portfolio) (asset_name : string) (new_quantity : float) : portfolio =
    List.map (fun a ->
      if a.name = asset_name then
        { a with quantity = new_quantity }
      else
        a
    ) portfolio

  let add_asset (portfolio : portfolio) (asset : asset) : portfolio =
    asset :: portfolio
  (* Remove an asset from the portfolio by its name (could potentially make longer by rewriting filter for a portfolio) *)
  let remove_asset (portfolio : portfolio) (asset_name : string) (day_bought: date) : portfolio =
    List.filter (fun a -> a.name <> asset_name || a.date_purchased <> day_bought) portfolio
  (* Update the quantity of an asset in the portfolio *)
  let make_asset (name : string) (quantity: float) (purchase_price : float) (date_purchased : date) (sector : string) : asset =
    { name = name;
    quantity = quantity;
    purchase_price = purchase_price;
    date_purchased = date_purchased;
    sector = sector; }
    let asset_to_string asset =
      Printf.sprintf "[Name: %s, Quantity: %.2f, Purchase Price: %.2f, Date Purchased: %s, Sector: %s]"
        asset.name
        asset.quantity
        asset.purchase_price
        (Date.to_string asset.date_purchased)
        asset.sector
    let portfolio_to_string portfolio =
      let asset_strings = List.map asset_to_string portfolio in
       String.concat ", " asset_strings

  (*Untested Functions*)
  
  let find_asset (asset_name:string) = 
    List.find (fun x -> 
      let stock_name,_ = x in if stock_name = asset_name then true else false) stock_data

  
  let best_dollar_asset (portfolio: portfolio) (day: date) = 
    let first,first_bought = find_asset (List.nth portfolio 0).name, (List.nth portfolio 0).date_purchased in 
    let max = ref ((List.nth portfolio 0).name, (Stock.get_dollar_diff first ) first_bought day) in 
    List.iter (fun x -> 
      let stock =  find_asset (x.name) in 
      let dollar_dif = Stock.get_dollar_diff stock x.date_purchased day in
      let name,dv = !max in
      (if dollar_dif > dv then (max := (name,dv)) else ())
      ) portfolio;!max
      

  let best_percent_asset (portfolio: portfolio) (day: date) = 
    let first,first_bought = find_asset (List.nth portfolio 0).name, (List.nth portfolio 0).date_purchased in 
    let max = ref ((List.nth portfolio 0).name, (Stock.get_percent_diff first ) first_bought day) in 
    List.iter (fun x -> 
      let stock =  find_asset (x.name) in 
      let percent_dif = Stock.get_percent_diff stock x.date_purchased day in
      let name,pv = !max in
      (if percent_dif > pv then (max := (name,pv)) else ())
      ) portfolio;!max

  let worst_dollar_asset (portfolio: portfolio) (day: date) = 
    let first,first_bought = find_asset (List.nth portfolio 0).name, (List.nth portfolio 0).date_purchased in 
    let min = ref ((List.nth portfolio 0).name, (Stock.get_dollar_diff first ) first_bought day) in 
    List.iter (fun x -> 
      let stock =  find_asset (x.name) in 
      let dollar_dif = Stock.get_dollar_diff stock x.date_purchased day in
      let name,dv = !min in
      (if dollar_dif < dv then (min := (name,dv)) else ())
      ) portfolio;!min

  let worst_percent_asset (portfolio: portfolio) (day: date) = 
    let first,first_bought = find_asset (List.nth portfolio 0).name, (List.nth portfolio 0).date_purchased in 
    let min = ref ((List.nth portfolio 0).name, (Stock.get_percent_diff first ) first_bought day) in 
    List.iter (fun x -> 
      let stock =  find_asset (x.name) in 
      let dollar_dif = Stock.get_dollar_diff stock x.date_purchased day in
      let name,pv = !min in
      (if dollar_dif < pv then (min := (name,pv)) else ())
      ) portfolio;!min
  let total_portfolio_value (portfolio: portfolio) (day: date) = 
    let count = ref 0.0 in 
    List.iter (fun x -> 
      let _,stock_v =  Stock.find_date (find_asset (x.name)) day in 
      let dollar_v = (x.quantity *. stock_v.closep) in 
      count:= (!count +. dollar_v)
      ) portfolio;!count 

  let total_dollarc(portfolio: portfolio) (day: date) = 
    let min = ref 0.0 in
    List.iter (fun x -> 
      let stock =  find_asset (x.name) in 
      let dollar_dif = ((x.quantity) *. (Stock.get_dollar_diff stock x.date_purchased day)) in 
      min:= (!min +. dollar_dif)
      ) portfolio;!min
  let total_asset_pc (portfolio: portfolio) (day: date) = 
    let min = ref 0.0 in
    List.iter (fun x -> 
      let stock =  find_asset (x.name) in 
      let percent_dif = Stock.get_percent_diff stock x.date_purchased day in 
      min:= (!min +. percent_dif)
      ) portfolio; 
      !min
  let perasset_dollarc (portfolio: portfolio) (day: date) = 
    let count = ref [] in 
    List.iter (fun x ->
      let stock =  find_asset (x.name) in 
      let dollar_dif = ((x.quantity) *. (Stock.get_dollar_diff stock x.date_purchased day)) in  
      count:= (x.name,dollar_dif)::!count) portfolio; 
    !count
  let perasset_percentc (portfolio: portfolio) (day: date) = 
    let count = ref [] in 
    List.iter (fun x ->
      let stock =  find_asset (x.name) in 
      let percent = (Stock.get_percent_diff stock x.date_purchased day) in  
      count:= (x.name,percent)::!count) portfolio; 
    !count
  let total_asset_quantity (portfolio: portfolio): (string*float) list = 
    let count = ref [] in 
    List.iter (fun x -> count:= (x.name,x.quantity)::!count) portfolio; 
    !count
  let find_list_sectors (portfolio: portfolio) = 
    let count = ref [] in 
    List.iter (fun x -> count:= (x.name,x.sector)::!count) portfolio; 
    !count
  
  let find_sector_pc (portfolio: portfolio) (sector: string)(day: date) = 
    let count = ref 0.0 in 
    List.iter (fun x -> if x.sector = sector then 
    let stock =  find_asset (x.name) in 
    let percent = (Stock.get_percent_diff stock x.date_purchased day) in 
    count := percent +. !count) portfolio;
    !count
  let find_restofsectors_pc (portfolio: portfolio) (sector: string)(day: date) = 
    let count = ref 0.0 in 
    List.iter (fun x -> if x.sector != sector then
    let stock =  find_asset (x.name) in 
    let percent = (Stock.get_percent_diff stock x.date_purchased day) in 
    count := percent +. !count) portfolio;
    !count
  
  let find_sector_dc (portfolio: portfolio) (sector: string)(day: date) = 
    let count = ref 0.0 in 
    List.iter (fun x -> if x.sector = sector then 
    let stock =  find_asset (x.name) in 
    let dollar = (x.quantity *. Stock.get_dollar_diff stock x.date_purchased day) in 
    count := dollar +. !count) portfolio;
    !count
  let find_restofsectors_dc (portfolio: portfolio) (sector: string)(day: date) = 
    let count = ref 0.0 in 
    List.iter (fun x -> if x.sector != sector then
    let stock =  find_asset (x.name) in 
    let dollar = (Stock.get_dollar_diff stock x.date_purchased day) in 
    count := dollar +. !count) portfolio;
    !count
  
  let find_multisector_pc (portfolio: portfolio) (sectors: string list)(day: date) = 
    let count = ref 0.0 in 
    List.iter (fun x -> if ((List.mem x.sector sectors) = true) then 
    let stock =  find_asset (x.name) in 
    let percent = (Stock.get_percent_diff stock x.date_purchased day) in 
    count := percent +. !count) portfolio;
    !count
  let find_multisector_dc (portfolio: portfolio) (sectors: string list)(day: date) = 
    let count = ref 0.0 in 
    List.iter (fun x -> if ((List.mem x.sector sectors) = true) then 
    let stock =  find_asset (x.name) in 
    let dollar = (Stock.get_dollar_diff stock x.date_purchased day) in 
    count := dollar +. !count) portfolio;
    !count

  let particular_stock_value (portfolio: portfolio) (asset_name: string) (day:date) =
    let count = ref 0.0 in 
    List.iter (fun x -> if x.name = asset_name then 
    let stock =  find_asset (x.name) in 
    let _,eod_value = Stock.find_date stock day in
    let dollar = (x.quantity *.eod_value.closep) in 
    count := dollar +. !count) portfolio;
    !count
  
  let particular_stock_quantity (portfolio: portfolio) (asset_name: string) (day:date) =
    let count = ref 0.0 in 
    List.iter (fun x -> if x.name = asset_name then 
    count:= !count +. x.quantity ) portfolio;
    !count

end