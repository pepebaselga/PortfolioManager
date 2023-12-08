open Module1
open Date 


module Asset = struct
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
  let add_asset (portfolio : portfolio) (asset : asset) : portfolio =
    asset :: portfolio
  (* Remove an asset from the portfolio by its name (could potentially make longer by rewriting filter for a portfolio) *)
  let remove_asset (portfolio : portfolio) (asset_name : string) : portfolio =
    List.filter (fun a -> a.name <> asset_name) portfolio
  (* Update the quantity of an asset in the portfolio *)
  let update_asset_quantity (portfolio : portfolio) (asset_name : string) (new_quantity : float) : portfolio =
    List.map (fun a ->
      if a.name = asset_name then
        { a with quantity = new_quantity }
      else
        a
    ) portfolio
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
end