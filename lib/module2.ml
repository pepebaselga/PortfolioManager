module Asset = struct
  type asset = {
    name: string;              (* Asset name *)
    quantity: float;           (* Quantity of the asset owned *)
    purchase_price: float;     (* Purchase price of the asset *)
    current_price: float;      (* Current market price of the asset *)
    asset_type: string;        (* Type of the asset, e.g., stock, bond *)
    sector: string;            (* Sector the asset belongs to *)
  }
  (* Prrtfolio type is a list of assets *)
  type portfolio = asset list
  (* Example of creating an asset *)
  let example_asset = {
    name = "Example Company";
    quantity = 100.0;
    purchase_price = 50.0;
    current_price = 55.0;
    asset_type = "Stock";
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
end