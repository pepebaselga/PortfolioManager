open Module1

module BuySell = struct
  type trigger =
    | Buy
    | Sell
    | Neut

  type single_patterns =
    | Hammer
    | RevHammer
    | Doji
    | Marubozu
    | NoPattern

  type double_patterns =
    | EngulfingBull
    | EngulfingBear
    | TweezerTop
    | TweezerBot
    | NoPattern

  let check_hammmer (cs : Date.date * Candlestick.cs) =
    let date, c = cs in
    let body_size = abs_float (c.closep -. c.openp) in
    let range = abs_float (c.highp -. c.lowp) in
    let lower_shadow_size = abs_float (c.lowp -. min c.openp c.closep) in
    let upper_shadow_size = abs_float (max c.openp c.closep -. c.highp) in
    if
      lower_shadow_size /. body_size >= 2.
      && lower_shadow_size > upper_shadow_size
      && body_size > range *. 0.05
    then (Hammer, date)
    else (NoPattern, date)

  let check_rev_hammmer (cs : Date.date * Candlestick.cs) =
    let date, c = cs in
    let body_size = abs_float (c.closep -. c.openp) in
    let lower_shadow_size = abs_float (c.lowp -. min c.openp c.closep) in
    let upper_shadow_size = abs_float (max c.openp c.closep -. c.highp) in
    let range = abs_float (c.highp -. c.lowp) in
    if
      upper_shadow_size /. body_size >= 2.
      && lower_shadow_size < upper_shadow_size
      && body_size > range *. 0.05
    then (RevHammer, date)
    else (NoPattern, date)

  let check_doji (cs : Date.date * Candlestick.cs) =
    let date, c = cs in
    let body_size = abs_float (c.closep -. c.openp) in
    let range = abs_float (c.highp -. c.lowp) in
    if body_size <= range *. 0.05 && body_size > 0. then (Doji, date)
    else (NoPattern, date)

  let check_marubozu (cs : Date.date * Candlestick.cs) =
    let date, c = cs in
    let body_size = abs_float (c.closep -. c.openp) in
    let range = abs_float (c.highp -. c.lowp) in
    if body_size >= range *. 0.9 then (Marubozu, date) else (NoPattern, date)

  let check_engulfing
      (f : Date.date * Candlestick.cs)
      (s : Date.date * Candlestick.cs) =
    let date, fc = f in
    let date1, sc = s in
    let f_col = Candlestick.get_color fc in
    let s_col = Candlestick.get_color sc in
    if f_col = Red && s_col = Green then
      let first_top = fc.openp in
      let first_bot = fc.closep in
      let second_top = fc.closep in
      let second_bot = fc.openp in
      let crit = first_top > second_top && first_bot < second_bot in
      if crit then (EngulfingBull, date1) else (NoPattern, date1)
    else if f_col = Green && s_col = Red then
      let first_top, first_bot = (fc.closep, fc.openp) in
      let second_top, second_bot = (fc.openp, fc.closep) in
      let crit = first_top > second_top && first_bot < second_bot in
      if crit then (EngulfingBear, date1) else (NoPattern, date1)
    else (NoPattern, date1)

  let check_tweezer_top
      (f : Date.date * Candlestick.cs)
      (s : Date.date * Candlestick.cs) =
    let date, fc = f in
    let date1, sc = s in
    let f_high = fc.highp in
    let s_high = sc.highp in
    let range = f_high *. 0.05 in
    if
      abs_float (f_high -. s_high) < range
      && Candlestick.get_color fc = Green
      && Candlestick.get_color sc = Red
    then (TweezerTop, date1)
    else (NoPattern, date1)

  let check_tweezer_bot
      (f : Date.date * Candlestick.cs)
      (s : Date.date * Candlestick.cs) =
    let date, fc = f in
    let date1, sc = s in
    let f_low = fc.lowp in
    let s_low = sc.lowp in
    let range = f_low *. 0.05 in
    if abs_float (f_low -. s_low) < range then (TweezerTop, date1)
    else (NoPattern, date1)

  let make_res t d s : (trigger * Date.date * string) list = [ (t, d, s) ]

  let r2s_helper h d s =
    match h with
    | Buy -> "Buy" ^ ", " ^ Date.to_string d ^ ", " ^ s ^ "\n"
    | Sell -> "Sell" ^ ", " ^ Date.to_string d ^ ", " ^ s ^ "\n"
    | Neut -> "Neutral" ^ ", " ^ Date.to_string d ^ ", " ^ s ^ "\n"

  let rec res_to_string (l : (trigger * Date.date * string) list) : string =
    match l with
    | [] -> ""
    | [ (h, d, s) ] -> r2s_helper h d s
    | (h, d, s) :: t -> res_to_string [ (h, d, s) ] ^ res_to_string t

  let test_single_patterns (c : Date.date * Candlestick.cs) =
    let d, candle = c in
    let col = Candlestick.get_color candle in
    let res, date = check_hammmer c in
    let res1, date = check_rev_hammmer c in
    let res2, date = check_doji c in
    let res3, date = check_marubozu c in
    if res = Hammer then (Buy, date, "Hammer")
    else if res1 = RevHammer then (Buy, date, "RevHammer")
    else if res2 = Doji then (Sell, date, "Doji")
    else if res3 = Marubozu && col = Green then (Buy, date, "Marubozu(Bullish)")
    else if res3 = Marubozu && col = Red then (Sell, date, "Marubozu(Bearish)")
    else (Neut, date, "NoPattern")

  let rec find_single_patterns (stock : Stock.stock) =
    match stock with
    | n, [] -> []
    | n, (date, d) :: t ->
        [ test_single_patterns (date, d) ] @ find_single_patterns (n, t)

  let test_duo_patterns
      (f : Date.date * Candlestick.cs)
      (s : Date.date * Candlestick.cs) =
    let res, date = check_engulfing f s in
    if res = EngulfingBull then (Buy, date, "Engulfing(Bullish)")
    else if res = EngulfingBear then (Sell, date, "Engulfing(Bearish)")
    else (Neut, date, "NoPattern")

  let rec find_duo_patterns (stock : Stock.stock) =
    match stock with
    | n, [] -> []
    | n, [ _ ] -> []
    | n, (date1, d1) :: (date2, d2) :: t ->
        [ test_duo_patterns (date1, d1) (date2, d2) ] @ find_duo_patterns (n, t)

  (* let rec find_tri_patterns (stock : Stock.stock) = match stock with | n,[]
     -> [] | n,[_] -> [] | n,[_;_] -> [] | n,((date1,d1) :: (date2,d2) ::
     (date3,d3) :: t) -> [test_tri_patterns (date1,d1) (date2,d2) (date3,d3)] @
     find_tri_patterns (n,t) *)
  let rec find_tri_patterns (stock : Stock.stock) = []
end
