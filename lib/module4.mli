(* File: module4.ml *)

open Module1

module BuySell : sig
(* transaction trigger types *)  
    type trigger =
    | Buy
    | Sell
    | Neut
(* single pattern types *)
  type single_patterns =
    | Hammer
    | RevHammer
    | Doji
    | Marubozu
    | NoPattern
(* double pattern types *)
  type double_patterns =
    | EngulfingBull
    | EngulfingBear
    | TweezerTop
    | TweezerBot
    | NoPattern
(* check if candlstick is hammer pattern*)
  val check_hammmer : Date.date * Candlestick.cs -> single_patterns * Date.date
(* check if candlstick is reverse hammer pattern*)
  val check_rev_hammmer : Date.date * Candlestick.cs -> single_patterns * Date.date
(* check if candlstick is doji pattern *)
  val check_doji : Date.date * Candlestick.cs -> single_patterns * Date.date
(* check if candlstick is marubozu pattern *)
  val check_marubozu : Date.date * Candlestick.cs -> single_patterns * Date.date
(* check if candlstick is engulfing pattern *)
  val check_engulfing :
    Date.date * Candlestick.cs -> Date.date * Candlestick.cs -> double_patterns * Date.date
(* check if candlstick is tweezer top pattern *)
  val check_tweezer_top :
    Date.date * Candlestick.cs -> Date.date * Candlestick.cs -> double_patterns * Date.date
(* check if candlstick is tweezer bottom pattern *)
  val check_tweezer_bot :
    Date.date * Candlestick.cs -> Date.date * Candlestick.cs -> double_patterns * Date.date
(* makes a result for testing *)
  val make_res : trigger -> Date.date -> string -> (trigger * Date.date * string) list
(* helper for res_to_string *)
  val r2s_helper : trigger -> Date.date -> string -> string
(* string version of result *)
  val res_to_string : (trigger * Date.date * string) list -> string
(* iterates through stocks and returns triggers *)
  val test_single_patterns : Date.date * Candlestick.cs -> trigger * Date.date * string
(* determines if a candle is a pattern *)
  val find_single_patterns : Stock.stock -> (trigger * Date.date * string) list
(* iterates through stock pairs and returns triggers *)
  val test_duo_patterns :
    Date.date * Candlestick.cs -> Date.date * Candlestick.cs -> trigger * Date.date * string
(* determines if a candlestick pair is a pattern *)
  val find_duo_patterns : Stock.stock -> (trigger * Date.date * string) list
(* iterates through stock triples and returns triggers *)
  val find_tri_patterns : Stock.stock -> (trigger * Date.date * string) list
end