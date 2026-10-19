(* ocaml-brain/src/swarm_engine.ml *)

open Domainslib

type market_type =
  | Crypto
  | US_Equities
  | Indian_NSE
  | Forex
  | Money_Market

type risk_persona =
  | Ultra_Safe
  | Balanced
  | Aggressive_Degen

let market_to_string = function
  | Crypto -> "Crypto"
  | US_Equities -> "US Stocks"
  | Indian_NSE -> "Indian NSE"
  | Forex -> "Forex"
  | Money_Market -> "Money Market"

let persona_to_string = function
  | Ultra_Safe -> "Ultra-Safe"
  | Balanced -> "Balanced"
  | Aggressive_Degen -> "High-Risk"

type bot_state = {
  id : int;
  mutable target_market : market_type;
  mutable risk_style : risk_persona;
  mutable capital : float;                (* Starts with $1,000,000.00 *)
  mutable initial_capital : float;
  mutable leverage : float;               (* Dynamically adjusted based on confidence & risk style *)
  mutable alpha_score : float;
  mutable weights : float array;          (* 16-dimensional synaptic policy *)
  mutable learning_rate : float;
  mutable lifetime_profit : float;
  mutable total_trades : int;
  mutable win_count : int;
  mutable generation : int;
  mutable quarantined : bool;
}

let num_bots = 1000
let vector_dim = 16

(* Randomly assign market specializations & risk appetites on instantiation *)
let init_swarm () : bot_state array =
  Random.self_init ();
  Array.init num_bots (fun i ->
    let market = match i mod 5 with
      | 0 -> Crypto
      | 1 -> US_Equities
      | 2 -> Indian_NSE
      | 3 -> Forex
      | _ -> Money_Market
    in
    let style = match (i / 5) mod 3 with
      | 0 -> Ultra_Safe
      | 1 -> Balanced
      | _ -> Aggressive_Degen
    in
    let initial_lev = match style with
      | Ultra_Safe -> 1.0
      | Balanced -> 2.5
      | Aggressive_Degen -> 5.0
    in
    {
      id = i;
      target_market = market;
      risk_style = style;
      capital = 1_000_000.0;
      initial_capital = 1_000_000.0;
      leverage = initial_lev;
      alpha_score = 0.0;
      weights = Array.init vector_dim (fun _ -> (Random.float 2.0 -. 1.0) *. 0.1);
      learning_rate = 0.005 +. Random.float 0.02;
      lifetime_profit = 0.0;
      total_trades = 0;
      win_count = 0;
      generation = 1;
      quarantined = false;
    })

(* Forward Pass: Evaluate market micro-signals into alpha score *)
let evaluate_strategy (bot: bot_state) (bid: float) (ask: float) (vol: float) : float =
  let mid = (bid +. ask) /. 2.0 in
  let spread = ask -. bid in
  
  let state = Array.make vector_dim 0.0 in
  state.(0) <- sin (mid *. 0.001);
  state.(1) <- cos (mid *. 0.002);
  state.(2) <- spread *. 10.0;
  state.(3) <- (vol -. 100.0) *. 0.01;
  state.(4) <- (float_of_int (bot.id mod 20) -. 10.0) *. 0.01;
  for d = 5 to vector_dim - 1 do
    state.(d) <- state.(d - 1) *. 0.88
  done;

  let dot = ref 0.0 in
  for d = 0 to vector_dim - 1 do
    dot := !dot +. (state.(d) *. bot.weights.(d))
  done;

  (* Risk Scaling: Aggressive bots amplify signal, Safe bots dampen volatility *)
  let multiplier = match bot.risk_style with
    | Ultra_Safe -> 0.4
    | Balanced -> 1.0
    | Aggressive_Degen -> 2.2
  in
  !dot *. multiplier

(* Reinforcement Learning & Capital Reward Function:
   The more profit the bot makes, the more capital bonus & leverage reward it receives *)
let execute_and_reward (bot: bot_state) (price_delta: float) : unit =
  if not bot.quarantined then begin
    let conviction = abs_float bot.alpha_score in
    
    (* Dynamically decide whether to risk capital *)
    let threshold = match bot.risk_style with
      | Ultra_Safe -> 0.18        (* Only trades high-certainty set ups *)
      | Balanced -> 0.08
      | Aggressive_Degen -> 0.03  (* High-frequency risk taker *)
    in

    if conviction > threshold then begin
      bot.total_trades <- bot.total_trades + 1;
      
      (* Position sizing governed by capital and risk style *)
      let risk_pct = match bot.risk_style with
        | Ultra_Safe -> 0.01        (* 1% capital per trade *)
        | Balanced -> 0.05          (* 5% capital *)
        | Aggressive_Degen -> 0.20  (* 20% capital *)
      in

      let position_size = bot.capital *. risk_pct *. bot.leverage in
      let direction = if bot.alpha_score > 0.0 then 1.0 else -1.0 in
      let trade_pnl = position_size *. (price_delta /. 65000.0) *. direction in

      bot.capital <- max 10_000.0 (bot.capital +. trade_pnl);
      bot.lifetime_profit <- bot.lifetime_profit +. trade_pnl;

      (* REINFORCEMENT LEARNING REWARD:
         Winning bots get scaled learning rewards and leverage bonus.
         Losing bots get penalised and de-leveraged. *)
      if trade_pnl > 0.0 then begin
        bot.win_count <- bot.win_count + 1;
        (* Reward Bonus: Increase leverage cap & reward multiplier *)
        bot.leverage <- min 10.0 (bot.leverage *. 1.02);
        for d = 0 to vector_dim - 1 do
          bot.weights.(d) <- bot.weights.(d) +. (bot.learning_rate *. conviction *. 1.5)
        done
      end else begin
        (* Penalty: Reduce leverage & adapt weights *)
        bot.leverage <- max 1.0 (bot.leverage *. 0.95);
        for d = 0 to vector_dim - 1 do
          bot.weights.(d) <- bot.weights.(d) -. (bot.learning_rate *. conviction *. 1.2)
        done
      end;

      (* Market Migration:
         If a bot has lost money 5 times in its current market, it can autonomously
         switch to another market to hunt for better alpha *)
      if (bot.total_trades - bot.win_count) >= 6 && bot.lifetime_profit < 0.0 then begin
        bot.target_market <- match bot.target_market with
          | Crypto -> US_Equities
          | US_Equities -> Indian_NSE
          | Indian_NSE -> Forex
          | Forex -> Money_Market
          | Money_Market -> Crypto
      end
    end
  end

let step_swarm (pool: Task.pool) (swarm: bot_state array) (bid: float) (ask: float) (vol: float) (prior_mid: float) : unit =
  let current_mid = (bid +. ask) /. 2.0 in
  let price_delta = current_mid -. prior_mid in

  Task.parallel_for pool ~start:0 ~finish:(num_bots - 1) ~body:(fun i ->
    let bot = swarm.(i) in
    if prior_mid > 0.0 then
      execute_and_reward bot price_delta;
    bot.alpha_score <- evaluate_strategy bot bid ask vol
  )

let () =
  Printf.printf "\n========================================================================\n%!";
  Printf.printf " MULTICORE OCAML 5.x MULTI-ASSET CAPITAL SWARM (1,000 x $1,000,000 BOTS) \n%!";
  Printf.printf "========================================================================\n%!";
  let num_domains = min 8 (Domain.recommended_domain_count ()) in
  let pool = Task.setup_pool ~num_domains:(max 1 (num_domains - 1)) in
  let swarm = init_swarm () in

  let total_swarm_capital = float_of_int num_bots *. 1_000_000.0 in
  Printf.printf "[SETUP] Initialized 1,000 Individual Bots across %d Parallel Domains.\n%!" num_domains;
  Printf.printf "[CAPITAL] Total Swarm AUM: $%.2f ($1.00 Billion Simulated Capital).\n%!" total_swarm_capital;
  Printf.printf "[MARKETS] Crypto, US Equities, Indian NSE, Forex, Money Market.\n%!";
  Printf.printf "[RISK] Ultra-Safe (1x), Balanced (2.5x), High-Risk Aggressive (5x-10x).\n\n%!";

  let ticks = [
    (65420.00, 65420.50, 150.0);
    (65435.00, 65435.50, 280.0);
    (65415.00, 65415.50, 310.0);
    (65440.00, 65440.50, 420.0);
    (65455.00, 65455.50, 380.0);
  ] in

  let prior_price = ref 0.0 in

  List.iteri (fun cycle (bid, ask, vol) ->
    let current_mid = (bid +. ask) /. 2.0 in
    let start_t = Unix.gettimeofday () in

    Task.run pool (fun () ->
      step_swarm pool swarm bid ask vol !prior_price
    );
    let elapsed_ms = (Unix.gettimeofday () -. start_t) *. 1000.0 in

    let current_total_capital = Array.fold_left (fun acc b -> acc +. b.capital) 0.0 swarm in
    let swarm_net_pnl = current_total_capital -. total_swarm_capital in

    Printf.printf "[CYCLE #%d] Price: $%.2f | Cycle Processed in: %.2f ms\n%!" (cycle + 1) current_mid elapsed_ms;
    Printf.printf "  ├─ Swarm Total AUM : $%.2f (Net P&L: %+.2f)\n%!" current_total_capital swarm_net_pnl;

    (* Sample Bot Inspect across different strategies and markets *)
    let b1 = swarm.(10) in  (* Crypto / Safe *)
    let b2 = swarm.(142) in (* Indian NSE / Aggressive *)
    let b3 = swarm.(505) in (* US Stocks / Balanced *)
    Printf.printf "  ├─ Bot #010 [%s | %s] Cap: $%.2f | Lev: %.1fx | PnL: %+.2f\n%!"
      (market_to_string b1.target_market) (persona_to_string b1.risk_style) b1.capital b1.leverage b1.lifetime_profit;
    Printf.printf "  ├─ Bot #142 [%s | %s] Cap: $%.2f | Lev: %.1fx | PnL: %+.2f\n%!"
      (market_to_string b2.target_market) (persona_to_string b2.risk_style) b2.capital b2.leverage b2.lifetime_profit;
    Printf.printf "  └─ Bot #505 [%s | %s] Cap: $%.2f | Lev: %.1fx | PnL: %+.2f\n\n%!"
      (market_to_string b3.target_market) (persona_to_string b3.risk_style) b3.capital b3.leverage b3.lifetime_profit;

    prior_price := current_mid;
  ) ticks;

  Printf.printf "========================================================================\n";
  Printf.printf "                 SWARM ASSET ALLOCATION & LEADERBOARD                   \n";
  Printf.printf "========================================================================\n";
  
  let sorted_bots = Array.copy swarm in
  Array.sort (fun a b -> compare b.lifetime_profit a.lifetime_profit) sorted_bots;

  Printf.printf "TOP 3 ALPHA CHAMPIONS (Highest Capital Reward):\n";
  for i = 0 to 2 do
    let b = sorted_bots.(i) in
    Printf.printf "  #%d. Bot #%03d [%s | %s] Balance: $%.2f (Profit: %+.2f, WinRate: %.1f%%)\n"
      (i + 1) b.id (market_to_string b.target_market) (persona_to_string b.risk_style)
      b.capital b.lifetime_profit
      (if b.total_trades > 0 then (float_of_int b.win_count /. float_of_int b.total_trades *. 100.0) else 0.0)
  done;

  Task.teardown_pool pool
