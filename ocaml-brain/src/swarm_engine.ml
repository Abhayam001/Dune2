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
  mutable capital : float;
  mutable initial_capital : float;
  mutable leverage : float;
  mutable alpha_score : float;
  mutable weights : float array;
  mutable learning_rate : float;
  mutable lifetime_profit : float;
  mutable total_trades : int;
  mutable win_count : int;
  mutable generation : int;
  mutable quarantined : bool;
}

(* The Meta-Overseer Supreme Brain:
   Maintains macro-experience, cross-market memory, error pattern ledger,
   and dynamic reward/punishment policy learned from observing all 1,000 bots. *)
type meta_overseer_memory = {
  mutable total_inspections : int;
  mutable aggregate_lessons_learned : int;
  mutable market_regime_weights : float array; (* Memory weights for [Crypto; US; NSE; FX; MM] *)
  mutable punishment_severity : float;        (* Dynamically tightened when swarm loses money *)
  mutable reward_generosity : float;          (* Scaled up when swarm hits high win streaks *)
  mutable systemic_drawdown_memory : float;   (* Retains memory of worst macro tail-risks *)
  mutable most_effective_strategy : string;
}

let num_bots = 1000
let vector_dim = 16

let init_overseer () : meta_overseer_memory =
  {
    total_inspections = 0;
    aggregate_lessons_learned = 0;
    market_regime_weights = [| 1.0; 1.0; 1.0; 1.0; 1.0 |];
    punishment_severity = 1.0;
    reward_generosity = 1.0;
    systemic_drawdown_memory = 0.0;
    most_effective_strategy = "Ultra-Safe Mean Reversion";
  }

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

let evaluate_strategy (bot: bot_state) (overseer: meta_overseer_memory) (bid: float) (ask: float) (vol: float) : float =
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

  let style_mult = match bot.risk_style with
    | Ultra_Safe -> 0.4
    | Balanced -> 1.0
    | Aggressive_Degen -> 2.2
  in

  (* Overseer Meta-Modulation:
     The Overseer applies learned macro weights from its memory to throttle or boost each market *)
  let market_idx = match bot.target_market with
    | Crypto -> 0 | US_Equities -> 1 | Indian_NSE -> 2 | Forex -> 3 | Money_Market -> 4
  in
  let macro_multiplier = overseer.market_regime_weights.(market_idx) in

  !dot *. style_mult *. macro_multiplier

(* Overseer Meta-Learning Step:
   Observes all 1,000 bots, measures collective win/loss rates, identifies market flaws,
   and adjusts punishment/reward policies from collective lessons *)
let overseer_audit_and_learn (overseer: meta_overseer_memory) (swarm: bot_state array) : unit =
  overseer.total_inspections <- overseer.total_inspections + 1;
  
  let total_profit = ref 0.0 in
  let win_sum = ref 0 in
  let loss_sum = ref 0 in
  let market_pnl = Array.make 5 0.0 in

  Array.iter (fun b ->
    total_profit := !total_profit +. b.lifetime_profit;
    win_sum := !win_sum + b.win_count;
    loss_sum := !loss_sum + (b.total_trades - b.win_count);
    let m_idx = match b.target_market with
      | Crypto -> 0 | US_Equities -> 1 | Indian_NSE -> 2 | Forex -> 3 | Money_Market -> 4
    in
    market_pnl.(m_idx) <- market_pnl.(m_idx) +. b.lifetime_profit
  ) swarm;

  let total_decisions = !win_sum + !loss_sum in
  overseer.aggregate_lessons_learned <- overseer.aggregate_lessons_learned + total_decisions;

  (* Lesson 1: Market Allocation Adaptation.
     If a market is causing net losses across all its bots, Overseer dampens that market's signal *)
  for m = 0 to 4 do
    if market_pnl.(m) < 0.0 then
      overseer.market_regime_weights.(m) <- max 0.3 (overseer.market_regime_weights.(m) *. 0.96)
    else if market_pnl.(m) > 0.0 then
      overseer.market_regime_weights.(m) <- min 2.0 (overseer.market_regime_weights.(m) *. 1.04)
  done;

  (* Lesson 2: Punishment & Reward Policy Tuning.
     If the swarm is bleeding capital, the Overseer ramps up punishment severity.
     If the swarm is generating strong alpha, Overseer increases reward generosity. *)
  if !total_profit < 0.0 then begin
    overseer.punishment_severity <- min 3.0 (overseer.punishment_severity *. 1.05);
    overseer.reward_generosity <- max 0.5 (overseer.reward_generosity *. 0.95);
    overseer.systemic_drawdown_memory <- min (!total_profit) overseer.systemic_drawdown_memory
  end else begin
    overseer.punishment_severity <- max 0.7 (overseer.punishment_severity *. 0.98);
    overseer.reward_generosity <- min 2.5 (overseer.reward_generosity *. 1.02)
  end

(* Individual Bot Execution:
   Punishment and Reward multipliers are directly governed by the Overseer's learned policy *)
let execute_and_reward (bot: bot_state) (overseer: meta_overseer_memory) (price_delta: float) : unit =
  if not bot.quarantined then begin
    let conviction = abs_float bot.alpha_score in
    let threshold = match bot.risk_style with
      | Ultra_Safe -> 0.18
      | Balanced -> 0.08
      | Aggressive_Degen -> 0.03
    in

    if conviction > threshold then begin
      bot.total_trades <- bot.total_trades + 1;
      let risk_pct = match bot.risk_style with
        | Ultra_Safe -> 0.01
        | Balanced -> 0.05
        | Aggressive_Degen -> 0.20
      in

      let position_size = bot.capital *. risk_pct *. bot.leverage in
      let direction = if bot.alpha_score > 0.0 then 1.0 else -1.0 in
      let trade_pnl = position_size *. (price_delta /. 65000.0) *. direction in

      bot.capital <- max 10_000.0 (bot.capital +. trade_pnl);
      bot.lifetime_profit <- bot.lifetime_profit +. trade_pnl;

      (* OVERSEER GOVERNED REWARD & PUNISHMENT:
         The Overseer's learned parameters modulate the magnitude of reinforcement! *)
      if trade_pnl > 0.0 then begin
        bot.win_count <- bot.win_count + 1;
        let bonus = overseer.reward_generosity in
        bot.leverage <- min 10.0 (bot.leverage *. (1.0 +. (0.02 *. bonus)));
        for d = 0 to vector_dim - 1 do
          bot.weights.(d) <- bot.weights.(d) +. (bot.learning_rate *. conviction *. 1.5 *. bonus)
        done
      end else begin
        let penalty = overseer.punishment_severity in
        bot.leverage <- max 1.0 (bot.leverage *. (1.0 -. (0.05 *. penalty)));
        for d = 0 to vector_dim - 1 do
          bot.weights.(d) <- bot.weights.(d) -. (bot.learning_rate *. conviction *. 1.2 *. penalty)
        done
      end;

      (* Market Migration *)
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

let step_swarm (pool: Task.pool) (swarm: bot_state array) (overseer: meta_overseer_memory) (bid: float) (ask: float) (vol: float) (prior_mid: float) : unit =
  let current_mid = (bid +. ask) /. 2.0 in
  let price_delta = current_mid -. prior_mid in

  Task.parallel_for pool ~start:0 ~finish:(num_bots - 1) ~body:(fun i ->
    let bot = swarm.(i) in
    if prior_mid > 0.0 then
      execute_and_reward bot overseer price_delta;
    bot.alpha_score <- evaluate_strategy bot overseer bid ask vol
  )

let () =
  Printf.printf "\n========================================================================\n%!";
  Printf.printf " MULTICORE OCAML 5.x HIERARCHICAL SWARM: OVERSEER BRAIN + 1,000 BOT CORES \n%!";
  Printf.printf "========================================================================\n%!";
  let num_domains = min 8 (Domain.recommended_domain_count ()) in
  let pool = Task.setup_pool ~num_domains:(max 1 (num_domains - 1)) in
  let swarm = init_swarm () in
  let overseer = init_overseer () in

  let total_swarm_capital = float_of_int num_bots *. 1_000_000.0 in
  Printf.printf "[SETUP] Initialized 1,000 Bots ($1.00 Billion Capital) on %d CPU Domains.\n%!" num_domains;
  Printf.printf "[META-OVERSEER] Master Overseer Cognitive Core Online.\n";
  Printf.printf "  └─ Adaptive Punishment Severity: %.2fx | Reward Generosity: %.2fx\n\n%!"
    overseer.punishment_severity overseer.reward_generosity;

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
      step_swarm pool swarm overseer bid ask vol !prior_price
    );
    let elapsed_ms = (Unix.gettimeofday () -. start_t) *. 1000.0 in

    (* Overseer evaluates whole swarm and updates its master lessons *)
    overseer_audit_and_learn overseer swarm;

    let current_total_capital = Array.fold_left (fun acc b -> acc +. b.capital) 0.0 swarm in
    let swarm_net_pnl = current_total_capital -. total_swarm_capital in

    Printf.printf "[CYCLE #%d] BTC: $%.2f | Cycle Processed in: %.2f ms\n%!" (cycle + 1) current_mid elapsed_ms;
    Printf.printf "  ├─ Swarm Total AUM      : $%.2f (Net P&L: %+.2f)\n%!" current_total_capital swarm_net_pnl;
    Printf.printf "  ├─ Overseer Experience  : %d Total Lessons Learned from 1,000 Bots\n%!" overseer.aggregate_lessons_learned;
    Printf.printf "  ├─ Overseer Policy State: Punishment: %.2fx | Reward Bonus: %.2fx\n%!"
      overseer.punishment_severity overseer.reward_generosity;
    Printf.printf "  └─ Market Regime Weights: [Crypto: %.2fx | US: %.2fx | NSE: %.2fx | FX: %.2fx | MM: %.2fx]\n\n%!"
      overseer.market_regime_weights.(0) overseer.market_regime_weights.(1)
      overseer.market_regime_weights.(2) overseer.market_regime_weights.(3)
      overseer.market_regime_weights.(4);

    prior_price := current_mid;
  ) ticks;

  Printf.printf "========================================================================\n";
  Printf.printf "              OVERSEER BRAIN GLOBAL SUMMARY & VERDICT                  \n";
  Printf.printf "========================================================================\n";
  Printf.printf "  Total Bot Decisions Analyzed : %d\n" overseer.aggregate_lessons_learned;
  Printf.printf "  Overseer Adaptive Discipline : %s (Severity: %.2fx)\n"
    (if overseer.punishment_severity > 1.2 then "Strict Quarantine Mode" else "Growth Encouragement Mode")
    overseer.punishment_severity;
  Printf.printf "  Systemic Tail-Risk Memory    : $%.2f peak drawdown observed\n" overseer.systemic_drawdown_memory;
  Printf.printf "========================================================================\n\n";

  Task.teardown_pool pool
