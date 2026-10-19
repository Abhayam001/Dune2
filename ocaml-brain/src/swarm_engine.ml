(* ocaml-brain/src/swarm_engine.ml *)

open Domainslib

type bot_state = {
  id : int;
  mutable alpha_score : float;
  mutable state_vector : float array;
  mutable weights : float array;          (* Dynamic learning weights for each signal dimension *)
  mutable learning_rate : float;          (* Autonomous step size eta *)
  mutable cumulative_pnl : float;         (* Individual P&L performance track *)
  mutable win_count : int;
  mutable loss_count : int;
  mutable generation : int;               (* Darwinian generation counter *)
  mutable quarantined : bool;
}

let num_bots = 1000
let vector_dim = 16

(* Initialize swarm with randomized individual synaptic weights & learning rates *)
let init_swarm () : bot_state array =
  Random.self_init ();
  Array.init num_bots (fun i ->
    {
      id = i;
      alpha_score = 0.0;
      state_vector = Array.make vector_dim 0.0;
      weights = Array.init vector_dim (fun _ -> (Random.float 2.0 -. 1.0) *. 0.1);
      learning_rate = 0.005 +. Random.float 0.02;
      cumulative_pnl = 0.0;
      win_count = 0;
      loss_count = 0;
      generation = 1;
      quarantined = false;
    })

(* Forward pass: Compute alpha score from dot product of state vector and learned weights *)
let compute_alpha (bot: bot_state) (bid: float) (ask: float) (vol: float) : float =
  let mid = (bid +. ask) /. 2.0 in
  let spread = ask -. bid in
  
  (* Extract micro-signals into 16-dim state vector *)
  bot.state_vector.(0) <- sin (mid *. 0.001);
  bot.state_vector.(1) <- cos (mid *. 0.002);
  bot.state_vector.(2) <- spread *. 10.0;
  bot.state_vector.(3) <- (vol -. 100.0) *. 0.01;
  bot.state_vector.(4) <- (float_of_int (bot.id mod 20) -. 10.0) *. 0.01;
  for d = 5 to vector_dim - 1 do
    bot.state_vector.(d) <- bot.state_vector.(d) *. 0.90 +. (bot.state_vector.(d - 1) *. 0.08)
  done;

  (* Dot product: W . X *)
  let dot = ref 0.0 in
  for d = 0 to vector_dim - 1 do
    dot := !dot +. (bot.state_vector.(d) *. bot.weights.(d))
  done;
  !dot

(* Autonomous Online Reinforcement Learning Step:
   Each bot evaluates its prior prediction against actual price delta and updates weights *)
let learn_from_market (bot: bot_state) (price_delta: float) : unit =
  if not bot.quarantined then begin
    (* Reward is positive if directional prediction matched market price movement *)
    let prediction = bot.alpha_score in
    let reward = prediction *. price_delta in
    bot.cumulative_pnl <- bot.cumulative_pnl +. reward;

    if reward > 0.0 then
      bot.win_count <- bot.win_count + 1
    else if reward < 0.0 then
      bot.loss_count <- bot.loss_count + 1;

    (* Policy Gradient / SGD weight update: W_i <- W_i + eta * reward * X_i *)
    for d = 0 to vector_dim - 1 do
      let grad = reward *. bot.state_vector.(d) in
      bot.weights.(d) <- bot.weights.(d) +. (bot.learning_rate *. grad);
      (* Weight clipping to prevent gradient explosion *)
      bot.weights.(d) <- max (-2.0) (min 2.0 bot.weights.(d))
    done
  end

(* Darwinian Natural Selection:
   Ranks swarm by cumulative P&L. Top bots reproduce; worst bots are eliminated & mutated *)
let natural_selection_evolution (swarm: bot_state array) : (int * float * float) =
  let sorted_bots = Array.copy swarm in
  Array.sort (fun a b -> compare b.cumulative_pnl a.cumulative_pnl) sorted_bots;

  let top_bot = sorted_bots.(0) in
  let worst_bot = sorted_bots.(num_bots - 1) in

  (* Replace the bottom 150 underperforming bots with mutated offspring of top 50 bots *)
  for i = (num_bots - 150) to (num_bots - 1) do
    let parent = sorted_bots.(Random.int 50) in
    let target = sorted_bots.(i) in
    target.generation <- parent.generation + 1;
    target.learning_rate <- max 0.001 (parent.learning_rate +. (Random.float 0.004 -. 0.002));
    target.cumulative_pnl <- 0.0;
    target.win_count <- 0;
    target.loss_count <- 0;
    for d = 0 to vector_dim - 1 do
      (* Genetic Crossover + Mutation *)
      let mutation = (Random.float 0.04 -. 0.02) in
      target.weights.(d) <- parent.weights.(d) +. mutation
    done
  done;

  (top_bot.id, top_bot.cumulative_pnl, worst_bot.cumulative_pnl)

let step_swarm (pool: Task.pool) (swarm: bot_state array) (bid: float) (ask: float) (vol: float) (prior_mid: float) : unit =
  let current_mid = (bid +. ask) /. 2.0 in
  let price_delta = current_mid -. prior_mid in

  Task.parallel_for pool ~start:0 ~finish:(num_bots - 1) ~body:(fun i ->
    let bot = swarm.(i) in
    if prior_mid > 0.0 then
      learn_from_market bot price_delta;
    bot.alpha_score <- compute_alpha bot bid ask vol
  )

let () =
  Printf.printf "\n============================================================\n%!";
  Printf.printf " MULTICORE OCAML 5.x AUTONOMOUS LEARNING QUANT SWARM ENGINE \n%!";
  Printf.printf "============================================================\n%!";
  let num_domains = min 8 (Domain.recommended_domain_count ()) in
  let pool = Task.setup_pool ~num_domains:(max 1 (num_domains - 1)) in
  let swarm = init_swarm () in
  Printf.printf "[SETUP] Initialized 1,000 Self-Learning Bots across %d CPU Domains.\n%!" num_domains;
  Printf.printf "[LEARNING] Online Reinforcement Learning (SGD Policy Gradient) Active.\n%!";
  Printf.printf "[GENETICS] Darwinian Neuroevolutionary Selection Active.\n\n%!";

  (* Simulate 10 sequential live ticks with price movements *)
  let ticks = [
    (65420.00, 65420.50, 150.0);
    (65424.00, 65424.50, 180.0);
    (65428.50, 65429.00, 240.0);
    (65422.00, 65422.50, 310.0);
    (65418.00, 65418.50, 190.0);
    (65425.00, 65425.50, 220.0);
    (65431.00, 65431.50, 280.0);
    (65438.00, 65438.50, 350.0);
    (65432.00, 65432.50, 210.0);
    (65440.00, 65440.50, 400.0);
  ] in

  let prior_price = ref 0.0 in

  List.iteri (fun cycle (bid, ask, vol) ->
    let current_mid = (bid +. ask) /. 2.0 in
    let start_t = Unix.gettimeofday () in

    Task.run pool (fun () ->
      step_swarm pool swarm bid ask vol !prior_price
    );
    let elapsed_ms = (Unix.gettimeofday () -. start_t) *. 1000.0 in

    (* Metrics aggregation *)
    let sum_alpha = ref 0.0 in
    let profitable_bots = ref 0 in
    Array.iter (fun b ->
      sum_alpha := !sum_alpha +. b.alpha_score;
      if b.cumulative_pnl > 0.0 then incr profitable_bots
    ) swarm;

    let avg_alpha = !sum_alpha /. float_of_int num_bots in
    let sentiment = if avg_alpha > 0.01 then "BULLISH" else if avg_alpha < -0.01 then "BEARISH" else "NEUTRAL" in

    Printf.printf "[TICK #%02d] BTC: $%.2f (Δ: %+.2f) | Evaluated & Learned in: %.2f ms\n%!"
      (cycle + 1) current_mid (if !prior_price > 0.0 then current_mid -. !prior_price else 0.0) elapsed_ms;
    Printf.printf "  ├─ Swarm Alpha Consensus: %+.4f [%s]\n%!" avg_alpha sentiment;
    Printf.printf "  ├─ Autonomous Learning  : %d / 1,000 Bots Currently Profitable\n%!" !profitable_bots;
    Printf.printf "  └─ Bot #007 [Gen %d] W0: %+.3f, P&L: %+.4f | Bot #500 [Gen %d] W0: %+.3f, P&L: %+.4f\n%!"
      swarm.(7).generation swarm.(7).weights.(0) swarm.(7).cumulative_pnl
      swarm.(500).generation swarm.(500).weights.(0) swarm.(500).cumulative_pnl;

    prior_price := current_mid;
  ) ticks;

  (* Trigger Natural Selection Epoch *)
  Printf.printf "\n------------------------------------------------------------\n%!";
  Printf.printf "         DARWINIAN NATURAL SELECTION EPOCH EXECUTED         \n%!";
  Printf.printf "------------------------------------------------------------\n%!";
  let (top_id, top_pnl, worst_pnl) = natural_selection_evolution swarm in
  Printf.printf "  ► Alpha Champion: Bot #%d with P&L: %+.4f (Reproduced across swarm)\n%!" top_id top_pnl;
  Printf.printf "  ► Extinction    : 150 Underperforming Bots culled (Lowest P&L: %+.4f)\n%!" worst_pnl;
  Printf.printf "  ► Next Epoch    : Generation 2 instantiated with mutated crossover weights.\n%!";

  (* Run 5 Meta-Watcher Council Audit *)
  Printf.printf "\n------------------------------------------------------------\n%!";
  Printf.printf "          5 META-WATCHER SUPERVISORY COUNCIL AUDIT          \n%!";
  Printf.printf "------------------------------------------------------------\n%!";
  let bot_alphas = Array.map (fun b -> b.alpha_score) swarm in
  let decisions = Watcher_core.audit_swarm bot_alphas 0.18 0.012 0.42 380 0.08 in
  List.iter (fun (d: Watcher_core.watcher_decision) ->
    Printf.printf "  [%s] %s | %s\n%!" d.watcher_id (if d.triggered then "🔴 ACTION" else "🟢 NOMINAL") d.reason
  ) decisions;

  Printf.printf "============================================================\n\n%!";
  Task.teardown_pool pool
