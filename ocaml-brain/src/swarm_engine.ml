(* ocaml-brain/src/swarm_engine.ml *)

open Domainslib

type bot_state = {
  id : int;
  mutable alpha_score : float;
  mutable state_vector : float array;
  mutable quarantined : bool;
}

let num_bots = 1000
let vector_dim = 16

let init_swarm () : bot_state array =
  Array.init num_bots (fun i ->
    {
      id = i;
      alpha_score = 0.0;
      state_vector = Array.make vector_dim 0.0;
      quarantined = false;
    })

let update_bot_state (bot: bot_state) (bid: float) (ask: float) (vol: float) : unit =
  if not bot.quarantined then begin
    let mid = (bid +. ask) /. 2.0 in
    let spread = ask -. bid in
    let noise = (float_of_int (bot.id mod 17) -. 8.0) *. 0.001 in
    let alpha = (sin (mid *. 0.005) *. 0.5) +. (vol *. 0.0001) -. (spread *. 0.05) +. noise in
    bot.alpha_score <- alpha;
    for d = 0 to vector_dim - 1 do
      bot.state_vector.(d) <- bot.state_vector.(d) *. 0.95 +. (alpha *. float_of_int (d + 1) *. 0.01)
    done
  end

let step_swarm (pool: Task.pool) (swarm: bot_state array) (bid: float) (ask: float) (vol: float) : unit =
  Task.parallel_for pool ~start:0 ~finish:(num_bots - 1) ~body:(fun i ->
    update_bot_state swarm.(i) bid ask vol
  )

let () =
  Printf.printf "=== Initializing Multicore OCaml 5.x 1000-Bot Cognitive Swarm ===\n%!";
  let num_domains = min 8 (Domain.recommended_domain_count ()) in
  let pool = Task.setup_pool ~num_additional_domains:(num_domains - 1) () in
  let swarm = init_swarm () in
  Printf.printf "Swarm instantiated: %d bots on %d multicore evaluation domains.\n%!" num_bots num_domains;

  (* Simulation cycle *)
  let tick_bid = 65420.50 in
  let tick_ask = 65421.00 in
  let tick_vol = 125.4 in

  Task.run pool (fun () ->
    step_swarm pool swarm tick_bid tick_ask tick_vol
  );

  let bot_alphas = Array.map (fun b -> b.alpha_score) swarm in
  let decisions = Watcher_core.audit_swarm bot_alphas 0.22 0.015 0.35 450 0.12 in
  
  Printf.printf "Swarm evaluation cycle completed. Evaluating Meta-Watcher Council:\n%!";
  List.iter (fun (d: Watcher_core.watcher_decision) ->
    Printf.printf "[%s] Triggered: %b | Reason: %s\n%!" d.watcher_id d.triggered d.reason
  ) decisions;
  
  Task.teardown_pool pool;
  Printf.printf "Autonomous Quant Swarm cycle cleanly terminated.\n%!"
