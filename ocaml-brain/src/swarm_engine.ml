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
    let noise = (float_of_int (bot.id mod 23) -. 11.0) *. 0.002 in
    let alpha = (sin (mid *. 0.002 +. float_of_int bot.id) *. 0.4) +. (vol *. 0.0001) -. (spread *. 0.02) +. noise in
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
  Printf.printf "\n============================================================\n%!";
  Printf.printf "     MULTICORE OCAML 5.x 1,000-BOT COGNITIVE SWARM DASHBOARD \n%!";
  Printf.printf "============================================================\n%!";
  let num_domains = min 8 (Domain.recommended_domain_count ()) in
  let pool = Task.setup_pool ~num_additional_domains:(num_domains - 1) () in
  let swarm = init_swarm () in
  Printf.printf "[SETUP] Instantiated %d parallel bot brains across %d CPU Domains.\n%!" num_bots num_domains;

  (* Run 5 simulation ticks *)
  let sample_ticks = [
    (65420.50, 65421.00, 142.5);
    (65425.00, 65425.50, 210.3);
    (65418.20, 65419.00, 95.8);
    (65430.10, 65430.80, 320.1);
    (65435.00, 65435.50, 185.0);
  ] in

  List.iteri (fun cycle (bid, ask, vol) ->
    let start_t = Unix.gettimeofday () in
    Task.run pool (fun () ->
      step_swarm pool swarm bid ask vol
    );
    let elapsed_ms = (Unix.gettimeofday () -. start_t) *. 1000.0 in

    (* Compute Swarm Consensus Metrics *)
    let sum_alpha = ref 0.0 in
    let bullish_count = ref 0 in
    let bearish_count = ref 0 in
    Array.iter (fun b ->
      sum_alpha := !sum_alpha +. b.alpha_score;
      if b.alpha_score > 0.05 then incr bullish_count
      else if b.alpha_score < -0.05 then incr bearish_count
    ) swarm;

    let avg_alpha = !sum_alpha /. float_of_int num_bots in
    let sentiment = if avg_alpha > 0.02 then "AGGRESSIVE BUY" else if avg_alpha < -0.02 then "DEFENSIVE SELL" else "NEUTRAL" in

    Printf.printf "\n[CYCLE #%d] Price: $%.2f | Evaluated in: %.2f ms\n%!" (cycle + 1) ((bid +. ask) /. 2.0) elapsed_ms;
    Printf.printf "  └─ Consensus Alpha : %+.4f (%s)\n%!" avg_alpha sentiment;
    Printf.printf "  └─ Swarm Breakdown : %d Bullish | %d Bearish | %d Neutral Bots\n%!" !bullish_count !bearish_count (num_bots - !bullish_count - !bearish_count);

    (* Sample bot inspection *)
    Printf.printf "  └─ Sample Bot Inspect: [Bot #007: Alpha %+.4f] [Bot #420: Alpha %+.4f] [Bot #999: Alpha %+.4f]\n%!"
      swarm.(7).alpha_score swarm.(420).alpha_score swarm.(999).alpha_score;
  ) sample_ticks;

  (* Run the 5 Meta-Watcher Supervisory Council *)
  Printf.printf "\n------------------------------------------------------------\n%!";
  Printf.printf "           5 META-WATCHER SUPERVISORY COUNCIL AUDIT          \n%!";
  Printf.printf "------------------------------------------------------------\n%!";

  let bot_alphas = Array.map (fun b -> b.alpha_score) swarm in
  let decisions = Watcher_core.audit_swarm bot_alphas 0.22 0.015 0.35 450 0.12 in

  List.iter (fun (d: Watcher_core.watcher_decision) ->
    let status_str = if d.triggered then "🔴 ACTION REQUIRED" else "🟢 NOMINAL" in
    Printf.printf "  [%s] %-18s | Reason: %s\n%!" d.watcher_id status_str d.reason
  ) decisions;

  Printf.printf "============================================================\n\n%!";
  Task.teardown_pool pool
