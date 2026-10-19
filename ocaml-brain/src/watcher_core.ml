(* ocaml-brain/src/watcher_core.ml *)

type system_action =
  | Normal
  | InjectJitter of float
  | QuarantineBots of int list
  | GlobalCircuitBreaker of string

type watcher_decision = {
  watcher_id : string;
  triggered : bool;
  action : system_action;
  reason : string;
}

(* W1: Macro-Risk Sentinel *)
let eval_w1_macro_risk (volatility: float) (drawdown_pct: float) : watcher_decision =
  if drawdown_pct > 0.08 || volatility > 0.65 then
    { watcher_id = "W1: Macro-Risk Sentinel";
      triggered = true;
      action = GlobalCircuitBreaker "Macro tail-risk event / volatility ceiling exceeded";
      reason = Printf.sprintf "Drawdown: %.2f%%, Volatility: %.2f" (drawdown_pct *. 100.0) volatility }
  else
    { watcher_id = "W1: Macro-Risk Sentinel"; triggered = false; action = Normal; reason = "Nominal" }

(* W2: Alpha Decay Auditor *)
let eval_w2_alpha_decay (bot_alphas: float array) : watcher_decision =
  let decayed_indices = ref [] in
  Array.iteri (fun idx alpha ->
    if alpha < -0.15 then decayed_indices := idx :: !decayed_indices
  ) bot_alphas;
  if List.length !decayed_indices > 50 then
    { watcher_id = "W2: Alpha Decay Auditor";
      triggered = true;
      action = QuarantineBots !decayed_indices;
      reason = Printf.sprintf "%d bots exhibited structural alpha decay" (List.length !decayed_indices) }
  else
    { watcher_id = "W2: Alpha Decay Auditor"; triggered = false; action = Normal; reason = "Nominal" }

(* W3: Correlation Warden *)
let eval_w3_correlation (herd_correlation: float) : watcher_decision =
  if herd_correlation > 0.85 then
    { watcher_id = "W3: Correlation Warden";
      triggered = true;
      action = GlobalCircuitBreaker "Herd crowding correlation breach detected (>0.85)";
      reason = Printf.sprintf "Crowding index: %.3f" herd_correlation }
  else
    { watcher_id = "W3: Correlation Warden"; triggered = false; action = Normal; reason = "Nominal" }

(* W4: Latency & Drift Spy *)
let eval_w4_latency_drift (drift_us: int) : watcher_decision =
  if drift_us > 1500 then
    { watcher_id = "W4: Latency & Drift Spy";
      triggered = true;
      action = InjectJitter 2.5;
      reason = Printf.sprintf "System pipeline drift high: %d us" drift_us }
  else
    { watcher_id = "W4: Latency & Drift Spy"; triggered = false; action = Normal; reason = "Nominal" }

(* W5: Adversarial Interceptor *)
let eval_w5_adversarial (spoof_index: float) : watcher_decision =
  if spoof_index > 0.70 then
    { watcher_id = "W5: Adversarial Interceptor";
      triggered = true;
      action = InjectJitter 5.0;
      reason = Printf.sprintf "Adversarial quote stuffing / toxic flow detected: %.2f" spoof_index }
  else
    { watcher_id = "W5: Adversarial Interceptor"; triggered = false; action = Normal; reason = "Nominal" }

let audit_swarm (bot_alphas: float array) (volatility: float) (drawdown: float) (herd_corr: float) (drift_us: int) (spoof: float) : watcher_decision list =
  [
    eval_w1_macro_risk volatility drawdown;
    eval_w2_alpha_decay bot_alphas;
    eval_w3_correlation herd_corr;
    eval_w4_latency_drift drift_us;
    eval_w5_adversarial spoof;
  ]
