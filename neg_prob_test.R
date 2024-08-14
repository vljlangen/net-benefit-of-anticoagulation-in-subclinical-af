tempdata_yes_NOAC <- qaly_sampler(
  rate = healthStates_rates_variation, 
  af_rate = healthStates_rates_AF, 
  policy = policy_Yes_NOAC, 
  severity = event_severity, 
  qaly = qaly, 
  months = 120, 
  size = sim, 
  base_qaly = base_qaly, 
  initial_state = 2, #inital state is susceptible
  end.NOAC.after.bleeding = decision.to.medicate)