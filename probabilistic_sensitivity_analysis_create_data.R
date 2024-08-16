#iteration <- 20
########################################################################
test <- data.frame(Stroke = numeric(iteration),
                   Bleed = numeric(iteration),
                   Value = numeric(iteration))

###normal distribution parameters
#stroke_mean_effect <- (0.5+0.92)/2
#bleed_mean_effect <- (1.05 + 2.5)/2
#stroke_sd <- (0.92 - stroke_mean_effect)/1.96
#bleed_sd <- (2.5 - bleed_mean_effect)/1.96

###log-normal parameters
stroke_mean <- log(0.68)
bleed_mean <- log(1.62)
stroke_sd <- (log(0.92)-stroke_mean)/1.96
bleed_sd <- (log(2.5)-bleed_mean)/1.96
#set.seed(seed)
for (k in 1:iteration) {
  #log-normal distribution
  effectCoefficients <- c(Stroke = exp(rnorm(1, mean = stroke_mean, sd = stroke_sd)),
                          Bleed = exp(rnorm(1, mean = bleed_mean, sd = bleed_sd)))
  #normal distribution
  #effectCoefficients <- c(Stroke = rnorm(1, mean = stroke_mean_effect, sd = stroke_sd), 
  #                        Bleed = rnorm(1, mean = bleed_mean_effect, sd = bleed_sd))
  #uniform distribution
  #effectCoefficients <- c(Stroke = runif(1, min = 0.5, max = 0.92), 
  #                        Bleed = runif(1, min = 1.05, max = 2.5))
  total_bleed_rate_difference <- effectCoefficients[2] * total_bleed_rate - total_bleed_rate
  stroke_rate <- c(NoNOAC = 1, NOAC = effectCoefficients[1])*0.0105
  bleed_rate <- matrix(c(bleed_proportions * total_bleed_rate,
                         c(0.1, 0.1, 0.8)*total_bleed_rate_difference + bleed_proportions * total_bleed_rate),
                       ncol = 3,
                       byrow = TRUE,
                       dimnames = list(c("No NOAC", "NOAC"),
                                       c("ICH","Subdural","Other major bleed")))
  
  rates <- c(death_rate/12, stroke_rate[1]/12, bleed_rate[1,]/12, afRate/12)
  rates_noac <- c(death_rate/12, stroke_rate[2]/12, bleed_rate[2,]/12, afRate/12)
  healthStates_rates_variation <- array(c(rates[1], 1- sum(rates), rates[2:6], #monthly rates without OAC
                                rates_noac[1], 1- sum(rates_noac), rates_noac[2:6]), #monthly rates with OAC
                              dim = c(7,2),
                              dimnames = list(c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation"),
                                              c("No NOAC", "NOAC")))
  
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
  temp_yes_NOAC <- numeric(sim)
  
  tempdata_no_NOAC <- qaly_sampler(
    rate = healthStates_rates_variation, 
    af_rate = healthStates_rates_AF, 
    policy = policy_No_NOAC, 
    severity = event_severity, 
    qaly = qaly, 
    months = 120, 
    size = sim, 
    base_qaly = base_qaly, 
    initial_state = 2,
    end.NOAC.after.bleeding = decision.to.medicate) #inital state is susceptible
  temp_no_NOAC <- numeric(sim)
  for (s in 1:sim) {
    try(temp_yes_NOAC[s] <- tempdata_yes_NOAC[[s]][length(tempdata_yes_NOAC[[s]][,4]),4])
      try(temp_no_NOAC[s] <- tempdata_no_NOAC[[s]][length(tempdata_no_NOAC[[s]][,4]),4])
  }
  #prob_sensitivity_data_Yes_NOAC[k,] <- c(effectCoefficients, Value = mean(temp_yes_NOAC)/12)
  #prob_sensitivity_data_No_NOAC[k,] <- c( effectCoefficients, Value = mean(temp_no_NOAC)/12)
  test[k,] <- c(effectCoefficients, Value = mean(temp_yes_NOAC - temp_no_NOAC)/12)
  print(sprintf("Done %.2f%%", k/iteration*100))
}


############################################################################
############################################################################
###                                                                      ###
###.                 SAVE THE CREATED DATA INTO A LIST                   ###
###                                                                      ###
############################################################################
############################################################################


# Save the data frames into a list
list_of_data_for_probabilistic_sensitivity_analysis <- 
  
  list(
    test = test,
    sim  = sim ,
    iteration  = iteration
  )
