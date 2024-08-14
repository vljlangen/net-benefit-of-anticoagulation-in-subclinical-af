#tornado plot preamble

base.value <- plotdata[c(241,242), c(2,3)]
sensitivity_bounds <- array(c(0.62, 1.36, 0.0105, 0.0106, 0.04256, 0.7,
                              0.79, 2.10, 0.0267, 0.04, 0.09, 0.9), 
                            dim = c(6,2), 
                            dimnames = list(c("Medication effect on Stroke", "Medication effect on Bleed", "Stroke rate", "Major Bleed rate", "Death rate", "Other major bleed proportion"), 
                                            c("Lower", "Upper")))
#iteration <- 10
#Variation vector
tornado_cluster_data <- NULL
for(i in 1:iteration){
  tornado_cluster_temp <- NULL
  for (ul in 1:2){
    for (k in 1:6) {
      effectCoefficients <- c(Stroke = 0.68, Bleed = 1.62)
      total_bleed_rate <- 0.0106
      total_bleed_rate_difference <- effectCoefficients[2] * total_bleed_rate - total_bleed_rate
      stroke_rate <- c(NoNOAC = 1, NOAC = effectCoefficients[1])*0.0105
      bleed_rate <- matrix(c(bleed_proportions * total_bleed_rate,
                             c(0.10, 0.10, 0.8)*total_bleed_rate_difference + bleed_proportions * total_bleed_rate),
                           ncol = 3,
                           byrow = TRUE,
                           dimnames = list(c("No NOAC", "NOAC"),
                                           c("ICH","Subdural","Other major bleed")))
      death_rate <- 0.04256
      afRate <- 0.075
      
      if (k == 1) {
        effectCoefficients <- c(Stroke = sensitivity_bounds[k, ul], Bleed = 1.62)
        stroke_rate <- c(NoNOAC = 1, NOAC = effectCoefficients[1])*0.0105
      }
      
      if (k==2) {
        effectCoefficients <- c(Stroke = 0.68, Bleed = sensitivity_bounds[k, ul])
        total_bleed_rate_difference <- effectCoefficients[2] * total_bleed_rate - total_bleed_rate
        bleed_rate <- matrix(c(bleed_proportions * total_bleed_rate,
                               c(0.10, 0.10, 0.8)*total_bleed_rate_difference + bleed_proportions * total_bleed_rate),
                             ncol = 3,
                             byrow = TRUE,
                             dimnames = list(c("No NOAC", "NOAC"),
                                             c("ICH","Subdural","Other major bleed")))
      }
      
      if (k==3) {
        stroke_rate <- c(NoNOAC = 1, NOAC = effectCoefficients[1])*sensitivity_bounds[k, ul]
      }
      
      if (k==4) {
        total_bleed_rate <- sensitivity_bounds[k, ul]
        total_bleed_rate_difference <- effectCoefficients[2] * total_bleed_rate - total_bleed_rate
        bleed_rate <- matrix(c(bleed_proportions * total_bleed_rate,
                               c(0.1, 0.1, 0.8)*total_bleed_rate_difference + bleed_proportions * total_bleed_rate),
                             ncol = 3,
                             byrow = TRUE,
                             dimnames = list(c("No NOAC", "NOAC"),
                                             c("ICH","Subdural","Other major bleed")))
      }
      
      if (k==5) {
        death_rate <- sensitivity_bounds[k, ul]
      }
      
      if (k==6) {
        bleed_rate <- matrix(c(bleed_proportions * total_bleed_rate,
                               c((1-sensitivity_bounds[k, ul])/2, (1-sensitivity_bounds[k, ul])/2, sensitivity_bounds[k, ul])*total_bleed_rate_difference + bleed_proportions * total_bleed_rate),
                             ncol = 3,
                             byrow = TRUE,
                             dimnames = list(c("No NOAC", "NOAC"),
                                             c("ICH","Subdural","Other major bleed")))
      }
      
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
        initial_state = 2,
        end.NOAC.after.bleeding = decision.to.medicate) #inital state is susceptible
      temp_yes_NOAC <- NULL
      
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
      temp_no_NOAC <- NULL
      for (s in 1:sim) {
        temp_yes_NOAC[s] <- tempdata_yes_NOAC[[s]][length(tempdata_yes_NOAC[[s]][,4]),4]
        temp_no_NOAC[s] <- tempdata_no_NOAC[[s]][length(tempdata_no_NOAC[[s]][,4]),4]
      }
      tornado_cluster_temp <- c(tornado_cluster_temp, (mean(temp_yes_NOAC) - mean(temp_no_NOAC))/12)
    }
  }
  
  tornado_cluster_temp <- array(
    data = tornado_cluster_temp, 
    dim = c(6,2), 
    dimnames = list(c(),
                    c("Lower_Bound", "Upper_Bound")))
  #risks mimimun and maximum
  temp <- rep(NA,2)
  for (ul in 1:2) {
    effectCoefficients <- c(Stroke = sensitivity_bounds[k, ul], Bleed = sensitivity_bounds[k, ul])
    total_bleed_rate <- 0.0106
    total_bleed_rate_difference <- effectCoefficients[2] * total_bleed_rate - total_bleed_rate
    stroke_rate <- c(NoNOAC = 1, NOAC = effectCoefficients[1])*0.0105
    bleed_rate <- matrix(c(bleed_proportions * total_bleed_rate,
                           c(0.1,0.1,0.8)*total_bleed_rate_difference + bleed_proportions * total_bleed_rate),
                         ncol = 3,
                         byrow = TRUE,
                         dimnames = list(c("No NOAC", "NOAC"),
                                         c("ICH","Subdural","Other major bleed")))
    death_rate <- sensitivity_bounds[k, ul]
    afRate <- 0.075
    
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
      initial_state = 2,
      end.NOAC.after.bleeding = decision.to.medicate) #inital state is susceptible
    temp_yes_NOAC <- NULL
    
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
    temp_no_NOAC <- NULL
    for (s in 1:sim) {
      temp_yes_NOAC[s] <- tempdata_yes_NOAC[[s]][length(tempdata_yes_NOAC[[s]][,4]),4]
      temp_no_NOAC[s] <- tempdata_no_NOAC[[s]][length(tempdata_no_NOAC[[s]][,4]),4]
    }
    temp[ul] <- (mean(temp_yes_NOAC) - mean(temp_no_NOAC))/12
  }
  
  tornado_cluster_temp <- rbind(tornado_cluster_temp, temp)
  
  tornado_cluster_temp <- cbind(tornado_cluster_temp, LU_Difference = tornado_cluster_temp[,1] - tornado_cluster_temp[,2])
  tornado_cluster_temp <- cbind(as.data.frame(tornado_cluster_temp), Parameter = c("Medication effect on Stroke", "Medication effect on Bleed", "Stroke rate", "Major Bleed rate", "Death rate", "Other major bleed proportion", "Bleed, Stroke, and mortality"))
  
  tornado_cluster_data <- rbind(tornado_cluster_data, tornado_cluster_temp)
  print(sprintf("Done %.0f%%", i/iteration * 100))
}

#################################################################
###### Plotting region ##########################################

#order.parameters <- tornado_data %>% arrange(LU_Difference) %>%
# mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
#select(Parameter) %>% unlist() %>% levels()

# width of columns in plot (value between 0 and 1)
#width <- 0.95

# get data frame in shape for ggplot and geom_rect
#tornado.2 <- tornado_data %>% 
# gather columns Lower_Bound and Upper_Bound into a single column using gather
#  gather(key='type', value='output.value', Lower_Bound:Upper_Bound) %>%
# just reordering columns
#  select(Parameter, type, output.value, LU_Difference) %>%
# create the columns for geom_rect
#  mutate(Parameter=factor(Parameter, levels=order.parameters),
#         y_min=pmin(output.value, (as.numeric(base.value[1,2]-base.value[2,2]))/12),
#         y_max=pmax(output.value, (as.numeric(base.value[1,2]-base.value[2,2]))/12),
#         x_min=as.numeric(Parameter)-width/2,
#         x_max=as.numeric(Parameter)+width/2)




############################################################################
############################################################################
###                                                                      ###
###.                 SAVE THE CREATED DATA INTO A LIST                   ###
###                                                                      ###
############################################################################
############################################################################


# Save the data frames into a list
tornado_list <- 
  
  list(
    tornado_cluster_data = tornado_cluster_data,
    sim  = sim ,
    iteration  = iteration
  )

