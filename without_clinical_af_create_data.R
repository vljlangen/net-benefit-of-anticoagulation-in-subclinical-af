effectCoefficients <- c(Stroke = 0.68, Bleed = 1.62)
total_bleed_rate <- 0.0106
bleed_proportions <- c(0.0018, 0.0015, 0.0073)/total_bleed_rate
total_bleed_rate_difference <- effectCoefficients[2] * total_bleed_rate - total_bleed_rate
stroke_rate <- c(NoNOAC = 1, NOAC = effectCoefficients[1])*0.0105
bleed_rate <- matrix(c(bleed_proportions * total_bleed_rate,
                      c(0.1, 0.1, 0.8)*total_bleed_rate_difference + bleed_proportions * total_bleed_rate),
                    ncol = 3,
                    byrow = TRUE,
                    dimnames = list(c("No NOAC", "NOAC"),
                                    c("ICH","Subdural","Other major bleed")))
death_rate <- 0.04256
afRate <- 0

rates <- c(death_rate/12, stroke_rate[1]/12, bleed_rate[1,]/12, afRate/12)
rates_noac <- c(death_rate/12, stroke_rate[2]/12, bleed_rate[2,]/12, afRate/12)
healthStates_rates <- array(c(rates[1], 1- sum(rates), rates[2:6], #monthly rates without OAC
                                        rates_noac[1], 1- sum(rates_noac), rates_noac[2:6]), #monthly rates with OAC
                                      dim = c(7,2),
                                      dimnames = list(c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation"),
                                                      c("No NOAC", "NOAC")))

healthStates_rates_AF <- c(0.116533816/12, 0.985536, 0.025317808/12, 0.005012542/12, 0.005012542/12, 0.028399475/12)

source("vakavuusasteet.R")

#probabilities for severity of observed health state
event_severity <- array(c(data[1,1], data[2,1], data[3,1], data[4,1], 0, 
                          data[5,1], data[6,1], data[7,1], data[8,1], 0, 
                          data[9,1] , data[10,1], 0, data[11,1], data[12,1], 
                          data[13,1], data[14,1], 0, data[15,1], data[16,1],
                          
                          data[1,2], data[2,2], data[3,2], data[4,2], 0, 
                          data[5,2], data[6,2], data[7,2], data[8,2], 0, 
                          data[9,2] , data[10,2], 0, data[11,2], data[12,2], 
                          data[13,2], data[14,2], 0, data[15,2], data[16,2]),
                        dim = c(5,4,2),
                        dimnames = list(c("Death", "Severe disability", "Moderate disability", "Mild disability", "No disability"),
                                        c("Ischemic stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding"),
                                        c("No NOAC", "NOAC")))


severity_qaly <- array(c(0, 0.16, 0.60, 0.88, 1,
                         0, 0.45, 0.73, 0.89, 1),
                       dim = c(5,2),
                       dimnames = list(c("Death", "Severe disability", "Moderate disability", "Mild disability", "No disability"),
                                       c("until 6 months after event", "from 6 months after event")))

base_qaly <- c(rep(0.794,36), rep(0.733, 61-36))
qaly <- severity_qaly%o%rep(1,61) #outer product to define life long qalys

policy_No_NOAC <- array(c(1, 0,
                         1, 0,
                         1, 0,
                         1, 0,
                         1, 0,
                         1, 0,
                         0, 1),
                       dim = c(2,7),
                       dimnames = list(c("No NOAC", "NOAC"),
                                       c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation")
                ))

policy_Yes_NOAC <- array(c(0, 1,
                           0, 1,
                           0, 1,
                           0, 1,
                           0, 1,
                           0, 1,
                           0, 1),
                         dim = c(2,7),
                         dimnames = list(c("No NOAC", "NOAC"),
                                         c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation")
                         ))

data_No_NOAC <- qaly_sampler(
  rate = healthStates_rates, 
  af_rate = healthStates_rates_AF, 
  policy = policy_No_NOAC, 
  severity = event_severity, 
  qaly = qaly, 
  months = 60, 
  size = sim, 
  base_qaly = base_qaly, 
  initial_state = 2,
  seed = seed,
  end.NOAC.after.bleeding = decision.to.medicate) #inital state is susceptible
tempdata <- data_No_NOAC
tempdf <- data.frame(Time = 0:60)
for (s in 1:sim) {
    temp <- rep(NA, 61)
    temp[1:length(tempdata[[s]][,4][tempdata[[s]][,3] != 0])] <- tempdata[[s]][,4][tempdata[[s]][,3] != 0]
    tempdf[{{paste0("simulation",s)}}] <- temp
    rm(temp)
}
tempdata <- tempdf; rm(tempdf)
tempdata <- tempdata %>% pivot_longer(cols = starts_with("simulation"), names_to = "simulation", values_to = "QALY")
simudata_long <- tempdata %>% mutate(Group = "No NOAC")


data_Yes_NOAC <- qaly_sampler(
  rate = healthStates_rates, 
  af_rate = healthStates_rates_AF, 
  policy = policy_Yes_NOAC, 
  severity = event_severity, 
  qaly = qaly, 
  months = 60, 
  size = sim, 
  base_qaly = base_qaly, 
  initial_state = 2,
  seed = seed,
  end.NOAC.after.bleeding = decision.to.medicate) # initial state is suscepctible without any morbidities
tempdata <- data_Yes_NOAC
tempdf <- data.frame(Time = 0:60)
for (s in 1:sim) {
  temp <- rep(NA, 61)
  temp[1:length(tempdata[[s]][,4][tempdata[[s]][,3] != 0])] <- tempdata[[s]][,4][tempdata[[s]][,3] != 0]
  tempdf[{{paste0("simulation",s)}}] <- temp
  rm(temp)
}
tempdata <- tempdf; rm(tempdf)
tempdata <- tempdata %>% pivot_longer(cols = starts_with("simulation"), names_to = "simulation", values_to = "QALY")
simudata_long <- rbind(simudata_long, tempdata %>% mutate(Group = "NOAC"))


plotdata_without_clinical_af <- simudata_long %>%
  mutate(QALY = ifelse(is.na(QALY), ifelse(is.infinite(max(QALY, na.rm=T)), 0, max(QALY, na.rm=T)), QALY), .by = c(simulation, Group)) %>%
  group_by(Time, Group) %>% 
  summarise(QALY = mean(QALY, na.rm= T))

No_data_without_clinical_af <- simudata_long %>% mutate(QALY = ifelse(is.na(QALY), ifelse(is.infinite(max(QALY, na.rm=T)), 0, max(QALY, na.rm=T)), QALY), .by = c(simulation, Group)) %>% filter(Time == 60 & Group == "No NOAC") %>% pull(QALY)
Yes_data_without_clinical_af <- simudata_long %>% mutate(QALY = ifelse(is.na(QALY), ifelse(is.infinite(max(QALY, na.rm=T)), 0, max(QALY, na.rm=T)), QALY), .by = c(simulation, Group)) %>% filter(Time == 60 & Group == "NOAC") %>% pull(QALY)

end_val_NOAC_without_clinical_af <- plotdata_without_clinical_af[dim(plotdata_without_clinical_af)[1]-1,3]
end_val_No_NOAC_without_clinical_af <- plotdata_without_clinical_af[dim(plotdata_without_clinical_af)[1],3]



############################################################################
############################################################################
###                                                                      ###
###               SAVE THE CREATED DATA FRAMES INTO A LIST               ###
###                                                                      ###
############################################################################
############################################################################

used_sims <- sim

# Save the data frames into a list
list_of_data_for_without_clinical_af <- 
  
  list(
    No_data_without_clinical_af = No_data_without_clinical_af,
    Yes_data_without_clinical_af  = Yes_data_without_clinical_af ,
    end_val_NOAC_without_clinical_af  = end_val_NOAC_without_clinical_af ,
    end_val_No_NOAC_without_clinical_af   = end_val_No_NOAC_without_clinical_af,
    used_sims                             = used_sims
  )

