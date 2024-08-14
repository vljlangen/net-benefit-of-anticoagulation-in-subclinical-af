
############################################################################
############################################################################
###                                                                      ###
###                        PREPARE DATA FOR PLOTS                        ###
###                                                                      ###
############################################################################
############################################################################


#severity distribution
disability_data_Yes_NOAC <- array(dim = c(121,5), dimnames = list(paste0("Time",1:121), c("Death", "Severe disability", "Moderate disability", "Mild disability", "No disability")))
disability_data_No_NOAC <- array(dim = c(121,5), dimnames = list(paste0("Time",1:121), c("Death", "Severe disability", "Moderate disability", "Mild disability", "No disability")))
healthState_data_Yes_NOAC <- array(dim = c(121,7), dimnames = list(paste0("Time",1:121), c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation")))
healthState_data_No_NOAC <- array(dim = c(121,7), dimnames = list(paste0("Time",1:121), c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation")))

suppressWarnings(
  for (t in 1:121) {
    tempvec <- rep(0,5)
    for (s in 1:sim) {
      try(tempvec[data_Yes_NOAC[[s]][t,2]] <- tempvec[data_Yes_NOAC[[s]][t,2]] + 1)
    }
    disability_data_Yes_NOAC[t,] <- tempvec
    
    tempvec <- rep(0,5)
    for (s in 1:sim) {
      try(tempvec[data_No_NOAC[[s]][t,2]] <- tempvec[data_No_NOAC[[s]][t,2]] + 1)
    }
    disability_data_No_NOAC[t,] <- tempvec
    
    tempvec <- rep(0,7)
    for (s in 1:sim) {
      try(tempvec[data_Yes_NOAC[[s]][t,1]] <- tempvec[data_Yes_NOAC[[s]][t,1]] + 1)
    }
    healthState_data_Yes_NOAC[t,] <- tempvec
    
    tempvec <- rep(0,7)
    for (s in 1:sim) {
      try(tempvec[data_No_NOAC[[s]][t,1]] <- tempvec[data_No_NOAC[[s]][t,1]] + 1)
    }
    healthState_data_No_NOAC[t,] <- tempvec
    
  }
)

disability_data_Yes_NOAC <- sapply(as.data.frame(disability_data_Yes_NOAC), cumsum)[,1:4]
disability_data_Yes_NOAC <- cbind(disability_data_Yes_NOAC, Time = 0:120)
disability_data_Yes_NOAC <- pivot_longer(as.data.frame(disability_data_Yes_NOAC), cols=c(1:4), names_to = "Disability", values_to = "Count")

disability_data_No_NOAC <- sapply(as.data.frame(disability_data_No_NOAC), cumsum)[,1:4]
disability_data_No_NOAC <- cbind(disability_data_No_NOAC, Time = 0:120)
disability_data_No_NOAC <- pivot_longer(as.data.frame(disability_data_No_NOAC), cols=c(1:4), names_to = "Disability", values_to = "Count")

healthState_data_Yes_NOAC <- sapply(as.data.frame(healthState_data_Yes_NOAC), cumsum)[,c(1,3,4,5,6)]
healthState_data_Yes_NOAC <- cbind(healthState_data_Yes_NOAC, Time = 1:121)
healthState_data_Yes_NOAC <- pivot_longer(as.data.frame(healthState_data_Yes_NOAC), cols=c(1:5), names_to = "Observation", values_to = "Count")

healthState_data_No_NOAC <- sapply(as.data.frame(healthState_data_No_NOAC), cumsum)[,c(1,3,4,5,6)]
healthState_data_No_NOAC <- cbind(healthState_data_No_NOAC, Time = 1:121)
healthState_data_No_NOAC <- pivot_longer(as.data.frame(healthState_data_No_NOAC), cols=c(1:5), names_to = "Observation", values_to = "Count")

total_counts_Yes_NOAC <- healthState_data_Yes_NOAC %>% filter(Time==121)
total_counts_No_NOAC <- healthState_data_No_NOAC %>% filter(Time==121)



######################################################################
##  Save information about the used simulations to the data frames  ##
######################################################################

healthState_data_Yes_NOAC$sim  <- sim
healthState_data_No_NOAC$sim   <- sim
disability_data_Yes_NOAC$sim   <- sim
disability_data_No_NOAC$sim    <- sim


############################################################################
############################################################################
###                                                                      ###
###               SAVE THE CREATED DATA FRAMES INTO A LIST               ###
###                                                                      ###
############################################################################
############################################################################


# Save the data frames into a list
list_of_data_for_fig2_and_fig3 <- 
  
list(
healthState_data_Yes_NOAC = healthState_data_Yes_NOAC,
healthState_data_No_NOAC  = healthState_data_No_NOAC ,
disability_data_Yes_NOAC  = disability_data_Yes_NOAC ,
disability_data_No_NOAC   = disability_data_No_NOAC
)

