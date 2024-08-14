#qaly difference
#expected time for death
#expected time for clinical flimmer
#hazard ratio
#expected time spend susceptible
#expected time between disabilities
#expected time for stroke
#expected time for bleed

life_years_No_NOAC <- NULL
life_years_Yes_NOAC <- NULL
qalys_No_NOAC <- 0
qalys_Yes_NOAC <- 0
#10year averages
average_death_No_NOAC <- 0
average_death_Yes_NOAC <- 0
average_stroke_No_NOAC <- 0
average_ich_bleed_No_NOAC <- 0
average_subdural_bleed_No_NOAC <- 0
average_other_Major_bleed_No_NOAC <- 0
average_stroke_Yes_NOAC <- 0
average_ich_bleed_Yes_NOAC <- 0
average_subdural_bleed_Yes_NOAC <- 0
average_other_Major_bleed_Yes_NOAC <- 0
#5year averages
average_death_No_NOAC_5y <- 0
average_death_Yes_NOAC_5y <- 0
average_stroke_No_NOAC_5y <- 0
average_ich_bleed_No_NOAC_5y <- 0
average_subdural_bleed_No_NOAC_5y <- 0
average_other_Major_bleed_No_NOAC_5y <- 0
average_stroke_Yes_NOAC_5y <- 0
average_ich_bleed_Yes_NOAC_5y <- 0
average_subdural_bleed_Yes_NOAC_5y <- 0
average_other_Major_bleed_Yes_NOAC_5y <- 0
average_qaly_No_NOAC_5y <- 0
average_qaly_Yes_NOAC_5y <- 0
average_life_years_No_NOAC_5y <- 0
average_life_years_Yes_NOAC_5y <- 0
#statistical results
insidences_No_NOAC <- c(death = 0, stroke = 0, ich = 0, subdural = 0, otherMajor = 0)
insidences_Yes_NOAC <- c(death = 0, stroke = 0, ich = 0, subdural = 0, otherMajor = 0)
for (i in 1:sim) {
  life_years_No_NOAC <- c(life_years_No_NOAC, dim(data_No_NOAC[[i]])[1]/12)
  life_years_Yes_NOAC <- c(life_years_Yes_NOAC, dim(data_Yes_NOAC[[i]])[1]/12)
  qalys_No_NOAC <- qalys_No_NOAC + data_No_NOAC[[i]][dim(data_No_NOAC[[i]])[1], 4]/12
  qalys_Yes_NOAC <- qalys_Yes_NOAC + data_Yes_NOAC[[i]][dim(data_Yes_NOAC[[i]])[1], 4]/12
#10year averages
  average_death_No_NOAC <- average_death_No_NOAC + sum(data_No_NOAC[[i]][,1] == 1)/sim
  average_stroke_No_NOAC <- average_stroke_No_NOAC + sum(data_No_NOAC[[i]][,1]==3)/sim
  average_ich_bleed_No_NOAC <- average_ich_bleed_No_NOAC + sum(data_No_NOAC[[i]][,1]==4)/sim
  average_subdural_bleed_No_NOAC <- average_subdural_bleed_No_NOAC + sum(data_No_NOAC[[i]][,1]==5)/sim
  average_other_Major_bleed_No_NOAC <- average_other_Major_bleed_No_NOAC + sum(data_No_NOAC[[i]][,1]==6)/sim
  average_death_Yes_NOAC <- average_death_Yes_NOAC + sum(data_Yes_NOAC[[i]][,1]==1)/sim
  average_stroke_Yes_NOAC <- average_stroke_Yes_NOAC + sum(data_Yes_NOAC[[i]][,1]==3)/sim
  average_ich_bleed_Yes_NOAC <- average_ich_bleed_Yes_NOAC + sum(data_Yes_NOAC[[i]][,1]==4)/sim
  average_subdural_bleed_Yes_NOAC <- average_subdural_bleed_Yes_NOAC + sum(data_Yes_NOAC[[i]][,1]==5)/sim
  average_other_Major_bleed_Yes_NOAC <- average_other_Major_bleed_Yes_NOAC + sum(data_Yes_NOAC[[i]][,1]==6)/sim
#5year averages
  average_death_No_NOAC_5y <- average_death_No_NOAC_5y + sum(data_No_NOAC[[i]][1:min(dim(data_No_NOAC[[i]])[1], 60), 1] ==1)/sim
  average_stroke_No_NOAC_5y <- average_stroke_No_NOAC_5y + sum(data_No_NOAC[[i]][1:min(dim(data_No_NOAC[[i]])[1], 60),1]==3)/sim
  average_ich_bleed_No_NOAC_5y <- average_ich_bleed_No_NOAC_5y + sum(data_No_NOAC[[i]][1:min(dim(data_No_NOAC[[i]])[1], 60),1]==4)/sim
  average_subdural_bleed_No_NOAC_5y <- average_subdural_bleed_No_NOAC_5y + sum(data_No_NOAC[[i]][1:min(dim(data_No_NOAC[[i]])[1], 60),1]==5)/sim
  average_other_Major_bleed_No_NOAC_5y <- average_other_Major_bleed_No_NOAC_5y + sum(data_No_NOAC[[i]][1:min(dim(data_No_NOAC[[i]])[1], 60),1]==6)/sim
  average_death_Yes_NOAC_5y <- average_death_Yes_NOAC_5y + sum(data_Yes_NOAC[[i]][1:min(dim(data_Yes_NOAC[[i]])[1], 60),1]==1)/sim
  average_stroke_Yes_NOAC_5y <- average_stroke_Yes_NOAC_5y + sum(data_Yes_NOAC[[i]][1:min(dim(data_Yes_NOAC[[i]])[1], 60),1]==3)/sim
  average_ich_bleed_Yes_NOAC_5y <- average_ich_bleed_Yes_NOAC_5y + sum(data_Yes_NOAC[[i]][1:min(dim(data_Yes_NOAC[[i]])[1], 60),1]==4)/sim
  average_subdural_bleed_Yes_NOAC_5y <- average_subdural_bleed_Yes_NOAC_5y + sum(data_Yes_NOAC[[i]][1:min(dim(data_Yes_NOAC[[i]])[1], 60),1]==5)/sim
  average_other_Major_bleed_Yes_NOAC_5y <- average_other_Major_bleed_Yes_NOAC_5y + sum(data_Yes_NOAC[[i]][1:min(dim(data_Yes_NOAC[[i]])[1], 60),1]==6)/sim
  average_qaly_No_NOAC_5y <- average_qaly_No_NOAC_5y + data_No_NOAC[[i]][min(dim(data_No_NOAC[[i]])[1], 60),4]/12000
  average_qaly_Yes_NOAC_5y <- average_qaly_Yes_NOAC_5y + data_Yes_NOAC[[i]][min(dim(data_Yes_NOAC[[i]])[1], 60),4]/12000
  average_life_years_No_NOAC_5y <- average_life_years_No_NOAC_5y + min(dim(data_No_NOAC[[i]])[1], 60)/12000
  average_life_years_Yes_NOAC_5y <- average_life_years_Yes_NOAC_5y + min(dim(data_Yes_NOAC[[i]])[1], 60)/12000
#insidences
  insidences_No_NOAC[1] <- insidences_No_NOAC[1] + (data_No_NOAC[[i]][dim(data_No_NOAC[[i]])[1],2] == 1)*1 #deaths
  insidences_No_NOAC[2] <- insidences_No_NOAC[2] + sum(data_No_NOAC[[i]][1:(dim(data_No_NOAC[[i]])[1]),1] == 3)
  insidences_No_NOAC[3] <- insidences_No_NOAC[3] + sum(data_No_NOAC[[i]][1:(dim(data_No_NOAC[[i]])[1]),1] == 4)
  insidences_No_NOAC[4] <- insidences_No_NOAC[4] + sum(data_No_NOAC[[i]][1:(dim(data_No_NOAC[[i]])[1]),1] == 5)
  insidences_No_NOAC[5] <- insidences_No_NOAC[5] + sum(data_No_NOAC[[i]][1:(dim(data_No_NOAC[[i]])[1]),1] == 6)
  insidences_Yes_NOAC[1] <- insidences_Yes_NOAC[1] + (data_Yes_NOAC[[i]][dim(data_Yes_NOAC[[i]])[1],2] == 1)*1 #deaths
  insidences_Yes_NOAC[2] <- insidences_Yes_NOAC[2] + sum(data_Yes_NOAC[[i]][1:(dim(data_Yes_NOAC[[i]])[1]),1] == 3)
  insidences_Yes_NOAC[3] <- insidences_Yes_NOAC[3] + sum(data_Yes_NOAC[[i]][1:(dim(data_Yes_NOAC[[i]])[1]),1] == 4)
  insidences_Yes_NOAC[4] <- insidences_Yes_NOAC[4] + sum(data_Yes_NOAC[[i]][1:(dim(data_Yes_NOAC[[i]])[1]),1] == 5)
  insidences_Yes_NOAC[5] <- insidences_Yes_NOAC[5] + sum(data_Yes_NOAC[[i]][1:(dim(data_Yes_NOAC[[i]])[1]),1] == 6)
}

print(t.test(life_years_No_NOAC, life_years_Yes_NOAC, alternative = "greater"))
life_years_No_NOAC <- sum(life_years_No_NOAC)
life_years_Yes_NOAC <- sum(life_years_Yes_NOAC)

table2_data <- data.frame(
  NoNOAC = c(insidences_No_NOAC, life_years_No_NOAC, qalys_No_NOAC),
  YesNOAC = c(insidences_Yes_NOAC, life_years_Yes_NOAC, qalys_Yes_NOAC),
  difference10k = c(insidences_Yes_NOAC - insidences_No_NOAC, life_years_Yes_NOAC - life_years_No_NOAC, qalys_Yes_NOAC - qalys_No_NOAC),
  averageDifference = c(average_death_Yes_NOAC - average_death_No_NOAC, average_stroke_Yes_NOAC - average_stroke_No_NOAC, average_ich_bleed_Yes_NOAC - average_ich_bleed_No_NOAC, average_subdural_bleed_Yes_NOAC - average_subdural_bleed_No_NOAC, average_other_Major_bleed_Yes_NOAC - average_other_Major_bleed_No_NOAC, (life_years_Yes_NOAC - life_years_No_NOAC)/sim, (qalys_Yes_NOAC - qalys_No_NOAC)/sim),
  averageDifference_5y = c(average_death_Yes_NOAC_5y - average_death_No_NOAC_5y, average_stroke_Yes_NOAC_5y - average_stroke_No_NOAC_5y, average_ich_bleed_Yes_NOAC_5y - average_ich_bleed_No_NOAC_5y, average_subdural_bleed_Yes_NOAC_5y - average_subdural_bleed_No_NOAC_5y, average_other_Major_bleed_Yes_NOAC_5y - average_other_Major_bleed_No_NOAC_5y, average_qaly_Yes_NOAC_5y - average_qaly_No_NOAC_5y, average_life_years_Yes_NOAC_5y - average_life_years_No_NOAC_5y),
  rates_No_NOAC = c(insidences_No_NOAC/life_years_No_NOAC, NA, NA),
  rates_Yes_NOAc = c(insidences_Yes_NOAC/life_years_Yes_NOAC, NA, NA),
  expected_first_event_in_years_No_NOAC = c(life_years_No_NOAC / insidences_No_NOAC, NA, NA),
  expected_first_event_in_years_Yes_NOAC = c(life_years_Yes_NOAC / insidences_Yes_NOAC, NA, NA),
  firstEventProbability_No_NOAC = c((insidences_No_NOAC/life_years_No_NOAC)[1]/sum(insidences_No_NOAC/life_years_No_NOAC), (insidences_No_NOAC/life_years_No_NOAC)[2]/sum(insidences_No_NOAC/life_years_No_NOAC), (insidences_No_NOAC/life_years_No_NOAC)[3]/sum(insidences_No_NOAC/life_years_No_NOAC), (insidences_No_NOAC/life_years_No_NOAC)[4]/sum(insidences_No_NOAC/life_years_No_NOAC), (insidences_No_NOAC/life_years_No_NOAC)[5]/sum(insidences_No_NOAC/life_years_No_NOAC), NA, NA),
  firstEventProbability_Yes_NOAC = c((insidences_Yes_NOAC/life_years_Yes_NOAC)[1]/sum(insidences_Yes_NOAC/life_years_Yes_NOAC), (insidences_Yes_NOAC/life_years_Yes_NOAC)[2]/sum(insidences_Yes_NOAC/life_years_Yes_NOAC), (insidences_Yes_NOAC/life_years_Yes_NOAC)[3]/sum(insidences_Yes_NOAC/life_years_Yes_NOAC), (insidences_Yes_NOAC/life_years_Yes_NOAC)[4]/sum(insidences_Yes_NOAC/life_years_Yes_NOAC), (insidences_Yes_NOAC/life_years_Yes_NOAC)[5]/sum(insidences_Yes_NOAC/life_years_Yes_NOAC), NA, NA)
)

table2_data <- t(table2_data)
colnames(table2_data) <- c("Death", "Stroke", "Intracranial bleeding", "Other intracranial bleeding", "Extracranial bleeding", "Life years", "QALYs")
table2_data <- cbind(as.data.frame(table2_data), row_names = c("Without NOAC medication", "With NOAC medication", paste(sim, "Difference between NOAC and no treatment"), "Average difference per patient in 10 years", "Average difference per patient in 5 years", "Rates without NOAC medication", "Rates with NOAC medication", "Expected time for the first event in years without NOAC medication", "Expected time for the first event in years with NOAC medication", "Probability to observe as the first event without NOAC medication", "Probability to observe as the first event with NOAC medication"))

table2 <- gt(table2_data, rowname_col = "row_names") %>%
  tab_header(title = "Summary statistics", subtitle = "Calculated from the simulation") %>%
  fmt_number(columns = everything(), decimals = 3)

print(table2)

cat("Expected time to death or clinical flimmer\n\tWith NOAC medication:", 
    1/(insidences_Yes_NOAC[1]/life_years_Yes_NOAC + healthStates_rates["Clinical atrial fibrillation",2]),
    "\n\tWithout NOAC medication:",
    1/(insidences_No_NOAC[1]/life_years_No_NOAC + healthStates_rates["Clinical atrial fibrillation", 1]),
    "\n")
