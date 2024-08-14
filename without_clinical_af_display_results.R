
###########################################################################
###########################################################################
###                                                                     ###
###                           DISPLAY RESULTS                           ###
###                                                                     ###
###########################################################################
###########################################################################

print(t.test(No_data_without_clinical_af, Yes_data_without_clinical_af, alternative = "greater"))

print("The difference in QALYs (Quality-Adjusted Life Years)")
print("between the NOAC group and the group not using NOAC is")
print("displayed below.")
print("(Exact equation:")
print("end_val_NOAC_without_clinical_af - end_val_No_NOAC_without_clinical_af)")
print(paste("Used simulations were:", used_sims))
print(end_val_NOAC_without_clinical_af - end_val_No_NOAC_without_clinical_af)


