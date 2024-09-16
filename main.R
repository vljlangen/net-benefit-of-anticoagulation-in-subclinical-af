
############################################################################
############################################################################
###                                                                      ###
###           TIME DEPENDENT MARKOV DECISION PROCESS ALGORITHM           ###
###                                                                      ###
############################################################################
############################################################################


##################################################################
##                        Load libraries                        ##
##################################################################

# Load pacman
library(pacman)

# Load rest of packages with pacman
p_load(ggplot2, ggthemes, tibble, dplyr, showtext, magick,
       tidyr, forcats, circlize, patchwork, gt,
       ggstream, cowplot, pdftools, foreach, doParallel, doRNG, interp)


##################################################################
##               How to Run All the Scripts Below               ##
##################################################################

# 1. Open RStudio.
#
# 2. Create a New R Project:
#    - Go to `File` -> `New Project...`.
#    - Choose `New Directory` and then `Project`.
#    - Enter a name for your project and select a location for the project folder.
#    - Click `Create Project`.
#    - This action will create a `.Rproj` file in your project folder.
#
# 3. Place All Files in the Project Root:
#    - Move or save all relevant files (scripts mentioned below) into the project folder.
#    - RStudio will automatically set the working directory to the project root.
#
# 4. Start Your Session Correctly:
#    - Always start your R session by double-clicking the `.Rproj` file created in Step 2.
#    - This ensures that the working directory is set correctly to the project root.


############################################################################
############################################################################
###                                                                      ###
###                 THE MAIN FUNCTION THAT HAS TO BE RUN                 ###
###                                                                      ###
############################################################################
############################################################################

source("qaly_sampler.R")

source("severity_matrix.R")

##########################################################################
###########################################################################
###                                                                     ###
###                         ANALYSES AND GRAPHS                         ###
###                                                                     ###
###########################################################################
###########################################################################


#set simulation size
sim <- 10000
#set global seed for the random number generator
seed <- 46692
#set policy to keep NOAC medication after hemorrhagic stroke and other
#intracranial bleeding or not
decision.to.medicate <- FALSE


############################################################################
############################################################################
###                                                                      ###
###                           EVENT SEVERITIES                           ###
###                                                                      ###
############################################################################
############################################################################


gt(data, rowname_col = "row_names", groupname_col = "group_names") %>%
  tab_header(title = "Disability coefficients with or without NOAC medication") %>%
  fmt_number(rows = everything(), columns = everything(), decimals = 3) %>%
  tab_stub_indent(rows = everything(), indent = 5) %>%
  cols_label(NoNOAC = "No", YesNOAC = "Yes")


############################################################################
############################################################################
###                                                                      ###
###                           CREATE MAIN DATA                           ###
###                                                                      ###
############################################################################
############################################################################



# Create main data for plot and everything else
source("create_main_data.R")

#Major Bleed fatality
print(
  (healthStates_rates[4,2]*data[5,2] + healthStates_rates[5,2]*data[9,2] + healthStates_rates[6,2]*data[13,2])/sum(healthStates_rates[4:6,2])
)

#mean QALY difference at the end of 10 follow up
mean(Yes_data-No_data)/12

#mean time in years in subclinical flimmer
##function to calculate first occurence of death or flimmer
time_in_sub <- function(x) {
  cumsum(
    cumprod(
      1*(
        !with(x, Observation == 1 | Observation == 7) #no death 1 and no flimmer 7
        ) #make logic to numeric
      ) #product is 1 until the first occurence from which after it is zero
    )[dim(x)[1]] #sum all together, thus the last value is the time in subclinical flimmer
  }
mean_in_subclinical_No_NOAC <- mean(unlist(lapply(data_No_NOAC, time_in_sub)))/12
mean_in_subclinical_Yes_NOAC <- mean(unlist(lapply(data_Yes_NOAC, time_in_sub)))/12
cat("Mean years in sublinical flimmer\n",
    "with NOAC: ", mean_in_subclinical_Yes_NOAC, "\n",
    "without NOAC: ", mean_in_subclinical_No_NOAC, "\n"
    )

# # Save the created data to a file
# save(list_of_data_for_plot,
#      file = "data/list_of_data_for_plot.RData")


##################################################################
##                           Figure 1                           ##
##################################################################



# # Load the created data from the file
# load("data/list_of_data_for_plot.RData")
# 
# # Extract data frames from the above data list
# thinline_noac <- list_of_data_for_plot$thinline_noac
# thinline_no_noac <- list_of_data_for_plot$thinline_no_noac
# plotdata <- list_of_data_for_plot$plotdata
# data_No_NOAC <- list_of_data_for_plot$data_No_NOAC
# data_Yes_NOAC <- list_of_data_for_plot$data_Yes_NOAC

# Draw and export the plot
source("draw_plot.R")


#################################################################
##                       Analyses part 1                       ##
#################################################################


#probabilistic sensitivity
iteration <- 2000

#create data
source("probabilistic_sensitivity_analysis_create_data.R")


# # Save the created data to a file
# save(list_of_data_for_probabilistic_sensitivity_analysis,
#      file = "data/list_of_data_for_probabilistic_sensitivity_analysis.RData")
# 
# # Load the created data from the file
# load("data/list_of_data_for_probabilistic_sensitivity_analysis.RData")
# 
# Extract data frames from the above data list
#
# test <-
# list_of_data_for_probabilistic_sensitivity_analysis$test
# 
# used_sim <-
# list_of_data_for_probabilistic_sensitivity_analysis$sim
# 
# used_iteration <-
# list_of_data_for_probabilistic_sensitivity_analysis$iteration

 

#display results
source("code_for_3d_graph.R")


#################################################################
##                        Figures 2 & 3                        ##
#################################################################

# First, create data for Figures 2 & 3
source("fig2_and_3_create_plot_data.R")


# # Save the created data to a file
# save(list_of_data_for_fig2_and_fig3,
#      file = "data/list_of_data_for_fig2_and_fig3.RData")
# 
# # # Load the created data from the file
#  load("data/list_of_data_for_fig2_and_fig3.RData")
# 
# 
# # Extract data frames from the above data list
# 
# healthState_data_Yes_NOAC <-
# list_of_data_for_fig2_and_fig3$healthState_data_Yes_NOAC
# 
# healthState_data_No_NOAC <-
# list_of_data_for_fig2_and_fig3$healthState_data_No_NOAC
# 
# disability_data_Yes_NOAC <-
# list_of_data_for_fig2_and_fig3$disability_data_Yes_NOAC
# 
# disability_data_No_NOAC <-
# list_of_data_for_fig2_and_fig3$disability_data_No_NOAC

 
# Thereafter, create and export Figure 2
source("figure2.R")

# Thereafter, create and export Figure 3
source("figure3.R")


#################################################################
##                           Table 2                           ##
#################################################################


source("table2.R")



#################################################################
##                       Analyses part 2                       ##
#################################################################


#Without clinical AF for 5 years
#
#Create data for:
#
#calculating t.test and qaly difference (treatment minus no treatment)
source("without_clinical_af_create_data.R")


# # Save the created data to a file
# save(list_of_data_for_without_clinical_af,
#      file = "data/list_of_data_for_without_clinical_af.RData")
# 
# # Load the created data from the file
# load("data/list_of_data_for_without_clinical_af.RData")
# 
# # Extract data frames and vectors from the above data list
# end_val_NOAC_without_clinical_af <-
# list_of_data_for_without_clinical_af$end_val_NOAC_without_clinical_af
# 
# end_val_No_NOAC_without_clinical_af <-
# list_of_data_for_without_clinical_af$end_val_No_NOAC_without_clinical_af
# 
# No_data_without_clinical_af <-
# list_of_data_for_without_clinical_af$No_data_without_clinical_af
# 
# Yes_data_without_clinical_af <-
# list_of_data_for_without_clinical_af$Yes_data_without_clinical_af
# 
# used_sims <- 
# list_of_data_for_without_clinical_af$used_sims


# Display results
#prints t.test and qaly difference (treatment minus no treatment)
source("without_clinical_af_display_results.R")
 

