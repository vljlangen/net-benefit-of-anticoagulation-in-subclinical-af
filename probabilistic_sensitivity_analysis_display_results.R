
#################################################################
##           Prepare stylistic settings of the graph           ##
#################################################################


# Define constants
base_size_constant <- 16

used_sim <- sim
used_iteration <- iteration
# Create vector for the size of simulations
simulation_annotation_sims_iterations <- paste0("N = ",
                                                used_sim,
                                                " simulations (",
                                                used_iteration,
                                                " iterations)" )

# Calculate adequate position for annotation

x_pos <- min(test$Stroke) + (0.01 * (max(test$Stroke) - min(test$Stroke)))
y_pos <- min(test$Bleed) + (0.95 * (max(test$Bleed) - min(test$Bleed)))



# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")

# Invoke showtext
showtext_auto()

# Select colors
fun_color_range <- colorRampPalette(c("red", "darkblue"))
#fun_color_range <- colorRampPalette(c("yellow", "red"))




# Draw plot
prob_plot <- ggplot(data = test, aes(x=Stroke, y=Bleed)) +
  theme_classic(base_size = base_size_constant, base_family = "rosario") +
  geom_point(aes(color = Value), size = 30, alpha = 0.2, shape = 15) +
  scale_color_continuous(low = fun_color_range(30)[1], high = fun_color_range(30)[30]) +

  labs(x = "Treatment effect on stroke \n(relative risk)") +
  labs(y = "Treatment effect on bleeding \n(relative risk)") +

  # Define layout and style
  theme(axis.title.x = element_text(face = "bold", margin = margin(t = 13, unit = "pt")),
        axis.title.y = element_text(face = "bold", margin = margin(r = 13, unit = "pt"))) #+

  # # Plot title        
  # ggtitle("NOAC minus No NOAC probabilistic sensitivity analysis") +
  #   
  # # Annotate the number of simulations:
  # annotate("text", x=x_pos, y=y_pos, label=simulation_annotation_sims_iterations,
  #         color="black", size=4,
  #         family = "rosario", hjust = 0, vjust = 0.5)

 
# Modify plot label
prob_plot <- prob_plot + labs(color = "Incremental QALY \nwith treatment")


#Probability sensitivity analysis contour plot

# Prepare data
g.size <- 40
regular_sens_data <- interp(x = test$Stroke, 
                           y = test$Bleed, 
                          z = test$Value, 
                           method = "linear",
                           duplicate = "mean",
                           nx = g.size,
                           ny = g.size)
temp2 <- lapply(as.list(regular_sens_data$x), function(x) {cbind(x, regular_sens_data$y)})
temp2 <- do.call(rbind, temp2)
temp2 <- cbind(temp2, as.vector(regular_sens_data$z))
colnames(temp2) <- c("Stroke", "Bleed", "Value")
temp2 <- as.data.frame(temp2)


# Create plot
prob_plot <- ggplot(data = temp2, aes(Stroke, Bleed, z=Value)) + 
  
  geom_contour_filled(bins = 20)

#geom_raster(aes(fill = Value))
#geom_contour()


print(prob_plot)


#################################################################
##                        Export figure                        ##
#################################################################


# When showtext() is used to change font(s), plots have to be saved as pdf files,
# otherwise proportions get distorted.

# Save as PDF with dpi specified
ggsave("figures/prob_sens_plot.pdf", width = 7, height = 5, dpi = 600)

# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf("figures/prob_sens_plot.pdf", density = 600)

# Save it as PNG
image_write(pdf_image,
            path = "figures/prob_sens_plot.png",
            format = "png",
            density = 600)



#################################################################
##                        Print results                        ##
#################################################################



print(sprintf("The NOAC was beneficial in the %.2f%% of the cases", 100*(sum(test[,3]>0) / length(test[,3]))))
print(paste("Used simulations:", used_sim))
print(paste("Used iterations:", used_iteration))



