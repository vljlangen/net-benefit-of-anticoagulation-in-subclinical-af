# Load packages
library(pacman)
p_load(tidyverse)
p_load(ggplot2)
p_load(rayshader)
p_load(RColorBrewer)
p_load(interp)


# Prepare data
regular_sens_data <- interp(x = test$Stroke, 
                            y = test$Bleed, 
                            z = test$Value, 
                            method = "linear",
                            duplicate = "mean",
                            xo = seq(0.5,0.92, length.out =7),
                            yo = seq(1.05, 2.50, length.out = 7))

temp2 <- lapply(as.list(regular_sens_data$x), function(x) {cbind(x, regular_sens_data$y)})
temp2 <- do.call(rbind, temp2)
temp2 <- cbind(temp2, as.vector(regular_sens_data$z))
colnames(temp2) <- c("Stroke", "Bleed", "Value")
temp2 <- as.data.frame(temp2)


# Draw a raster plot
stroke = ggplot(temp2, aes(Stroke, Bleed)) +
  geom_raster(aes(fill = Value)) +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA)) +
  theme_bw(base_size = 16, base_family = "helvetica") +
  labs(x = "Treatment effect on stroke") +
  labs(y = "Treatment effect on bleeding") +
  scale_fill_continuous(na.value = NA) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +   
  theme(
    axis.title.y.right = element_blank(),                        
    axis.title.y.left = element_text(margin = margin(r = 20)),   
    axis.title.x = element_text(margin = margin(t = 20))
  ) +
  labs(y = "Treatment effect on bleeding\n(relative risk)") +  
  labs(x = "Treatment effect on stroke\n(relative risk)")      


# Modify legend title
stroke <- stroke  +
  labs(fill = "Incremental\nQALY\nwith NOAC\n ")


# Apply a viridis color theme
stroke <- stroke + scale_fill_viridis_c(option = "plasma", na.value = NA)


# Display the ready raster plot
print(stroke)


# Convert to 3D image
plot_gg(stroke,multicore=TRUE,width=6,height=5,scale=250,
        shadow_intensity = 0.7)


# Export the plot
render_snapshot("prob_sens_plot_3d",
                software_render = TRUE,
                width = 3000,
                height = 3000)
