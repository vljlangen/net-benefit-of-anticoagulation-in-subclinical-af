 
###########################################################################
###########################################################################
###                                                                     ###
###                           CREATE FIGURE 3                           ###
###                                                                     ###
###########################################################################
###########################################################################


#################################################################
##           Prepare stylistic settings of the graph           ##
#################################################################


# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")

# Invoke showtext
showtext_auto()

# Define constants
base_size_constant <- 20
x_margin <- 14
y_margin <- 16
title_size <- 18

 

pal = c(
  "#00405d",   # Deep navy blue  
  "#9B7C91",  # Pastel middle tone
  "#FF90A0",  # Pastel pink
  "#FFDC87", # Pastel yellow
  "#FBFAB6"   # Pastel light yellow
)

 



# > healthState_data_Yes_NOAC$Observation %>% unique()
# [1] "Death"                                          
# [2] "Ischemic Stroke"                                
# [3] "Intracerebral bleeding"                         
# [4] "Other intracranial bleeding"                    
# [5] "Major bleeding other than intracranial bleeding"



# Stacking order for Plots 3 & 4
order <- c("Death", "Ischemic Stroke",
           "Intracerebral bleeding", "Other intracranial bleeding",
           "Major bleeding other than intracranial bleeding")


# Determine the ylim value; it shall be the value higher than
# the max observed value which is divisible by 250
ylim_Yes_NOAC <- (sum(healthState_data_Yes_NOAC %>%
                        filter(Time==120) %>%
                        pull(Count)
                      ) %/% 250 + 1) * 250

ylim_No_NOAC <- (sum(healthState_data_No_NOAC %>%
                       filter(Time==120) %>%
                       pull(Count)
                     ) %/% 250 + 1) * 250

# Determine the ylim value; it shall be the value higher than
# the max observed value which is divisible by 250
ylim_custom <- max(ylim_Yes_NOAC, ylim_No_NOAC) + 500




# Create vector for the size of simulations
simulation_annotation <- paste0("N = ", disability_data_Yes_NOAC$sim[1], " simulations")


# Plot 3
p3 <- healthState_data_Yes_NOAC %>% 
  mutate(Observation = factor(Observation, levels=order)) %>% 
  ggplot(aes(Time, Count, fill = Observation, label = Observation, color = Observation)) +
  geom_stream(type = "ridge",
              bw=1.05) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = base_size_constant, base_family = "rosario") +
  labs(x = "Time (years)") +
  scale_x_continuous(breaks = seq(0, 120, 12), limits = c(0, 120), expand = c(0, 0), labels = 0:10) +
  scale_y_continuous(breaks = seq(0, ylim_custom, 1000), limits = c(0, ylim_custom), expand = c(0, 0)) +
  
  theme(plot.title = element_text(size = title_size, face = "bold"),
        axis.line = element_line(linewidth = 0.5),     # Set axis line width
        axis.ticks = element_line(linewidth = 0.5),    # Set axis tick width
        axis.title.x = element_text(size = base_size_constant,
                                    face = "bold",
                                    margin = margin(t = x_margin, unit = "pt")),
        axis.title.y = element_text(size = base_size_constant,
                                    face = "bold",
                                    margin = margin(r = y_margin, unit = "pt"))) +
  ggtitle("With NOAC medication") #+
  
  # # Annotate the number of simulations:
  # annotate("text", x=5, y=ylim_custom-50, label=simulation_annotation,
  #          color="black", size=4,
  #          family = "rosario", hjust = 0, vjust = 0.5)



# Plot 4
p4 <- healthState_data_No_NOAC %>% 
  mutate(Observation = factor(Observation, levels=order)) %>% 
  ggplot(aes(Time, Count, fill = Observation, label = Observation, color = Observation)) +
  geom_stream(type = "ridge",
              bw=1.05) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  theme_classic(base_size = base_size_constant, base_family = "rosario") +
  labs(x = "Time (years)") +
  scale_x_continuous(breaks = seq(0, 120, 12), limits = c(0, 120), expand = c(0, 0), labels = 0:10) +
  scale_y_continuous(breaks = seq(0, ylim_custom, 1000), limits = c(0, ylim_custom), expand = c(0, 0)) +
  theme(plot.title = element_text(size = title_size, face = "bold"),
        axis.line = element_line(linewidth = 0.5),     # Set axis line width
        axis.ticks = element_line(linewidth = 0.5),    # Set axis tick width
        axis.title.x = element_text(size = base_size_constant,
                                    face = "bold",
                                    margin = margin(t = x_margin, unit = "pt")),
        axis.title.y = element_text(size = base_size_constant,
                                    face = "bold",
                                    margin = margin(r = y_margin, unit = "pt"))) +
  ggtitle("Without NOAC medication") #+
  
  # # Annotate the number of simulations:
  # annotate("text", x=5, y=ylim_custom-50, label=simulation_annotation,
  #          color="black", size=4,
  #          family = "rosario", hjust = 0, vjust = 0.5)




############################################################################
############################################################################
###                                                                      ###
###                           CREATE THE PANEL                           ###
###                                                                      ###
############################################################################
############################################################################



# Extract the legend from one of the plots
legend <- get_legend(
  p3 + theme(legend.box.margin = margin(0, 0, 0, 0)))

# Create the panel with a legend
panel_fig3 <- plot_grid(p3 + theme(legend.position="none"),
                        
                        NULL,
                        
                        p4 + theme(legend.position="none"),
                        
                        NULL,
                        
                        legend,
                        
                        rel_widths = c(1, 0.2, 1, 0.15, 1),
                        nrow = 1)

# Display the panel
print(panel_fig3)



###########################################################################
###########################################################################
###                                                                     ###
###                           EXPORT FIGURE 3                           ###
###                                                                     ###
###########################################################################
###########################################################################


# When showtext() is used to change font(s), plots have to be saved as pdf files,
# otherwise proportions get distorted.

# Save as PDF with dpi specified
ggsave("figures/figure3.pdf", width = 20, height =5, dpi = 600)

# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf("figures/figure3.pdf", density = 600)

# Save it as PNG
image_write(pdf_image,
            path = "figures/figure3.png",
            format = "png",
            density = 600)





