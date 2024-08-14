 

###########################################################################
###########################################################################
###                                                                     ###
###                           CREATE FIGURE 2                           ###
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

 


pal=c(
  "#00405d",  # Deep navy blue  
  "#a15296",  # Medium purple  
  "#E388AE",  # Soft pink  
  "#FFB851"   # Warm yellow  
)




# Stacking order for Plots 1 & 2
order <- c("Death", "Mild disability", "Moderate disability", "Severe disability")

round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

ylim_Yes_NOAC <- (sum(disability_data_Yes_NOAC %>%
                        filter(Time==120) %>%
                        pull(Count)
                      ) %/% 250 + 1) * 250

ylim_No_NOAC <- (sum(disability_data_No_NOAC %>%
                        filter(Time==120) %>%
                        pull(Count)
                     ) %/% 250 + 1) * 250

# Determine the ylim value; it shall be the value higher than
# the max observed value which is divisible by 250
ylim_custom <- max(ylim_Yes_NOAC, ylim_No_NOAC) + 1500



# Create vector for the size of simulations
simulation_annotation <- paste0("N = ", disability_data_Yes_NOAC$sim[1], " simulations")


# Plot 1
p1 <- disability_data_Yes_NOAC %>% 
  mutate(Disability = factor(Disability, levels=order)) %>% 
  ggplot(aes(Time, Count, fill = Disability, label = Disability, color = Disability)) +
  geom_stream(type = "ridge" ,bw=1.0) +
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
  
  

# Plot 2
p2 <- disability_data_No_NOAC %>% 
  mutate(Disability = factor(Disability, levels=order)) %>% 
  ggplot(aes(Time, Count, fill = Disability, label = Disability, color = Disability)) +
  geom_stream(type = "ridge" ,bw=1) +
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



# # Without legend
# panel_fig2 <- plot_grid(p1 + theme(legend.position="none"),
#                         
#                         NULL,
#                         
#                         p2 + theme(legend.position="none"),
#                         
#                    rel_widths = c(1, 0.1, 1),
#                    nrow = 1)
# 
# panel_fig2


# Extract the legend from one of the plots
legend <- get_legend(
  p1 + theme(legend.box.margin = margin(0, 0, 0, 0)))

# Create the panel with a legend
panel_fig2 <- plot_grid(p1 + theme(legend.position="none"),
                        
                        NULL,
                        
                        p2 + theme(legend.position="none"),
                        
                        legend,
                        
                        rel_widths = c(1, 0.2, 1, 1),
                        nrow = 1)

# Display the panel
print(panel_fig2)
 

###########################################################################
###########################################################################
###                                                                     ###
###                           EXPORT FIGURE 2                           ###
###                                                                     ###
###########################################################################
###########################################################################


# When showtext() is used to change font(s), plots have to be saved as pdf files,
# otherwise proportions get distorted.

# Save as PDF with dpi specified
ggsave("figures/figure2.pdf", width = 15, height =5, dpi = 600)

# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf("figures/figure2.pdf", density = 600)

# Save it as PNG
image_write(pdf_image,
            path = "figures/figure2.png",
            format = "png",
            density = 600)


