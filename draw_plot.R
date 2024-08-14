
###########################################################################
###########################################################################
###                                                                     ###
###                             CREATE PLOT                             ###
###                                                                     ###
###########################################################################
###########################################################################



#################################################################
##                        Define styles                        ##
#################################################################


# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")

# Invoke showtext
showtext_auto()

noac_color <- "#5C7DB2"

no_noac_color <- "#E0912E"

custom_thin_line <- 0.1

custom_big_line <- 2.0

custom_alpha <- 0.3

# Define colors
custom_colors <- c("NOAC" = "#5C7DB2", "No NOAC" = "#E0912E")

# Create vector for the size of simulations
simulation_annotation <- paste0("N = ", plotdata$sim[1], " simulations")


##################################################################
##                       Create the graph                       ##
##################################################################


p_main <-   ggplot() +
  
  theme_classic(base_size = 20, base_family = "rosario") +
  
  # Draw thin lines for NOAC users
  geom_line(data = thinline_noac,
            aes(x = Time, y = QALY, group = simulation),
            size = custom_thin_line,
            color = noac_color,
            linetype = "solid",
            alpha = custom_alpha,
            na.rm = T) +
  
  # Draw thin lines for those without NOAC
  geom_line(data = thinline_no_noac,
            aes(x = Time, y = QALY, group = simulation),
            size = custom_thin_line,
            color = no_noac_color,
            linetype = "solid",
            alpha = custom_alpha,
            na.rm = T
  ) +
  
  # Draw big line
  geom_smooth(data = plotdata,
              method = lm, se = F, formula = y ~ splines::bs(x, 3),
              aes(x = Time, y = QALY, color = Group, linetype = Group),
              size=custom_big_line) +
  
  # Manually set colors
  scale_color_manual(values = custom_colors) +  
  
  
  # Define layout and style
  labs(x = "Time (years)", y = "Cumulative QALYs") +
  scale_x_continuous(breaks = seq(0, 120, 12), limits = c(0, 120), expand = c(0, 0), labels = 0:10) +
  scale_y_continuous(breaks = seq(0, 120, 12), limits = c(0, 120), expand = c(0, 0), labels = 0:10) +
  theme(axis.title.x = element_text(face = "bold", margin = margin(t = 15, unit = "pt")),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15, unit = "pt"))) +
  
  
  theme(legend.position = c(.87, .9),
        legend.title=element_blank(),
        legend.key.width = unit(2,"cm")) #+
  
  # # Annotate the number of simulations:
  # annotate("text", x=117, y=10, label=simulation_annotation,
  #          color="black", size=6,
  #          family = "rosario", hjust = 1, vjust = 0.5)


##################################################################
##                        Magnified plot                        ##
##################################################################


p_magnified <-  ggplot() +
  
  theme_classic(base_size = 14, base_family = "rosario") +
  
  # Draw thin lines for NOAC users
  geom_line(data = thinline_noac,
            aes(x = Time, y = QALY, group = simulation),
            size = custom_thin_line,
            color = noac_color,
            linetype = "solid",
            alpha = custom_alpha,
            na.rm = T) +
  
  # Draw thin lines for those without NOAC
  geom_line(data = thinline_no_noac,
            aes(x = Time, y = QALY, group = simulation),
            size = custom_thin_line,
            color = no_noac_color,
            linetype = "solid",
            alpha = custom_alpha,
            na.rm = T
  ) +
  
  # Draw big line
  geom_smooth(data = plotdata,
              method = lm, se = F, formula = y ~ splines::bs(x, 3),
              aes(x = Time, y = QALY, color = Group, linetype = Group),
              size=custom_big_line) +
  
  # Manually set colors
  scale_color_manual(values = custom_colors) +  
  
  
  # Define layout and style
  
  xlab(NULL) +
  ylab(NULL) +
  
  scale_x_continuous(breaks = seq(96, 108, 6), limits = c(96, 108), expand = c(0, 0),
                     labels = seq(96/12, 108/12, 6/12)) +
  scale_y_continuous(breaks = seq(54, 66, 6), limits = c(54, 66), expand = c(0, 0),
                     labels = seq(54/12, 66/12, 6/12)) +
  theme(axis.title.x = element_text(face = "bold", margin = margin(t = 15, unit = "pt")),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15, unit = "pt"))) +
  
  # Remove legend
  theme(legend.position = "none") 




#################################################################
##        Embed the magnified plot within the main plot        ##
#################################################################


sim_plot <- p_main + annotation_custom(ggplotGrob(p_magnified), xmin = 10, xmax = 62, 
                           ymin = 64, ymax = 106) +
  
  
  # Annotate the "magnifying lens"  
  annotate("rect", xmin = 96, xmax = 108, ymin = 54, ymax = 66,
           alpha = 0.4, color = "black", fill = "lightblue", size = 0.5) +
  
  # If no fill for the "magnifying lens" is desired, use the following logic:
  
  # annotate("rect", xmin = 96, xmax = 108, ymin = 54, ymax = 66, 
  #          color = "black", size = 0.5, fill = NA) +
  
  # Add an arrow
  annotate("segment", x = 96, y = 64, xend = 62, yend = 87,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "black", size = 0.5)

print(sim_plot)



###########################################################################
###########################################################################
###                                                                     ###
###                           EXPORT PLOT                               ###
###                                                                     ###
###########################################################################
###########################################################################


# When showtext() is used to change font(s), plots have to be saved as pdf files,
# otherwise proportions get distorted.

# Save as PDF with dpi specified
ggsave("figures/plot.pdf", width = 8, height =5, dpi = 600)

# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf("figures/plot.pdf", density = 600)

# Save it as PNG
image_write(pdf_image,
            path = "figures/plot.png",
            format = "png",
            density = 600)

