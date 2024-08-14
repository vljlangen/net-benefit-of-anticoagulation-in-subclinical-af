
###########################################################################
###########################################################################
###                                                                     ###
###                           CREATE FIGURE 4                           ###
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
base_size_constant <- 18
x_margin <- 14
y_margin <- 1
title_size <- 16


# Define colors
custom_colors <- c("Lower_Bound" =   "#F5A07A",    # Pastel orange 
                   "Upper_Bound" =   "#7A9BCC")   # Pastel blue)

#plotting functions below are commented away so to make space for a new tornad
#cluster plot
sensitivity_plot <- ggplot(tornado_cluster_data, aes(x = Parameter)) +
  geom_jitter(aes(y = Lower_Bound, colour = "red"), width = 0.15)+
  geom_jitter(aes(y = Upper_Bound, colour = "blue"), width = 0.15)+
  coord_flip() +
  coord_flip() +
  xlab("") +
  ylab("10-year difference in cumulative QALY") +
  xlab("Average qaly difference between\ntreatment and no treatment") +
  
  theme(plot.title = element_text(size = title_size, face = "bold"),
        axis.line = element_line(linewidth = 0.5),     # Set axis line width
        axis.ticks = element_line(linewidth = 0.5),    # Set axis tick width
        axis.title.x = element_text(size = base_size_constant,
                                    face = "bold",
                                    margin = margin(t = x_margin, unit = "pt")),
        axis.title.y = element_text(size = base_size_constant,
                                    face = "bold",
                                    margin = margin(r = y_margin, unit = "pt"))) +
  
  # Manually set colors
  #  scale_fill_manual(values = custom_colors) +
  
  #  theme(legend.position="none") +
  ggtitle("NOAC vs. no NOAC treatment")


print(sensitivity_plot)

#tornado_lower <- tornado.2 %>% filter(type == "Lower_Bound")
#tornado_upper <- tornado.2 %>% filter(type == "Upper_Bound")




###########################################################################
###########################################################################
###                                                                     ###
###                        EXPORT TORNADO FIG                           ###
###                                                                     ###
###########################################################################
###########################################################################


# When showtext() is used to change font(s), plots have to be saved as pdf files,
# otherwise proportions get distorted.

# Save as PDF with dpi specified
ggsave("figures/tornado.pdf", dpi = 600)

# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf("figures/tornado.pdf", density = 600)

# Save it as PNG
image_write(pdf_image,
            path = "figures/tornado.png",
            format = "png",
            density = 600)






#################################################################
##                      hline at -0.06ish                      ##
#################################################################


# create plot
# (use scale_x_continuous to change labels in y axis to name of parameters)
#tornado_plot <- ggplot() + 
#  geom_rect(data = tornado.2, 
#            aes(ymax=y_max, ymin=y_min, xmax=x_max, xmin=x_min, fill=type)) +
#  theme_classic(base_size = base_size_constant, base_family = "rosario") +
#  theme(axis.title.y=element_blank(), legend.position = 'bottom',
#        legend.title = element_blank()) + 
#  geom_hline(yintercept = (as.numeric(base.value[1,2]-base.value[2,2]))/12,
#             linetype = "dashed") +

#  scale_y_continuous(breaks = c(-0.20, -0.15, -0.10, -0.05,
#                                0,
#                                0.05, 0.10, 0.15),
#                                limits = c(-0.20, 0.15), expand = c(0, 0)) +

#  scale_x_continuous(breaks = c(1:length(order.parameters)),
#                     labels = order.parameters) +
#  coord_flip() +
#  xlab("") +
#  ylab("10-year difference in cumulative QALY") +
#  xlab("Average qaly difference between treatment and no treatment") +

#    theme(plot.title = element_text(size = title_size, face = "bold"),
#          axis.line = element_line(linewidth = 0.5),     # Set axis line width
#          axis.ticks = element_line(linewidth = 0.5),    # Set axis tick width
#          axis.title.x = element_text(size = base_size_constant,
#                                      face = "bold",
#                                      margin = margin(t = x_margin, unit = "pt")),
#          axis.title.y = element_text(size = base_size_constant,
#                                      face = "bold",
#                                      margin = margin(r = y_margin, unit = "pt"))) +

# Manually set colors
#  scale_fill_manual(values = custom_colors) +

#  theme(legend.position="none") +
#  ggtitle("NOAC vs. no NOAC treatment")


#print(tornado_plot)




##################################################################
##                         hline at 0.0                         ##
##################################################################

# 
# # create plot
# # (use scale_x_continuous to change labels in y axis to name of parameters)
# tornado_plot <- ggplot() + 
# 
#   # geom_rect(data = tornado.2, 
#   #           aes(ymax=y_max, ymin=y_min, xmax=x_max, xmin=x_min, fill=type)) +
#    
#   geom_rect(data = tornado_lower, 
#             aes(ymax=y_max, ymin=0, xmax=x_max, xmin=x_min), fill="#F5A07A") +
#   
#   geom_rect(data = tornado_upper, 
#             aes(ymax=y_max, ymin=0, xmax=x_max, xmin=x_min), fill="#7A9BCC") +
#   
#   theme_classic(base_size = base_size_constant, base_family = "rosario") +
#   theme(axis.title.y=element_blank(), legend.position = 'bottom',
#         legend.title = element_blank()) + 
#   geom_hline(yintercept = 0,
#              linetype = "dashed") +
#   
#   scale_y_continuous(breaks = c(-0.15, -0.10, -0.05,
#                                 0,
#                                 0.05, 0.10, 0.15),
#                      limits = c(-0.15, 0.15), expand = c(0, 0)) +
#   
#   scale_x_continuous(breaks = c(1:length(order.parameters)),
#                      labels = order.parameters) +
#   coord_flip() +
#   xlab("") +
#   ylab("10-year difference in cumulative QALY (years)") +
#   #xlab("Average qaly difference between treatment and no treatment") +
#   
#   theme(plot.title = element_text(size = title_size, face = "bold"),
#         axis.line = element_line(linewidth = 0.5),     # Set axis line width
#         axis.ticks = element_line(linewidth = 0.5),    # Set axis tick width
#         axis.title.x = element_text(size = base_size_constant,
#                                     face = "bold",
#                                     margin = margin(t = x_margin, unit = "pt")),
#         axis.title.y = element_text(size = base_size_constant,
#                                     face = "bold",
#                                     margin = margin(r = y_margin, unit = "pt"))) +
#   
#   # # Manually set colors
#   # scale_fill_manual(values = custom_colors) +
#   
#   #theme(legend.position="none") +
#   ggtitle("NOAC vs. no NOAC treatment")
# 
# 
# 
# print(tornado_plot)

