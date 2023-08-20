library(ggplot2)
library(dplyr)
library(extrafont)

# Register the fonts for use
font_import(pattern = "cmss")
loadfonts(device = "win")

# Specify the font family
font_family <- "cmss"


lm_cat <- read.csv('lm_cat.csv')
lm_case <- read.csv('lm_case.csv')
lm_cat_1factor <- read.csv('cat_basic.csv')
fillers <- read.csv("fillers.csv")

# CAT PLOT

mean_positions <- aggregate(rating ~ condition, data = lm_cat_1factor, FUN = mean)
lm_cat_1factor$condition <- factor(lm_cat_1factor$condition, levels = c("LCATM", "UCATM", "LCATU", "UCATU"),
                                   labels = c("LCAT-LF", "UCAT-LF", "LCAT-UF", "UCAT-UF"))
color_mapping <- c("LCAT-LF" = "grey85", 
                   "UCAT-LF" = "grey85",   
                   "LCAT-UF" = "grey55",
                   "UCAT-UF" = "grey55")

## Calculate the mean positions for each condition
mean_positions <- aggregate(rating ~ condition, data = lm_cat_1factor, FUN = mean)

## Create the boxplot
p_cat <- ggplot(lm_cat_1factor, aes(x = condition, y = rating, fill = condition)) +
  geom_boxplot() +
  scale_fill_manual(values = color_mapping) +
  labs(x = "Coordination Type", y = "Score") +
  theme_minimal() +
  guides(fill = FALSE) +
  geom_text(data = mean_positions, aes(label = paste("Mean =", round(rating, 2))), 
            vjust = 0, color = "black", size = 3) +
  scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 1)) +
  theme(
    plot.background = element_rect(fill = "white"),  # Set plot background to white
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    panel.grid.major = element_line(color = "grey85"),  # Adjust grid lines if needed
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.x = element_text(size = 15, margin = margin(t = 5)),  # Adjust x-axis label size and spacing
    axis.text.y = element_text(size = 15),  # Adjust y-axis label size
    axis.title.x = element_text(size = 17, margin = margin(t = 25)),  # Adjust x-axis title size and spacing
    axis.title.y = element_text(size = 17, margin = margin(r = 10)),  # Adjust y-axis title size and spacing
    plot.margin = margin(25, 20, 10, 10, "pt"),  # Adjust overall plot margins
    legend.position = "none"  # Remove legend
  )

## Save the plot
ggsave("ucat_plot.png", p_cat, width = 7, height = 6, dpi = 700)
ggsave("ucat_plot.eps", p_cat, width = 7, height = 6, dpi = 700, device = "eps")




# CASE PLOT

lm_case$condition <- factor(lm_case$condition, levels = c("LCASEM", 
                                                          "UCASEM", 
                                                          "UCASEU"),
                            labels = c("LCASE-LF", 
                                       "UCASE-LF", 
                                       "UCASE-UF"))

mean_positions <- aggregate(rating ~ condition, data = lm_case, FUN = mean)

color_mapping <- c("LCASE-LF" = "grey85", 
                   "UCASE-LF" = "grey85",   
                   "UCASE-UF" = "grey55")


## Create the boxplot
p_case <- ggplot(lm_case, aes(x = condition, y = rating, fill = condition)) +
  geom_boxplot() +
  scale_fill_manual(values = color_mapping) +
  labs(x = "Coordination Type", y = "Score") +
  theme_minimal() +
  guides(fill = FALSE) +
  geom_text(data = mean_positions, aes(label = paste("Mean =", round(rating, 2))), 
            vjust = 0, color = "black", size = 3) +
  scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 1)) +
  theme(
    plot.background = element_rect(fill = "white"),  # Set plot background to white
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    panel.grid.major = element_line(color = "grey85"),  # Adjust grid lines if needed
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.x = element_text(size = 15, margin = margin(t = 5)),  # Adjust x-axis label size and spacing
    axis.text.y = element_text(size = 15),  # Adjust y-axis label size
    axis.title.x = element_text(size = 17, margin = margin(t = 25)),  # Adjust x-axis title size and spacing
    axis.title.y = element_text(size = 17, margin = margin(r = 10)),  # Adjust y-axis title size and spacing
    plot.margin = margin(25, 20, 10, 10, "pt"),  # Adjust overall plot margins
    legend.position = "none"  # Remove legend
  )
## Save the plot
ggsave("ucase_plot.png", p_case, width = 7, height = 6, dpi = 700)
ggsave("ucase_plot.eps", p_case, width = 7, height = 6, dpi = 700, device = "eps")
