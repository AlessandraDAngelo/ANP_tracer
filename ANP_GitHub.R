# R script for calculating the Arctic Nitrate-Phosphate tracer (Jones et al., 1998; Newton et al., 2013)
# D'Angelo A. and Loose B. (2021) - University of Rhode Island, Graduate School of Oceanography

# From Jones et al. (1998):
# NO3AW = 17.499 * PO4AW - 3.072, R = 0.9735 (AW)
# NO3PW = 12.368 * PO4PW - 10.549, R = 0.9915 (PW)
# for the Atlantic and Pacific water, respectively

# Plot the data within the Jones et al. (1998) model
library(ggplot2)
data %>%
  ggplot(aes(x = Phosphate, y = NO3aw)) +
  geom_point(size = 5, alpha = 0) +
  geom_smooth(color = "orange", size = 2) +
  geom_point(y = NO3pw, size = 5, alpha = 0) +
  geom_smooth(aes(x = Phosphate, y = NO3pw), color = "blue", size = 2) +
  geom_point(y = data$Nitrate.Nitrite, size = 5, pch = 21, fill = "red", color = "black") +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  labs(x = "Phosphate (uM)", y = "Nitrate + Nitrite (uM)", title = "Nitrate/Phosphate relationships, using Jones et al. (1998) model", cex.axis = 0.50) +
  theme(axis.text.x = element_text(hjust = 0.5, size = 13), axis.text.y = element_text(size = 13)) +
  theme(legend.position = "right")

ggsave("N_P_plot")

# Load necessary libraries
library(dplyr)

# Calculate NO3AW and NO3PW
NO3aw <- (17.499 * data$Phosphate) - 3.072
NO3pw <- (12.368 * data$Phosphate) - 10.549

# ANP definition (Newton et al., 2013): distance to AWFit / (distance to PWFit) + (distance to AWFit)
# Calculate distance to AWFit and PWFit
Distance_AW <- abs(data$Nitrate.Nitrite - 17.499 * data$Phosphate + 3.072) / sqrt((1^2) + (17.499^2))
Distance_PW <- abs(data$Nitrate.Nitrite - 12.368 * data$Phosphate + 10.549) / sqrt((1^2) + (12.368^2))

# Calculate ANP
data$ANP <- Distance_AW / (Distance_PW + Distance_AW)

#References: 
#Jones, E. P., Anderson, L. G., and Swift, J. H. 1998. Distribution of Atlantic and Pacific waters in the upper Arctic Ocean: implications for circulation. Geophys Res Lett, 25, 765–768. https://doi.org/10.1029/98GL00464.
#Newton, R., Schlosser, P., Mortlock, R., Swift, J., and MacDonald, R. 2013. Canadian Basin freshwater sources and changes: Results from the 2005 Arctic Ocean Section. J Geophys Res Oceans, 118, 2133–2154. https://doi.org/10.1002/jgrc.20101.