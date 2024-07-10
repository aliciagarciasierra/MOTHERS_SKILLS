#################################################
################ FIGURES #######################
#################################################

#------------ NOTE THAT ALL THE MODELS FROM THE NEXT SECTION ARE RUN --------#
#------------  IN THE FILE 02_ANALYSES.R ------------------------------------#


#------------ FIGURE 1


# Select relevant coefficients

coefs<-fixest::coefplot(list(model1,model2, model3, model4, model5, model6, model7, model8, model9, model10),
                        #drop=c("male"),
                        main="",
                        ci_level=0.95,
                        only.parms=TRUE) 

# Get the coefs and export to excel
coefs

# Import the excel file
coefs <- read_excel("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/THIRD PAPER/Presentation_Materials/coefplots.xlsx", sheet="Sheet1")

# Make sure variables ar numeric
coefs$estimate<-as.numeric(coefs$estimate)
coefs$ci_high<-as.numeric(coefs$ci_high)
coefs$ci_low<-as.numeric(coefs$ci_low)

# Arrange and mutate the dataset
figure1 <- coefs %>%
  arrange(estimate) %>%
  mutate(name = factor(variables, levels = c(
    "Mothers’ Mathematical Skills t-1",
    "Mothers’ Literacy Skills t-1"
  )))

# Create the plot
figure1 <- figure1 %>%
  ggplot(aes(variables, estimate, colour = Model)) +
  
  # Add horizontal line at y = 0
  geom_hline(yintercept = 0, colour = "gray", lty = 2) +
  
  # Add points with dodged position
  geom_point(position = position_dodge(width = 0.8)) +
  
  # Add linerange for confidence intervals
  geom_linerange(aes(x = variables, ymin = ci_low, ymax = ci_high),
                 position = position_dodge(width = 0.8),
                 lwd = 0.5) +
  
  # Add text labels for estimates
  geom_text(aes(x = variables, y = estimate, label = round(estimate, 2)),
            position = position_dodge(width = 0.8),
            vjust = 2, show.legend = FALSE) +
  
  # Customize the theme
  theme_pilot(
    axis_text_size = 13,
    legend_text_size = 13,
    legend_title_size = 13,
    legend_position = "right"
  ) +
  
  # Flip coordinates for better readability
  coord_flip() +
  
  # Customize guides and labels
  guides(color = guide_legend(reverse = TRUE)) +
  labs(x = "Variables", y = "Coefficients") +
  
  # Additional theme customization
  theme_pilot(axis_title_family = "Avenir Next Bold",
              axis_title_size = 14)

# Print Figure 1
figure1


#------------ FIGURE 2

# Create plot for graph 1 (model13 from 02_ANALYSES.R maths)
a <- ggiplot(
  model13, geom = 'ribbon', pt.join = TRUE, ci.lty = 0, ci.width = 0, ci.fill = TRUE,
  ci.fill.par = list(col = 'black', alpha = 0.3),
  xlab = "Time after changing occupations",
  ylab = "Maths estimate and 95% conf. int.",
  main = element_blank()
) +
  scale_x_continuous(limits = c(0, 14), breaks = seq(0, 14, 2)) +
  theme_pilot(
    axis_text_size = 13,
    legend_text_size = 13,
    legend_title_size = 13,
    legend_position = "right"
  ) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_pilot(
    axis_title_family = "Avenir Next Bold",
    axis_title_size = 14
  )

# Print plot a
a

# Create plot for graph 2 (model14 from 02_ANALYSES.R maths)
b <- ggiplot(
  model14, geom = 'ribbon', pt.join = TRUE, ci.lty = 0, ci.width = 0, ci.fill = TRUE,
  ci.fill.par = list(col = 'black', alpha = 0.3),
  xlab = "Time after changing occupations",
  ylab = "Reading estimate and 95% conf. int.",
  main = element_blank()
) +
  scale_x_continuous(limits = c(0, 14), breaks = seq(0, 14, 2)) +
  theme_pilot(
    axis_text_size = 13,
    legend_text_size = 13,
    legend_title_size = 13,
    legend_position = "right"
  ) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_pilot(
    axis_title_family = "Avenir Next Bold",
    axis_title_size = 14
  )

# Print plot b
b

# Arrange plots a and b side by side and print Figure 2
grid.arrange(a, b, ncol = 2, top = textGrob(" ", gp = gpar(fontfamily = "Avenir Next Demi Bold", fontsize = 18, font = 2)))

#------------ FIGURE 3


# Get coefficients from relevant models 
coefs <- fixest::coefplot(
  list(model19, model20),
  main = "",
  ci_level = 0.95,
  only.parms = TRUE
)

# Print the coefficients to check
print(coefs)

# Import the coefficients from excel
coefs <- read_excel("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/THIRD PAPER/Presentation_Materials/coefplots.xlsx", sheet = "Sheet7")
coefs$estimate <- as.numeric(coefs$estimate)
coefs$ci_high <- as.numeric(coefs$ci_high)
coefs$ci_low <- as.numeric(coefs$ci_low)

# PLOT
figure3 <- coefs %>%
  arrange(estimate) %>%
  mutate(name = factor(variables, levels = c(
    "Mothers’ Mathematical Skills t-1",
    "Mothers’ Literacy Skills t-1"
  ))) %>%
  ggplot(aes(variables, estimate, colour = Model)) +
  geom_hline(yintercept = 0, colour = "gray", lty = 2) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_linerange(aes(x = variables, ymin = ci_low, ymax = ci_high),
                 position = position_dodge(width = 0.8), lwd = 0.5) +
  geom_text(aes(x = variables, y = estimate, label = round(estimate, 2)),
            position = position_dodge(width = 0.8), vjust = 2, show.legend = FALSE) +
  coord_flip() +
  theme_pilot(
    axis_text_size = 13,
    legend_text_size = 13,
    legend_title_size = 13,
    legend_position = "bottom"
  ) +
  labs(x = "Variables", y = "Coefficients") +
  theme_pilot(
    axis_title_family = "Avenir Next Bold",
    axis_title_size = 14
  )

# Print the plot
print(figure3)


#------------ NOTE THAT ALL THE MODELS FROM THE NEXT SECTION ARE RUN --------#
#------------  IN THE FILE 03_ROBUSTNESS.R ------------------------------------#


#------------ FIGURE 4


#Get coefficients from the models of interest
coefs<-fixest::coefplot(list(model23, model24, model25, model26, model27, model28, model29, model30),
                        #drop=c("male"),
                        main="",
                        ci_level=0.95,
                        only.parms=TRUE) 


# Extract the coefs and export to excel
coefs

# Import the excel 
coefs <- read_excel("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/THIRD PAPER/Presentation_Materials/coefplots.xlsx", sheet="Sheet12")

# Make sure the variables are in the right format
coefs$estimate<-as.numeric(coefs$estimate)
coefs$ci_high<-as.numeric(coefs$ci_high)
coefs$ci_low<-as.numeric(coefs$ci_low)


# Plot figure
figure4 <- coefs %>%
  arrange(estimate) %>%
  mutate(name = factor(variables, levels = c(
    "Mothers’ Mathematical Skills t-1",
    "Mothers’ Literacy Skills t-1"
  ))) %>%
  ggplot(aes(variables, estimate, colour = Model)) +
  geom_hline(yintercept = 0, colour = "gray", lty = 2) +
  geom_point(position = position_dodge(width = 0.7)) +
  geom_linerange(aes(x = variables, ymin = ci_low, ymax = ci_high),
                 position = position_dodge(width = 0.7), lwd = 0.5) +
  geom_text(aes(x = variables, y = estimate, label = round(estimate, 3)),
            position = position_dodge(width = 0.7), vjust = 2, show.legend = FALSE) +
  coord_flip() +
  theme_pilot(
    axis_title_size = 14,
    axis_text_size = 13,
    legend_text_size = 13,
    legend_title_size = 13,
    legend_position = "right"
  ) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(x = "Variables", y = "Coefficients") +
  theme_pilot(axis_title_family = "Avenir Next Bold", axis_title_size = 14)

# Print figure
figure4




#------------ FIGURE 5


#Get coefficients from the models of interest
coefs<-fixest::coefplot(list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10),
                        #drop=c("male"),
                        main="",
                        ci_level=0.95,
                        only.parms=TRUE) 


# Extract the coefs and export to excel
coefs

# Import the excel 
coefs <- read_excel("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/THIRD PAPER/Presentation_Materials/coefplots.xlsx", sheet="Sheet9")

# Make sure the variables are in the right format
coefs$estimate<-as.numeric(coefs$estimate)
coefs$ci_high<-as.numeric(coefs$ci_high)
coefs$ci_low<-as.numeric(coefs$ci_low)


# Plot figure
figure5 <- coefs %>%
  arrange(estimate) %>%
  mutate(name = factor(variables, levels = c(
    "Mothers’ Mathematical Skills t-1",
    "Mothers’ Literacy Skills t-1"
  ))) %>%
  ggplot(aes(variables, estimate, colour = Model)) +
  geom_hline(yintercept = 0, colour = "gray", lty = 2) +
  geom_point(position = position_dodge(width = 0.7)) +
  geom_linerange(aes(x = variables, ymin = ci_low, ymax = ci_high),
                 position = position_dodge(width = 0.7), lwd = 0.5) +
  geom_text(aes(x = variables, y = estimate, label = round(estimate, 3)),
            position = position_dodge(width = 0.7), vjust = 2, show.legend = FALSE) +
  coord_flip() +
  theme_pilot(
    axis_title_size = 14,
    axis_text_size = 13,
    legend_text_size = 13,
    legend_title_size = 13,
    legend_position = "right"
  ) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(x = "Variables", y = "Coefficients") +
  theme_pilot(axis_title_family = "Avenir Next Bold", axis_title_size = 14)

# Print figure
figure5


#------------ FIGURE 6


#Get coefficients from the models of interest
coefs<-fixest::coefplot(list(model11, model12, model13, model14, model15, model16, model17, model18, model19, model20, model21, model22),
                        #drop=c("male"),
                        main="",
                        ci_level=0.95,
                        only.parms=TRUE) 


# Extract the coefs and export to excel
coefs

# Import the excel 
coefs <- read_excel("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/THIRD PAPER/Presentation_Materials/coefplots.xlsx", sheet="Sheet10")

# Make sure the variables are in the right format
coefs$estimate<-as.numeric(coefs$estimate)
coefs$ci_high<-as.numeric(coefs$ci_high)
coefs$ci_low<-as.numeric(coefs$ci_low)

# Plot
figure6 <- coefs %>%
  arrange(estimate) %>%
  mutate(name = factor(variables, levels = c(
    "Mothers’ Mathematical Skills t-1",
    "Mothers’ Literacy Skills t-1"
  ))) %>%
  ggplot(aes(variables, estimate, colour = Model)) +
  
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, colour = "gray", lty = 2) +
  
  # Add points with dodged position
  geom_point(position = position_dodge(width = 0.7)) +
  
  # Add linerange for confidence intervals
  geom_linerange(aes(x = variables, ymin = ci_low, ymax = ci_high),
                 position = position_dodge(width = 0.7), lwd = 0.5) +
  
  # Add text labels for estimates
  geom_text(aes(x = variables, y = estimate, label = round(estimate, 3)),
            position = position_dodge(width = 0.7), vjust = 2, show.legend = FALSE) +
  
  # Flip coordinates for better readability
  coord_flip() +
  
  # Customize the theme
  theme_pilot(
    axis_title_size = 14,
    axis_text_size = 13,
    legend_text_size = 13,
    legend_title_size = 13,
    legend_position = "right"
  ) +
  
  # Customize guides and labels
  guides(color = guide_legend(reverse = TRUE)) +
  labs(x = "Variables", y = "Coefficients") +
  
  # Additional theme customization
  theme_pilot(axis_title_family = "Avenir Next Bold", axis_title_size = 14)

# Print figure
figure6
