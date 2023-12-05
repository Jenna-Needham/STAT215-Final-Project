## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
mean(data$Political_References)
sd(data$Political_References)
summary(data$Political_References)
table(data$Political_References)

mean(data$Intensity)
sd(data$Intensity)
summary(data$Intensity)
table(data$Intensity)


mean(data$Narrative_Tone)
sd(data$Narrative_Tone)
summary(data$Narrative_Tone)
table(data$Narrative_Tone)

mean(data$Listeners)
sd(data$Listeners)
summary(data$Listeners)
table(data$Listeners)


##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Political_References, data$Narrative_Tone)
contingency_table <- table(data$Political_References, data$Narrative_Tone)


##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
contingency_table <- table(data$Political_References, data$Narrative_Tone)
chisq.test(data$Political_References, data$Narrative_Tone)

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
# Perform ANOVA
anova <- aov(data$Intensity ~ data$Political_References, data = raw_data.csv)
# Summarize ANOVA results
summary(anova)

# get R2
# between/total
# OR between/(between+within)
752/(752+159747)


anova <- aov(data$Listeners ~ data$Political_References, data = raw_data.csv)
summary(anova)
3.999e+12/(3.999e+12+7.217e+16)




##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################

intensity_column <- raw_data.csv$Intensity
intensity_subset <- raw_data.csv[1:183, "Intensity"]

listeners_column <- raw_data.csv$Listeners
listeners_subset <- raw_data.csv[1:183, "Listeners"]

correlation_vector <- cor(data$Intensity, data$Listeners)
Intensity <- c(intensity_subset)
observed_listeners <- c(listeners_subset)
listeners_table <- data.frame(Intensity, observed_listeners)
listeners_table

summary(raw_data.csv$Intensity)
summary(raw_data.csv$Listeners)

cor(data$Intensity, data$Listeners)




# Assuming raw_data.csv is your data frame
# Replace 'Intensity' and 'Listeners' with the actual column names from your data

intensity_column <- raw_data.csv$Intensity
intensity_subset <- raw_data.csv[1:183, "Intensity"]

listeners_column <- raw_data.csv$Listeners
listeners_subset <- raw_data.csv[1:183, "Listeners"]

plot(listeners_table$Intensity, listeners_table$observed_listeners)

mean_listeners <- mean(listeners_table$observed_listeners)
mean_intensity <- mean(listeners_table$Intensity)
abline(v = mean_intensity, h = mean_listeners, col = "red")

correlation <- cor(Intensity, observed_listeners)
cat("Correlation between Mean Intensity and Mean Listeners:", correlation, "\n")




##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_plot <- plot(listeners_table$data.Political_References, listeners_table$observed_intensity)
print(linear_plot)

linear_relationship <- lm(observed_intensity ~ data$Political_References, data = listeners_table)
summary(linear_relationship)

abline(linear_relationship, col = "red")

plot(listeners_table$data.Political_References, residuals(linear_relationship))

abline(h = 0, col = "red")



##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################


# Add a line for the linear slope
abline(linear_relationship, col = "yellow")

# Add a line for the mean of X on the x-axis (vertical line)
abline(v = mean(listeners_table$data.Political_References), col = "blue")

# Add a line for the mean of Y on the y-axis (horizontal line)
abline(h = mean(listeners_table$observed_inte), col = "green")


##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################


# Plot the residuals
plot(listeners_table$data.Political_References, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


