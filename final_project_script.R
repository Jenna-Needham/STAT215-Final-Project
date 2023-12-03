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
raw_data.csv <- read.csv("raw_data.csv")
anova_result <- aov(data$Intensity ~ data$Political_References, data = raw_data.csv)
print(anova_result)
summary(anova_result)
hist(raw_data.csv$Political_References)

plot1 <- ggplot(raw_data.csv, aes(x = Intensity, y = Political_References, color = Intensity, label = Observations)) +
  geom_point(size = 3) +
  ggtitle("Intensity by Political_References") +
  ylab("Political_References") +
  xlab("Intensity") +
  geom_text(nudge_y = 1)
print(plot1)

# Assuming 'data' is your data frame
mean_scores <- aggregate(Intensity ~ Political_References, data = data, FUN = mean)
# Assuming 'mean_scores' is the aggregated data
plot2 <- ggplot(mean_scores, aes(x = Intensity, y = Political_References)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Intensity by Political_References", x = "Intensity", y = "Political References")
print(plot2)

raw_data.csv$difference <- raw_data.csv$Political_References - mean(raw_data.csv$Political_References)

plot3 <- ggplot(raw_data.csv, aes(x = Intensity, y = difference, color = Intensity)) + 
  geom_point(size = 3) +
  ggtitle("Intensity Deviation by Political References") +
  ylab("Deviation from Mean Score")
print(plot3)

raw_data.csv$difference2 <- raw_data.csv$difference*raw_data.csv$difference
sum(raw_data.csv$difference2)
270.8634/(183)
var(raw_data.csv$difference)


# Perform ANOVA
anova <- aov(data$Intensity ~ data$Political_References, data = raw_data.csv)
# Summarize ANOVA results
summary(anova)
# get R2
# between/total
# OR between/(between+within)
752/(752+159747)



##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
# Assuming your data is in a data frame named 'your_data'

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


intensity_column <- raw_data.csv$Intensity
intensity_subset <- raw_data.csv[1:183, "Intensity"]

listeners_column <- raw_data.csv$Listeners
listeners_subset <- raw_data.csv[1:183, "Listeners"]

observation_column <- raw_data.csv$Observations
observation_subset <- raw_data.csv [1:183, "Observations"]


listeners_table<- data.frame(
  Observations = c(
   observation_subset
  ),
  Intensity = c(
    intensity_subset
  ),
  observed_listeners = c(
    listeners_subset
  )
)

linear_plot <- plot(listeners_table$Intensity, listeners_table$observed_listeners)
print(linear_plot)

linear_relationship <- lm(observed_listeners ~ Intensity, data = listeners_table)
summary(linear_relationship)

abline(linear_relationship, col = "red")

plot(listeners_table$Intensity, residuals(linear_relationship))

abline(h = 0, col = "red")



##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################


# Add a line for the linear slope
abline(linear_relationship, col = "yellow")

# Add a line for the mean of X on the x-axis (vertical line)
abline(v = mean(listeners_table$Intensity), col = "blue")

# Add a line for the mean of Y on the y-axis (horizontal line)
abline(h = mean(listeners_table$observed_listeners), col = "green")


##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################


# Plot the residuals
plot(listeners_table$Intensity, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


