## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline

## Load packages
# NOTE: Run base.R if these commands return an error!


library(readr)
library(dplyr)
library(ggplot2)
library(psych)

# Load data 
setwd("~/Documents/sta215")
dataset <- read_csv("raw_data.csv")
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
table("intended_age")
mean("intended_age")
sd("common_genre")

table(dataset$intended_age)
mean(dataset$intended_age)
sd(dataset$intended_age)
summary(dataset$intended_age)


table(dataset$simplicity)
mean(dataset$simplicity)
sd(dataset$simplicity)
summary(dataset$simplicity)


##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
aov(intended_age ~ common_genre, data = dataset)
boxplot(intended_age ~ common_genre, data = dataset)

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(dataset$common_genre, dataset$intended_age)
print(linear_plot)
meany <- mean(dataset$common_genre)
meanx <- mean(dataset$intended_age)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(common_genre ~ intended_age, data = dataset)
summary(linear_relationship)
abline(linear_relationship, col = "red")



##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset$intended_age, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(, dataset$song_topic)

chisq.test(table(dataset$common_genre, dataset$simplicity))