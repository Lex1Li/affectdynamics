# -------------------------------------------------#
#                  DATA WRANGLING                  #
# -------------------------------------------------#

# CLEANED DATA
# save(affect, cesd, data, overview, id, n, file = "/Users/Lexi/Desktop/internship/1_ data/datalexi.RData")
load("/Users/Lexi/Desktop/internship/1_ data/datalexi.RData")
load("/Users/Lexi/Desktop/internship/5_ results/3outs.RData")

##################################################
#                  1. LOAD DATA                  #
##################################################
affect <- read.csv("/Users/Lexi/Desktop/internship/4_ data0314/affect.csv", sep =";")
cesd <- read.csv("/Users/Lexi/Desktop/internship/4_ data0314/cesd.csv", sep =";")



######################################################
#                  1. LOAD PACKAGES                  #
######################################################
library(plyr)
library(ClusterVAR)
library(tidyverse)
library(xtable)
library(brms)
library(RColorBrewer)
library(grid)
library(ggplot2)



source("/Users/Lexi/Desktop/internship/internship/Lexi_Helpers.R")

#####################################################################
#                  2. REVERSE CESD STANDARDISATION                  #
#####################################################################

### from paper ###
  # values after within-dataset standardization (Koval et.al 2013)
  # mean 14.52 
  # sd 9.78

reverse <- function(standardised_values, mean, sd) {
  original_values <- (standardised_values * sd) + mean
  return(original_values)
}

cesd$original <- reverse(cesd$CESD, 14.52, 9.78)



#########################################################
#                  3. OVERVIEW OF DATA                  #
#########################################################
data <- affect[,-c(4,5)] # remove PA and NA columns

overview <- ddply(data, .(PID), function(x) nrow(x)) # measurements per person
colnames(overview)[2] <- "n_timepoints"

mean(overview$n_timepoints)
hist(overview$n_timepoints)
id <- overview$PID
n <- length(id)



##################################################################
#                4. FORMAT ADJUSTMENT FOR TS PLOT                #
##################################################################
data <- data %>%
  mutate(ID = PID) %>%
  group_by(PID) %>%
  mutate(Time = OCCASION) %>%
  arrange(Time, .by_group = TRUE) %>%
  mutate(DayNr = dense_rank(factor(UNIT, levels = unique(UNIT)))) %>%
  group_by(PID, DayNr) %>%
  mutate(Beep = row_number()) %>%
  ungroup() %>% 
  select(-PID, -OCCASION, -UNIT) %>% 
  select(ID, DayNr, Beep, Time, everything())

data <- as.data.frame(data)



######################################################
#                5. SAVE CLEANED DATA                #
######################################################
# save(affect, cesd, data, overview, id, n, file = "/Users/Lexi/Desktop/internship/1_ data/datalexi.RData")


