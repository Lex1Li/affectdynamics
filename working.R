set.seed(17)
rnd_sub <- id[sample(1:n, size=3)]

# PlotTS_Flex(data = data,
#             IDcol = "PID",
#             ID = id[1], # Subject number, here fixed
#             variable = "Happy", # Variable
#             ylab = TRUE,
#             xlim = c(1,200))


PlotTS_Flex(data = data,
            IDcol = "SEMA_ID",
            ID = rnd_sub[1], # Subject number, here fixed
            variable = "Happy", # Variable
            layout = FALSE,
            title = FALSE,
            ylab = TRUE,
            xlim = c(1,200))


PlotTS_Flex <- function(data,
                        IDcol, # column with subject ID
                        ID, # subject ID
                        variable, # variable colname
                        variable2 = NULL,
                        layout = FALSE,
                        title = TRUE,
                        ylab = FALSE,
                        xlim = NULL,
                        trend = TRUE,
                        resLegend = FALSE) {
  
  # Subset data
  data_ss <- data[data[[IDcol]] == ID, ] # get data of specified participant
  Nt_ss <- nrow(data_ss)
  if(is.null(xlim)) xlim <- c(1, Nt_ss) # set xlim if wasn't defined
  
  # Layout
  if(layout) layout(matrix(1:2, ncol=2), widths = c(1, .35)) # 2 plots width 1 & .35
  
  # LinePlot
  par(mar=c(4,3,2,1))
  plot.new()
  plot.window(xlim=xlim, ylim=c(0, 100))
  if(ylab) title(ylab=variable, line=2.25)
  axis(1)
  axis(2, las=2)
  grid()
  # Plot Data
  lines(data_ss[, variable])
  # abline(h=c(0,100))
  
  # # Second variable (I will use that for predictions later)
  # if(!is.null(variable2)) lines(data_ss[, variable2], col="orange")
  # 
  # if(trend) {
  #   time <- 1:nrow(data_ss)
  #   lm_obj <- lm(data_ss[, variable]~time)
  #   abline(lm_obj, lwd=1, col="black", lty=2)
  # }
  # 
  # if(resLegend) legend("topright", legend=c("Data", "Predictions"), bty="n", text.col=c("black", "orange"))
  # 
  # if(!is.null(title)) {
  #   if(title==TRUE) title(main=paste0(variable, " (person = ", u_pers[j], ")"), font.main=1)
  #   if(class(title) == "character") title(title, , font.main=1)
  # }
  
  
  # Marginal
  par(mar=c(4,0,2,1))
  hist_data <- hist(data_ss[, variable], plot = FALSE, breaks=seq(0, 100, length=20))
  barplot(hist_data$counts,
          horiz = TRUE,  # Horizontal bars
          names.arg = NULL,
          axes=FALSE)
  x_seq <- seq(0, 100, length=1000)
  gauss_den <- dnorm(x_seq,
                     mean = mean(data_ss[, variable], na.rm = TRUE),
                     sd = sd(data_ss[, variable], na.rm = TRUE))
  scaled_den <- (gauss_den/max(gauss_den) ) * max(hist_data$counts)
  lines(scaled_den, seq(0, 24, length=1000), col="red") # Not entiresure where those 22 come from
  
} # eoF

# sure
x <- hist(p2$Happy)
barplot(p2$Happy)

hist(p2$Happy)
barplot(x$counts)


?lines

#################################################
#                IND PARICIPANTS                #
#################################################

p2 <- data[data$PID == 2,]
table(p2$UNIT)


p3 <- data[data$PID == 3,]
table(p3$UNIT)


### TS PLOT ###
######################################################################
#                FORMAT ADJUSTMENT NEEDED FOR TS PLOT                #
######################################################################

# needs:
# Time -> 1-66
# Beep -> 1-10 within day 
# DayNr -> 1-7 


##### TRIAL ON P2 #####
p2 <- data[data$PID == 2,]


data <- data %>%
  group_by(PID) %>%
  mutate(
    Time = OCCASION
  ) %>%
  arrange(Time, .by_group = TRUE) %>%
  mutate(
    DayNr = dense_rank(factor(UNIT, levels = unique(UNIT)))
  ) %>%
  group_by(PID, DayNr) %>%
  mutate(
    Beep = row_number()
  ) %>%
  ungroup()

data <- as.data.frame(data)


#########################################################
#                TRY FUNCTION ON MY DATA                #
#########################################################
variable <- "Happy"
# Marginal
par(mar=c(4,0,2,1))
hist_data <- hist(data_ss[, variable], plot = FALSE, breaks=seq(0, 100, length=20))
barplot(hist_data$counts,
        horiz = TRUE)
x_seq <- seq(0, 100, length=1000)
gauss_den <- dnorm(x_seq,
                   mean = mean(data_ss[, variable], na.rm = TRUE),
                   sd = sd(data_ss[, variable], na.rm = TRUE))
scaled_den <- (gauss_den/max(gauss_den) ) * max(hist_data$counts)
# lines(scaled_den, x_seq, col="grey")
lines(scaled_den, seq(0, 24, length=1000), col="red") # Not entiresure where those 22 come from


##### PLOT TRIAL #####

for(i in 1:3) {
  PlotTS_Flex(data = data,
              IDcol = "ID",
              ID = rnd_sub[1], # Subject number, here fixed
              variable = "Happy", # Variable
              layout = TRUE,
              title = TRUE,
              ylab = TRUE,
              xlim = c(1, 80))
}



#########################################
#                EDIT BV                #
#########################################
### BV PLOT ###
plotBV_flex <- function(data,
                        IDcol,
                        ID,
                        variable1,
                        variable2,
                        lag = FALSE,
                        title = TRUE,
                        fit = TRUE,
                        para = FALSE,
                        diag = FALSE,
                        xlim = NULL,
                        R2=FALSE) {
  
  # Subset data
  data_ss <- data[data[[IDcol]] == ID, ]
  Nt_ss <- nrow(data_ss)
  if(is.null(xlim)) xlim <- c(1, Nt_ss)
  
  # Canvas
  par(mar=c(4,4,2,1))
  plot.new()
  plot.window(xlim=c(0, 100), ylim=c(0, 100))
  axis(1)
  axis(2, las=2)
  grid()
  if(diag) abline(0, 1, col="grey")
  if(lag) {
    title(xlab = bquote(.(variable1)[t-1]),
          ylab = bquote(.(variable2)[t]),
          line = 2.5)
  } else {
    title(xlab=variable1, ylab=variable2, line=2.5)
  }
  if(title) title(main=paste0("Person ", ID), font.main=1)
  
  # Plot Data
  if(lag) {
    x1 <- data_ss[-Nt_ss, variable1]
    x2 <- data_ss[-1, variable2]
  } else {
    x1 <- data_ss[, variable1]
    x2 <- data_ss[, variable2]
  }
  points(x1, x2, pch=20, cex=1)
  lm_obj <- lm(x2 ~ x1)
  if(fit) abline(lm_obj, lwd=2, col="red")
  
  # Add regression results
  if(para) text(80, 7, paste0("a = ",
                              round(coef(lm_obj)[1], 2),
                              ", b = ",
                              round(coef(lm_obj)[2], 2)),
                col="red")
  
  # ADD R2
  if(R2) {
    r2 <- cor(x1, x2, use="complete.obs")^2
    r2 <- round(r2, 2)
    text(20, 80, bquote(R^2 == .(r2)))
    
  }
  
} # eoF



### TRY PLOT ###
plotBV_flex(data = data,
            IDcol = "ID",
            ID = rnd_sub[1], # Subject number, here fixed
            variable1 = "Happy",
            variable2 = "Happy", # Variable
            lag = TRUE,
            title = FALSE,
            para = TRUE)




############################################
#                TRIAL PLOT                #
############################################
# Layout
lmat <- matrix(4:12, 3, 3, byrow = TRUE)
lmat <- cbind(1:3, lmat)
lo <- layout(lmat, widths = c(0.15, 1, .25, 1), heights = c(1,1,1))
layout.show(lo)

# Plot Labels
for(i in 1:3)   plotLabel(paste0("     Person ", LETTERS[i]), srt = 90)

# Plot 
for(i in 1:3) {
  PlotTS_Flex(data = data,
              IDcol = "ID",
              ID = rnd_sub[i], # Subject number, here fixed
              variable = "Happy", # Variable
              layout = FALSE,
              title = TRUE,
              ylab = TRUE,
              xlim = c(1, 80))
  
  plotBV_flex(data = data,
              IDcol = "ID",
              ID = rnd_sub[i], # Subject number, here fixed
              variable1 = "Happy",
              variable2 = "Happy", # Variable
              lag = TRUE,
              title = FALSE,
              para = TRUE)
}


###################################################################
#                TS PLOTS FOR ALL AFFECT AND ALL N                #
###################################################################
pdf("/Users/Lexi/Desktop/internship/4_ plots/TimeSeriesPlots.pdf", width=9, height=4)
variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")

# Loop over persons
for(j in 1:n) {
  # Make Layout
  lmat <- matrix(1:16, 2, 8, byrow = TRUE)
  lo <- layout(lmat, widths = c(1, .25, 1, .25, 1, .25, 1, .25), heights = c(1,1))
  
  # Make plot for person j
  for(i in 1:7)   PlotTS_Flex(data = data,
                              IDcol = "ID",
                              ID = id[j], # Subject number, here fixed
                              variable = variables[i], # Variable
                              layout = FALSE,
                              title = TRUE,
                              ylab = TRUE,
                              xlim = c(1, 80))
  plot.new(); plot.new()
  print(j)
}

dev.off()



source("/Users/Lexi/Desktop/internship/2_ code/Lexi_Helpers.R")
pdf("/Users/Lexi/Desktop/internship/4_ plots/1_ timeseries.pdf", width=8, height=8)

# Loop over persons
for(j in 1:n) {
  # Make Layout
  lmat <- matrix(1:16, 4, 4, byrow = TRUE)
  lo <- layout(lmat, widths = c(1, .3, 1, .3), heights = c(1,1,1,1))
  
  # ID panel
  par(mar = c(0, 0, 0, 0))  # remove all margins
  plot.new()
  text(0.6, 0.6, paste("Participant ID:", id[j]), cex = 1, font = 2)
  plot.new()
  
  # Make plot for person j
  for(i in 1:7)   PlotTS_Flex(data = data,
                              IDcol = "ID",
                              ID = id[j], # Subject number, here fixed
                              variable = variables[i], # Variable
                              layout = FALSE,
                              title = TRUE,
                              ylab = TRUE,
                              xlab = TRUE,
                              xlim = c(1, 80))
  
  print(j)
}

dev.off()


###################################################################
#                BV PLOTS FOR ALL AFFECT AND ALL N                #
###################################################################
source("/Users/Lexi/Desktop/internship/2_ code/Lexi_Helpers.R")

pdf("/Users/Lexi/Desktop/internship/4_ plots/2_ bv plots.pdf", width=9, height=4)

# Loop over persons
for(j in 1:n) {
  # Make Layout
  lmat <- matrix(1:8, 2, 4, byrow = TRUE)
  lo <- layout(lmat, widths = c(1, 1, 1, 1), heights = c(1,1))
  
  # ID panel
  plot.new()
  text(0.5, 0.5, labels = paste("Participant ID:", id[j]), cex = 1.5)
  
  # Make plot for person j
  for(i in 1:7)   plotBV_flex(data = data,
                              IDcol = "ID",
                              ID = id[j], # Subject number, here fixed
                              variable1 = variables[i],
                              variable2 = variables[i], # Variable
                              lag = TRUE,
                              title = FALSE,
                              para = TRUE)
  print(j)
}

dev.off()


plotBV_flex(data = data,
            IDcol = "ID",
            ID = id[j], # Subject number, here fixed
            variable1 = variables[i],
            variable2 = variables[i], # Variable
            lag = TRUE,
            title = TRUE,
            para = TRUE)



### p10 troubleshoot
p10 <- data[data$ID == 12,]

lmat <- matrix(1:8, 2, 4, byrow = TRUE)
lo <- layout(lmat, widths = c(1, 1, 1, 1), heights = c(1,1))

# Make plot for person j
for(i in 1:7)   plotBV_flex(data = p10,
                            IDcol = "ID",
                            ID = 12, # Subject number, here fixed
                            variable1 = variables[i],
                            variable2 = variables[i], # Variable
                            lag = TRUE,
                            title = TRUE,
                            para = TRUE)
plot.new()

summary(data_ss[, variable1])
summary(data_ss[, variable2])



lmat <- matrix(1:16, 2, 8, byrow = TRUE)
lo <- layout(lmat, widths = c(1, .25, 1, .25, 1, .25, 1, .25), heights = c(1,1))
layout.show(lo)




# Marginal
hist_data <- hist(data_ss[, "Happy"], plot = FALSE, breaks=seq(0, 100, length=20))
barplot(hist_data$counts,
        horiz = TRUE)
x_seq <- seq(0, 100, length=1000)
gauss_den <- dnorm(x_seq,
                   mean = mean(data_ss[, variable], na.rm = TRUE),
                   sd = sd(data_ss[, variable], na.rm = TRUE))
scaled_den <- (gauss_den/max(gauss_den) ) * max(hist_data$counts)
lines(scaled_den, seq(0, max(hist_data$counts), length=1000), col="black") # Not entiresure where those 22 come from


layout(1:1)




###############################################
#                FITTING TRIAL                #
###############################################
library(plyr)
library(ClusterVAR)
library(tidyverse)
library(xtable)
library(brms)

data[5:11]

out_1to6 <- LCVAR(Data = data,
                  yVars = 5:11,
                  xContinuous = 4,
                  Beep = 3,
                  Day = 2,
                  ID = 1,
                  Lags = 1,
                  Clusters = 1:6,
                  Cores = 9,
                  Rand = 75,
                  RndSeed = 1) # Reproducibility

# saveRDS(out_1to6, "/Users/Lexi/Desktop/internship/5_ results/1to6.RDS")
# out_1to6 <- readRDS("/Users/Lexi/Desktop/internship/5_ results/1to6.RDS")

sum <- summary(out_1to6, show="GNL")
sum$FunctionOutput$Proportions


plot(out1_1to6, show = "GNL")


# cluster inspection

three <- coef.ClusterVAR(out_1to6, c(1,1,1))

summary(three)

k <- 3
sc <- 1
pdf("/Users/Lexi/Desktop/internship/4_ plots/4_ k3heat.pdf", width=6*sc, height=6*sc)

plot(out1_1to6, show = "specific",
     Model = rep(1,k),
     labels = variables,
     mar = c(2.5,3.75,2,0.5))

dev.off()



#####################################################################
#                PLOTTING CLUSTER FIT FOR ALL MODELS                #
#####################################################################

# Layout
lmat <- matrix(1:6, 2, 3, byrow = TRUE)
lo <- layout(lmat)
# layout.show(lo)

par(col.main='white') # Switch the plot title colour to white.
plot(out1_1to6, show = "GNL")
par(col.main='black') # Switch back to black.
title("Seed 1", font.main=1)

par(col.main='white') # Switch the plot title colour to white.
plot(out2_1to6, show = "GNL")
par(col.main='black') # Switch back to black.
title("Seed 2", font.main=1)


# Layout
lmat <- matrix(1:6, 2, 3, byrow = TRUE)
lo <- layout(lmat, heights = c(1, 1), widths = c(1,1,1))
x <- plot(out1_1to6, show = "GNL")
plot(1:10, 1:10)
plot(1:11, 1:11)

help(layout)




##############################################
#                FUCKING CODE                #
##############################################

# write function
title <- paste0("Seed ", 1)

PlotIC <- function(model,
                   title = NULL) {
  model_summary <- summary(model, show="GNL")
  out_table <- model_summary$FunctionOutput
  cols <- c("#E41A1C", "#377EB8", "#4DAF4A")
  
  K <- nrow(out_table) # number of models
  labels <- c(1:K)
  yrange <- range(c(out_table$BIC, out_table$ICL))
  
  # canvas
  par(mar=c(4.4, 5.5, 2, 1.2))
  plot.new()
  plot.window(xlim=c(1,K), ylim=yrange)
  
  axis(1, labels = labels, at=1:K)
  axis(2, round(seq(yrange[1], yrange[2], length=8)), las=2)
  
  title(xlab="Number of clusters")
  title(ylab="Information Criterion", line=4.5)
  title(title, font.main=1)
  
  # plot data
  points(1:K, out_table$ICL, col=cols[1], pch=19)
  points(1:K, out_table$BIC, col=cols[2], lty=2, pch=17)
  lines(1:K, out_table$ICL, col=cols[1])
  lines(1:K, out_table$BIC, col=cols[2], lty=2)
  
  
  # legend
  legend("topright", legend=c("ICL", "BIC"),
         lty=1, col=cols[1:2], text.col=cols[1:2],
         bty="n", cex=1.2, pch=c(19, 17))
}

PlotIC(out1_1to6, title = "Seed 1")
PlotIC(out2_1to6)



lmat <- matrix(1:6, 2, 3, byrow = TRUE)
lo <- layout(lmat, heights = c(1, 1), widths = c(1,1,1))
layout.show(lo)

PlotIC(out1_1to6, title = "Seed 1")
PlotIC(out2_1to6, title = "Seed 2")
PlotIC(out3_1to6, title = "Seed 3")
PlotIC(out4_1to6, title = "Seed 4")
PlotIC(out5_1to6, title = "Seed 5")


##################################################
#                UP TO 8 CLUSTERS                #
##################################################
out1_1to8 <- LCVAR(Data = data,
                  yVars = 5:11,
                  xContinuous = 4,
                  Beep = 3,
                  Day = 2,
                  ID = 1,
                  Lags = 1,
                  Clusters = 1:8,
                  Cores = 9,
                  Rand = 75,
                  RndSeed = 1)


###########################################################################
#                LOOP THRU 8 CLUSTERS WITH DIFFERENT SEEDS                #
###########################################################################

model_summary <- summary(model, show="GNL")
out_table <- model_summary$FunctionOutput

model_summary$FunctionOutput$BIC

K <- nrow(out_table) 
labels <- c(1:K)
yrange <- range(c(out_table$BIC, out_table$ICL))

# just need BIC from out_table of each model

# run seed 
# only store BIC of each seed

#     seed1 seed2 seed3
# c1
# c2
# c3
# ...




BIC_1to8 <- as.data.frame(
  matrix(NA, 8, 50, dimnames = list(paste(1:8, 'Clusters'), paste('Seed', 1:50))))
  
for (i in 1:50) {
  model <- LCVAR(Data = data,
                 yVars = 5:11,
                 xContinuous = 4,
                 Beep = 3,
                 Day = 2,
                 ID = 1,
                 Lags = 1,
                 Clusters = 1:8,
                 Cores = 9,
                 Rand = 75,
                 RndSeed = i)
  
  model_sum <- summary(model, show="GNL")
  BIC_1to8[ , i] <- model_sum$FunctionOutput$BIC
}


BICrob_1to8

for (i in 1:2) {
  print(BICrob_1to8[, i])
}

test <- BICrob_1to8[, 1]

K <- length(test) # number of models
labels <- c(1:K)
yrange <- range(test)



PlotBIC <- function(BIC_singlecol, title = NULL) {
  K <- length(BIC_singlecol) # number of models
  labels <- c(1:K)
  yrange <- range(BIC_singlecol)
  
  # canvas
  par(mar=c(4.4, 5.5, 2, 1.2))
  plot.new()
  plot.window(xlim=c(1,K), ylim=yrange)
  
  axis(1, labels = labels, at=1:K)
  axis(2, round(seq(yrange[1], yrange[2], length=8)), las=2)
  
  title(xlab="Number of clusters")
  title(ylab="BIC", line=4.5)
  title(title, font.main=1)
  
  # plot data
  points(1:K, BIC_singlecol, pch=19, col = "red")
  lines(1:K, BIC_singlecol, col = "red")
}

for (i in 1:2) {
  PlotBIC(BICrob_1to8[, i], title = paste0("Seed ", i))
}




##### run model 50 times

BIC_1to8 <- as.data.frame(
  matrix(NA, 8, 30, dimnames = list(paste(1:8, 'Clusters'), paste('Seed', 1:30))))

for (i in 1:30) {
  model <- LCVAR(Data = data,
                 yVars = 5:11,
                 xContinuous = 4,
                 Beep = 3,
                 Day = 2,
                 ID = 1,
                 Lags = 1,
                 Clusters = 1:8,
                 Cores = 9,
                 Rand = 75,
                 RndSeed = i)
  
  model_sum <- summary(model, show="GNL")
  BIC_1to8[ , i] <- model_sum$FunctionOutput$BIC
  
  print(paste0("Finished seed ", i))
}


# save(BIC_1to8, file = "/Users/Lexi/Desktop/internship/5_ results/Robustness_ 1to8.RData")



##### PLOT THE 30 TRIALS
pdf("/Users/Lexi/Desktop/internship/4_ plots/6_ BIC 1to8.pdf", width=7, height=10)

lmat <- matrix(1:15, 5, 3, byrow = TRUE)
lo <- layout(lmat, heights = c(1, 1,1,1,1), widths = c(1,1,1))
# layout.show(lo)

for (i in 1:30) {
  PlotBIC(BIC_1to8[, i], title = paste0("Seed ", i))
}

dev.off()



#######################################################
#                SAMPLE 3 RANDOM SEEDS                #
#######################################################
set.seed(13535099)
random_seed <- sample(1:1e7, 3)



######################################################
#                RANDOM SEED 1, 2 & 3                #
######################################################
out_seed1 <- LCVAR(Data = data,
                    yVars = 5:11,
                    xContinuous = 4,
                    Beep = 3,
                    Day = 2,
                    ID = 1,
                    Lags = 1,
                    Clusters = 1:6,
                    Cores = 9,
                    Rand = 75,
                    RndSeed = random_seed[1])

out_seed2 <- LCVAR(Data = data,
                   yVars = 5:11,
                   xContinuous = 4,
                   Beep = 3,
                   Day = 2,
                   ID = 1,
                   Lags = 1,
                   Clusters = 1:6,
                   Cores = 9,
                   Rand = 75,
                   RndSeed = random_seed[2])

out_seed3 <- LCVAR(Data = data,
                   yVars = 5:11,
                   xContinuous = 4,
                   Beep = 3,
                   Day = 2,
                   ID = 1,
                   Lags = 1,
                   Clusters = 1:6,
                   Cores = 9,
                   Rand = 75,
                   RndSeed = random_seed[3])

# save(out_seed1, out_seed2, out_seed3, file = "/Users/Lexi/Desktop/internship/5_ results/3outs.RData")



################################################################
#                SEED 1 PLOT SCALED BIC AND NLL                #
################################################################
PlotIC(out_seed1, title = "Random Seed 1")
sum1 <- summary(out_seed1, show = "GNL")


##### SCALE BIC AND NLL

# Get negLL
negLL <- -sum1$FunctionOutput$`log-likelihood`
negLL_scaled <- negLL/max(negLL)

# Get BIC
BIC <- sum1$FunctionOutput$BIC
BIC_scaled <- BIC/max(BIC)


## Canvas
K <- 6
plot.new()
ymax <- 1
ymin <- min(c(negLL_scaled, BIC_scaled))

plot.window(xlim=c(1,K), ylim=c(ymin, ymax))
axis(1, 1:K)
axis(2, las=2)
grid()
title(xlab="Number of Clusters", line=2.5)

## Plot Data
shift <- 0 # why add/subtract shift?
# Plot negLL
points((1:K)-shift, negLL_scaled, pch=16, cex=1.25)
lines((1:K)-shift, negLL_scaled, lwd=2)
# Plot BIC
points((1:K)+shift, BIC_scaled, col="tomato", pch=16, cex=1.25)
lines((1:K)+shift, BIC_scaled, col="tomato", lwd=2)

# Legend
legend("topright", legend=c("Negative LL (scaled)", "BIC (scaled)"),
       text.col=c("black", "tomato"), bty="n", cex=1)

length(BIC)


#############################################################################
#                FUNCTION TO CAL AND PLOT SCALED BIC AND NLL                #
#############################################################################

PlotScaled <- function(model, title = NULL) {
  sum <- summary(model, show = "GNL")
  
  # Get negLL
  negLL <- -sum$FunctionOutput$`log-likelihood`
  negLL_scaled <- negLL/max(negLL)
  
  # Get BIC
  BIC <- sum$FunctionOutput$BIC
  BIC_scaled <- BIC/max(BIC)
  
  # canvas
  K <- length(BIC)
  plot.new()
  ymax <- 1
  ymin <- min(c(negLL_scaled, BIC_scaled))
  
  plot.window(xlim=c(1,K), ylim=c(ymin, ymax))
  axis(1, 1:K)
  axis(2, las=2)
  grid()
  title(xlab="Number of Clusters", line=2.5)
  title(title, font.main=1)
  
  # plot
  shift <- 0 # why add/subtract shift?
  
  # negLL
  points((1:K)-shift, negLL_scaled, pch=16, cex=1.25)
  lines((1:K)-shift, negLL_scaled, lwd=2)
  
  # BIC
  points((1:K)+shift, BIC_scaled, col="tomato", pch=16, cex=1.25)
  lines((1:K)+shift, BIC_scaled, col="tomato", lwd=2)
  
  # legend
  legend("topright", legend=c("Negative LL (scaled)", "BIC (scaled)"),
         text.col=c("black", "tomato"), bty="n", cex=1)
}


PlotScaled(out_seed1, title = "Random Seed 1")


#############################################
#                SEED 1 EVAL                #
#############################################
sum1
sum1_IC <- sum1$FunctionOutput$Proportions

classproportions1 <- matrix(NA, 6, 6)
for(i in 1:6) classproportions1[i,1:i] <- as.numeric(unlist(strsplit(sum1_IC[i], " ")))
rownames(classproportions1) <- paste0(1:6, " Clusters")
colnames(classproportions1) <- paste0("Class ", 1:6)
classproportions1 <- as.data.frame(classproportions1)



#################################################################
#                STACKED BAR PLOT FOR PROPORTION                #
#################################################################

library(RColorBrewer)

classproportions1_long <- classproportions1 %>%
  mutate(Cluster = paste(1:nrow(classproportions1), "Clusters")) %>%
  select(Cluster, everything()) %>% 
  pivot_longer(
    cols = starts_with("Class"),
    names_to = "Class",
    values_to = "Proportion",
    values_drop_na = TRUE
  )


# plot
ggplot(classproportions1_long, aes(x = Cluster, y = Proportion, fill = Class)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Seed 1",
       y = "Proportion of person per class",
       x = "",
       fill = "Class") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 12, colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        plot.title = element_text(margin = margin(r = 10), size = 14, colour = "black", hjust = 0.5),
        plot.margin = unit(c(1, 0.5, 0, 0.5), "cm")
  )




# 4.6.4 parameter estimate stuff

# Get coefficient objects for all K
coefs1 <- list()
K <- 6
for(k in 1:K) coefs1[[k]] <- coef.ClusterVAR(out_seed1, Model = rep(1, k))
# 
# ?coef.ClusterVAR
# 
# c2uster <- coef.ClusterVAR(out_seed1, c(1,1))
# temporal1 <- c2uster$VAR_coefficients # -> temporal
# x <- c2uster$Sigma # need to transform into contemporaneous model
# dim(x)
# 
# 
# # transform innovation to contemporaneous effects (partial correlations)
# library(corpcor)
# comtemp1 <- array(NA, dim = dim(c2uster$Sigma))
# 
# for (i in 1:dim(c2uster$Sigma)[3]) {
#   comtemp1[,,i] <- cor2pcor(c2uster$Sigma[,,i])
# }
# 
# dimnames(comtemp1) <- list(c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed"), 
#                            c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed"), NULL)
# 

### 2 cluster model 
c2uster <- coef.ClusterVAR(out_seed1, c(1,1))

# temporal effects
temporal1 <- c2uster$VAR_coefficients 

# innovation to contemporaneous effects (partial correlations)
contemporaneous1 <- array(NA, dim = dim(c2uster$Sigma))
for (i in 1:dim(c2uster$Sigma)[3]) {
  contemporaneous1[,,i] <- cor2pcor(c2uster$Sigma[,,i])
}

var_names <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")
dimnames(contemporaneous1) <- list(var_names, var_names, NULL)

temporal1
contemporaneous1



temporal1

dim(contemporaneous1[[6]])


#### PLOT HEAT

plotHeat <- function(phi,
                     k,
                     main,
                     labels=NULL,
                     las.x=1,
                     cex.axis=0.8,
                     cex.val=0.7,
                     mar = c(2.5,2.5,2.5,2.5),
                     small_difference_to_white = FALSE) {
  
  
  # -- Aux Variables --
  p <- ncol(phi)
  
  # Fill in default labels
  if(is.null(labels)) labels <-  paste0("Y", 1:p)

  
  # -- Make color gradient --
  color.gradient <- function(x, 
                             colors=c("#E41A1C", "white", "#377EB8"), 
                             colsteps=201) {
    return(colorRampPalette(colors) (colsteps) [findInterval(x, 
                                                             seq(min(x), max(x), length.out = colsteps))])
  }
  x <- 1:201
  grad <- color.gradient(x)
  
  # Make canvas
  par(mar=mar)
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(0, 1))
  
  # Auxiliary plotting variables
  sfm <- 1/(p*2)
  seq_mp_x <- seq(0, 1, length=p+1)[-(p+1)] + sfm
  
  # Plot Axes & Axis labels
  labels_tm1 <- sapply(labels, function(label) bquote(.(label)[t-1]), simplify = FALSE)
  labels_t1 <- sapply(labels, function(label) bquote(.(label)[t]), simplify = FALSE)
  axis(1, labels = do.call(expression, labels_tm1), at=seq_mp_x, cex.axis=cex.axis)
  axis(2, labels = do.call(expression, labels_t1[p:1]), at=seq_mp_x, las=2, cex.axis=cex.axis)
  title(main, font.main=1)
  
  # Plot Data
  for(i in 1:p) {
    for(j in 1:p) {
      
      # Get colour
      phi_ij <- phi[p:1, ][j, i]
      
      if (small_difference_to_white && abs(phi_ij) < 0.1) {
        col_ij <- "#FFFFFF"
      } else if (phi_ij < -1) {
        col_ij <- grad[1]
      } else if (phi_ij > 1) {
        col_ij <- grad[201]
      } else {
        col_ij <- grad[phi_ij * 100 + 101]
      }
      
      # Plot box
      rect(xleft = seq_mp_x[i]-sfm,
           ybottom = seq_mp_x[j]-sfm,
           xright = seq_mp_x[i]+sfm,
           ytop = seq_mp_x[j]+sfm,
           col = col_ij)
      
      # Plot text
      text(seq_mp_x[i], seq_mp_x[j], round(phi_ij , 2), cex=cex.val, col="black")
    }
  }
  
  
} # eoF







####### PARAMETERS
# coefficient objects of all models
coefs1 <- list()
K <- 6
for(k in 1:K) coefs1[[k]] <- coef.ClusterVAR(out_seed1, Model = rep(1, k))

# all temporal parameters
temporal1 <- sapply(coefs1, function(sublist) sublist[["VAR_coefficients"]])

# all contemporaneous parameters
innovations1 <- sapply(coefs1, function(sublist) sublist[["Sigma"]])
var_names <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")

contemporaneous1 <- vector("list", length(innovations1))
for (i in seq_along(innovations1)) {
  ar <- innovations1[[i]]
  dims <- dim(ar)
  
  pcor <- array(NA, dim = dims)
  for (j in 1:dims[3]) {
    pcor[,,j] <- cor2pcor(ar[,,j])
  }
  
  dimnames(pcor) <- list(var_names, var_names, NULL)
  contemporaneous1[[i]] <- pcor
}


temporal1

k <- 4

plot(out_seed1, show = "specific",
     Model = rep(1,k),
     labels = var_names,
     mar = c(2.5,3.75,2,0.5))

plot(out_seed1, show = "specificDiff",
     Model = rep(1,k),
     labels = var_names,
     mar = c(2.5,3.75,2,0.5))

# what i want
# 11  12  13  14
#     22  23  24
#         33  34
#             44

# so plot 11223344 first
# and then plot new on the others?



# steal model specific code
# steal specific diff code






coefs <- coef(out_seed1, Model = c(1,1))

###### SPECIFIC
K <- dims_phi[3]
# browser()

l_phi <- list()
for(k in 1:K) l_phi[[k]] <-  l_coef$VAR_coefficients[, , k]

# Decide on layout
if(K == 1) graphics::par(mfrow=c(1,1))
if(K == 2) graphics::par(mfrow=c(1,2))
if(K %in% 3:4) graphics::par(mfrow=c(2,2))
if(K %in% 5:9) graphics::par(mfrow=c(3,3))
if(K > 10) {
  ldim <- ceiling(sqrt(K))
  graphics::par(mfrow=c(ldim,ldim))
}

# Loop over clusters & plot
mar_heat = c(2.5,2.5,2,1)
cex.axis <- 0.8
cex.val <- 0.7
labels <- NULL

for(k in 1:K) plotHeat(phi = l_phi[[k]],
                       k = k,
                       main = paste0("Cluster ", k),
                       labels = labels,
                       cex.axis = cex.axis,
                       cex.val = cex.val, mar = mar_heat)






##### SPECIFIC DIFF

# Get parameters
# same as above
dims_phi <- dim(l_coef$VAR_coefficients)
K <- 4 #dims_phi[3]
l_phi <- list()
for(k in 1:K) l_phi[[k]] <-  l_coef$VAR_coefficients[, , k]


# Setup Layout matrix
lmat <- matrix((2*(K-1)+1):((2*(K-1)) + (K-1)^2), K-1, K-1, byrow=TRUE)
lmat <- rbind(1:(K-1), lmat)
lmat <- cbind(c(0, K:(2*(K-1))), lmat)
layout(mat=lmat,
       widths = c(0.2, rep(1, K-1)),
       heights =  c(0.2, rep(1, K-1)))
layout.show(15)

# Plot Labels
plotLabel <- function(x, srt=0, col="black",
                      xpos=.6, ypos=.6, cex=1.4) {
  par(mar=rep(0, 4))
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(0,1))
  text(xpos, ypos, x, srt=srt, cex=cex, col=col)
}

for(k in 2:K) plotLabel(paste0("Cluster ", k), cex=1.5)
for(k in 1:(K-1)) plotLabel(paste0("Cluster ", k), cex=1.5, srt=90)

# Plot Data
for(k1 in 1:(K-1)) {
  for(k2 in 2:K) {
    
    if(k1==k2) {
      NULL
    } 
    
    else {
      phi_diff <- l_phi[[k1]] - l_phi[[k2]]
      par(mar=c(2.5,2.5,2,1))
      plotHeat(phi = phi_diff, k = k, main = paste0("Difference: Cluster ", k1, " - Cluster ", k2))
    }
    
    
  }
} # end for Ks


##### AUTOCORRELATION AND DIFFERENCES BETWEEN CLUSTERS

# get parameters
dims <- dim(l_coef$VAR_coefficients)
K <- dims[3]
l_phi <- list()
for(k in 1:K) l_phi[[k]] <-  l_coef$VAR_coefficients[, , k]


# set layout
lmat <- matrix((K*2+1):((K*2)+K^2), K, K, byrow=TRUE)
lmat <- rbind(1:K, lmat)
lmat <- cbind(c(0, (K+1):(K*2)), lmat)

layout(mat=lmat,
       widths = c(0.2, rep(1, K)),
       heights =  c(0.2, rep(1, K)))
layout.show(24)



############################################################
x <- out_seed1
nClusters <- 4

coefs <- coef(x, Model = rep(1,nClusters))$VAR_coefficients
K
SETlayout(4)
?layout

#### FUNCTION

PlotCOEFS <- function(x, nClusters = NULL,
                      mar_heat = c(2.5,1.5,1.5,1.5), 
                      cex.axis = 0.8, 
                      cex.val = 0.5, 
                      labels = NULL,
                      legend = TRUE,
                      variable_names = var_names,
                      small_difference_to_white = FALSE) {
  
  # ----- get coefficients -----
  coefs <- coef(x, Model = rep(1,nClusters))$VAR_coefficients
  dims <- dim(coefs)
  K <- dim[3]
  
  l_phi <- list() # put coef into list form
  for(i in 1:K) l_phi[[i]] <- coefs[, , i] 
  
  
  # ----- set layout  -----
  graphics::layout(mat=SETlayout(K),
                   widths = c(0.2, rep(1, K)),
                   heights =  c(0.2, rep(1, K)))
  
  
  # ----- plot labels -----
  plotLabel <- function(x, srt=0, col="black",
                        xpos=.5, ypos=.5, cex=1.5) {
    par(mar=rep(0, 4))
    plot.new()
    plot.window(xlim=c(0,1), ylim=c(0,1))
    text(xpos, ypos, x, srt=srt, cex=cex, col=col)
  }
  
  for(i in 1:K) plotLabel(paste0("Cluster ", i), cex=1.5)
  for(i in 1:K) plotLabel(paste0("Cluster ", i), cex=1.5, srt=90)
  
  
  
  # ----- plot autocorrelations -----
  for(i in 1:K) plotHeat(phi = l_phi[[i]],
                         k = i,
                         main = paste0("Cluster ", i),
                         cex.axis = cex.axis,
                         cex.val = cex.val, 
                         mar = mar_heat,
                         labels = labels)
  
  
  # ----- plot coef differences -----
  for(k1 in 1:(K-1)) {
    for(k2 in 2:K) {
      
      if(k1>=k2) {
        NULL
      } else {
        phi_diff <- l_phi[[k1]] - l_phi[[k2]]
        plotHeat(phi = phi_diff, 
                 k = K, 
                 main = paste0("Difference: Cluster ", k1, " - Cluster ", k2),
                 cex.axis = cex.axis,
                 cex.val = cex.val, 
                 mar = mar_heat,
                 labels = labels,
                 small_difference_to_white = small_difference_to_white)
      }
      
    }
  } # end of plot differences
  
  
  # ----- legend -----
  if (legend && !is.null(var_names)) {
    par(mar=rep(1, 4))
    plot.new()
    plot.window(xlim=c(0,1), ylim=c(0,1))
    text(0, .95, "Variables:", adj = c(0,1), font = 2, cex = 1.3)
    for (i in seq_along(var_names)) {
      text(0, 0.88 - i * 0.11, paste0("Y", i, " = ", var_names[i]), adj = 0, cex = 1.3)
    }
  }
  
  
  
}

PlotCOEFS(out_seed1, 2, small_difference_to_white = TRUE)


# Lowkey i should add the mixing proportions
##################################################



for(k1 in 1:(K-1)) {
  for(k2 in 2:K) {
    
    if(k1>=k2) {
      NULL
    } 
    
    else {
      phi_diff <- l_phi[[k1]] - l_phi[[k2]]
      par(mar=c(2.5,2.5,2,1))
      plotHeat(phi = phi_diff, k = k, main = paste0("Difference: Cluster ", k1, " - Cluster ", k2))
    }
    
    
  }
} 


k <- 4

plot(out_seed1, show = "specificDiff",
     Model = rep(1,k),
     labels = var_names,
     mar = c(2.5,3.75,2,0.5))

nClusters = 4
Model = rep(1, nClusters)

coefs <- coef(out_seed1, Model = rep(1,nClusters))$VAR_coefficients




for (k1 in 1:(K-1)) {
  for (k2 in 2:K) {
    
    if(k1 == k2) {
      graphics::plot.new()
      graphics::plot.window(xlim=c(0,1), ylim=c(0,1))
    } 
    
    else {
      phi_diff <- l_phi[[k1]] - l_phi[[k2]]
      par(mar=c(2.5,2.5,2,1))
      plotHeat(phi = phi_diff, k = k, main = paste0("Difference: Cluster ", k1, " - Cluster ", k2))
    }
    
  }
} 

# if x == y -> plot auto correlation
# 

source("/Users/Lexi/Desktop/internship/2_ code/Lexi_Helpers.R")


# SETlayout <- function(K) {
#   stopifnot(K >= 2)
#   
#   inner_block <- matrix(NA, K, K)
#   values <- (2 * K + 1):(2 * K + K^2)
#   i <- 1
#   
#   # fill diagonal
#   for (k in 1:K) {
#     inner_block[k, k] <- values[i]
#     i <- i + 1
#   }
#   
#   # fill upper triangle
#   for (r in 1:(K - 1)) {
#     for (c in (r + 1):K) {
#       inner_block[r, c] <- values[i]
#       i <- i + 1
#     }
#   }
#   
#   # fill lower triangle
#   for (r in 2:K) {
#     for (c in 1:(r - 1)) {
#       inner_block[r, c] <- values[i]
#       i <- i + 1
#     }
#   }
#   
#   lmat <- rbind(0:K, cbind((K + 1):(2 * K), inner_block))
# }
# 
# 

# K =4
# layout(mat=SETlayout(K),
#        widths = c(0.2, rep(1, K)),
#        heights =  c(0.2, rep(1, K)))
# layout.show(8)

# combine pdfs
# Load the qpdf package
library(qpdf)

# Define the directory containing the PDFs
pdf_dir <- "/Users/Lexi/Desktop/internship/4_ plots/"

# Define the file names
pdf_files <- file.path(pdf_dir, paste0("8", letters[1:5], "_ cluster coefs.pdf"))

# Define output file path
output_file <- file.path(pdf_dir, "8_ cluster coefs.pdf")

# Combine the PDFs
pdf_combine(input = pdf_files, output = output_file)




### mixing proportion plot

####################################################################
#                8. SEED 1 STACKED PROPORTION PLOT                 #
####################################################################
# calculate proportions
sum1 <- summary(out_seed1, show = "GNL")
sum1_IC <- sum1$FunctionOutput$Proportions

clasum1_ICclassproportions1 <- matrix(NA, 6, 6)
for(i in 1:6) classproportions1[i,1:i] <- as.numeric(unlist(strsplit(sum1_IC[i], " ")))
rownames(classproportions1) <- paste0(1:6, " Clusters")
colnames(classproportions1) <- paste0("Class ", 1:6)
classproportions1 <- as.data.frame(classproportions1)


# transform to long form
classproportions1_long <- classproportions1 %>%
  mutate(Cluster = paste(1:nrow(classproportions1), "Clusters")) %>%
  select(Cluster, everything()) %>% 
  pivot_longer(
    cols = starts_with("Class"),
    names_to = "Class",
    values_to = "Proportion",
    values_drop_na = TRUE
  )


# plot
ggplot(classproportions1_long, aes(x = Cluster, y = Proportion, fill = Class)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Seed 1",
       y = "Proportion of person per class",
       x = "",
       fill = "Class") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 12, colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        plot.title = element_text(margin = margin(r = 10), size = 14, colour = "black", hjust = 0.5),
        plot.margin = unit(c(1, 0.5, 0, 0.5), "cm")
  )


library(plotly)
dat <- as.data.frame(matrix(rnorm(150*10), ncol=10))
names(dat) <- LETTERS[1:10]

xvalues <- apply(dat, 2, function(y) density(y)$x)
xmin <- min(xvalues)
xmax <- max(xvalues)
yvalues <- c(apply(dat, 2, function(a) density(a, from=xmin, to=xmax)$y))

dd <- cbind(c(yvalues), xvalues=c(xvalues), a = colnames(dat))

plot_ly(data.frame(dd), x = ~xvalues, y = ~a, z = ~yvalues, split = ~a, 
        type = "scatter3d", mode = "lines") 




################################################################################
##### TRIAL TIME SERIES PLOT #####
##################################

# extract classifications
class1_4clusters <- t(out_seed1$All_Models[[4]][[1]][[77]]$Classification)
class1_4clusters <- data.frame(id = as.numeric(rownames(class1_4clusters)),
                               classification = as.integer(class1_4clusters[, 1]))

# extract cluster 1
cluster1_s1_4c_id <- class1_4clusters[class1_4clusters$classification == 1,]

cluster1_4c <- affect[affect$PID %in% cluster1_s1_4c_id$id, ]


# collapsed plot
ggplot(cluster1_relaxed_4, aes(x = OCCASION, y = Relaxed, group = PID, colour = PID)) +
  geom_line() +
  labs(x = 'Time Point', y = 'Relaxed', title = 'Time series of Relaxed in Cluster 1') +
  theme_minimal() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm")  # top, right, bottom, left
  )


### happy 
# plot relaxed
cluster1_happy_4 <- cluster1_4c %>% 
  select(PID, OCCASION, Happy) %>%
  mutate(
    PID = as.character(PID),
    PID = paste0("ID", PID))

plot_ly(cluster1_happy_4, x = ~PID, y = ~OCCASION, z = ~Happy, split = ~PID, 
        line = list(width = 5),
        type = "scatter3d", mode = "lines") %>% 
  layout(scene = list(aspectmode = "manual", 
                      aspectratio = list(x=6,y=2,z=0.4)))

# collapsed plot
ggplot(cluster1_happy_4, aes(x = OCCASION, y = Happy, group = PID, colour = PID)) +
  geom_line() +
  labs(x = 'Time Point', y = 'Relaxed', title = 'Time series of Happy in Cluster 1') +
  theme_minimal() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm")  # top, right, bottom, left
  )


# make 3d plot into a function

# make time series plot into a function

# plot all 7 affect variable of each cluster into one pdf

### I'm gonna come back to this, I'm taking space ###

##########################################################################################################

######################################################
#               TRIAL TIME SERIES PLOT               #
######################################################
library(RColorBrewer)

PlotCOEFS(out_seed1, nClusters = 3)




# sidebar code
sidebar <- data.frame(
  Class = LETTERS[1:nClusters],
  Proportion = as.numeric(unlist(strsplit(sum1_IC[nClusters], " "))))

sidebar$location <- 1 - (cumsum(sidebar$Proportion) - sidebar$Proportion/2)

# Plot
ggplot(sidebar, aes(x = 1, y = Proportion, fill = factor(Class, levels = c("A", "B", "C")))) +
  geom_bar(stat = 'identity', width = 1) +
  geom_text(aes(y = location, label = Proportion), x = 1, colour = 'white', size = 5) +
  theme_void() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "none"
  )



PlotCOEFS <- function(x, nClusters = NULL,
                      sidebar = TRUE,
                      mar_heat = c(2.5,1.5,1.5,1.5), 
                      cex.axis = 0.8, 
                      cex.val = 0.5, 
                      labels = NULL,
                      legend = TRUE,
                      variable_names = var_names,
                      small_difference_to_white = FALSE) {
  
  # ----- get coefficients -----
  coefs <- coef(x, Model = rep(1, nClusters))$VAR_coefficients
  dims <- dim(coefs)
  K <- dims[3]
  
  l_phi <- list() # put coef into list form
  for(i in 1:K) l_phi[[i]] <- coefs[, , i] 
  
  
  # ----- set layout  -----
  lmat <- SETlayout(K)
  lmat <- cbind(max(lmat)+1, lmat)
  
  graphics::layout(mat=lmat,
                   widths = c(0.2, 0.2, rep(1, K)),
                   heights =  c(0.2, rep(1, K)))
  
  # ----- plot labels -----
  plotLabel <- function(x, srt=0, col="black",
                        xpos=.5, ypos=.5, cex=1.5) {
    par(mar=rep(0, 4))
    plot.new()
    plot.window(xlim=c(0,1), ylim=c(0,1))
    text(xpos, ypos, x, srt=srt, cex=cex, col=col)
  }
  
  for(i in 1:K) plotLabel(paste0("Cluster ", i), cex=1.5)
  for(i in 1:K) plotLabel(paste0("Cluster ", i), cex=1.5, srt=90)
  
  
  
  # ----- plot autocorrelations -----
  for(i in 1:K) PlotHEAT(phi = l_phi[[i]],
                         k = i,
                         main = paste0("Cluster ", i),
                         cex.axis = cex.axis,
                         cex.val = cex.val, 
                         mar = mar_heat,
                         labels = labels)
  
  
  # ----- plot coef differences -----
  for(k1 in 1:(K-1)) {
    for(k2 in 2:K) {
      
      if(k1>=k2) {
        NULL
      } else {
        phi_diff <- l_phi[[k1]] - l_phi[[k2]]
        PlotHEAT(phi = phi_diff, 
                 k = K, 
                 main = paste0("Difference: Cluster ", k1, " - Cluster ", k2),
                 cex.axis = cex.axis,
                 cex.val = cex.val, 
                 mar = mar_heat,
                 labels = labels,
                 small_difference_to_white = small_difference_to_white)
      }
      
    }
  } # end of plot differences
  
  
  # ----- legend -----
  if (legend && !is.null(var_names)) {
    par(mar=rep(1, 4))
    plot.new()
    plot.window(xlim=c(0,1), ylim=c(0,1))
    text(0, .95, "Variables:", adj = c(0,1), font = 2, cex = 1.3)
    for (i in seq_along(var_names)) {
      text(0, 0.88 - i * 0.11, paste0("Y", i, " = ", var_names[i]), adj = 0, cex = 1.3)
    }
  } 
  # ----- sidebar -----
  if (sidebar) {
    sidebar <- data.frame(
      Class = LETTERS[1:nClusters],
      Proportion = as.numeric(unlist(strsplit(sum1_IC[nClusters], " ")))
    )
    sidebar$location <- 1 - (cumsum(sidebar$Proportion) - sidebar$Proportion / 2)
    sidebar$label <- paste0('C', 1:K, '\n', sidebar$Proportion)
    
    side <- ggplot(sidebar, aes(x = 1, y = Proportion, fill = Class)) +
      geom_bar(stat = 'identity', width = 1) +
      geom_text(aes(y = location, label = label), x = 1, colour = 'black', size = 8/K) +
      theme_void() +
      # ggtitle('mixing\nproportions') +
      scale_fill_brewer(palette = "Pastel1") +
      theme(
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none"
        # plot.title = element_text(hjust = 0.5, vjust = -3, size = 9)
      )
    
    vp <- viewport(
      x = 0.2 / (0.2 + 0.2 + 1 * K), y = 0.5 - (0.05 / K),
      width = (0.1 / K), height = 1,
      just = c("right", "center")
    )
    
    plot.new()
    print(side, newpage = FALSE, vp)
  }
  
  
} # eof

PlotCOEFS(out_seed1, nClusters = 4)
library(grid)
1/4.2
?viewport
6/2
0.05/4
(1/(0.2+K*1))*K

graphics::layout(mat=SETlayout(K),
                 widths = c(0.2, rep(1, K), 0.2),
                 heights =  c(0.2, rep(1, K)))


lmat
layout.show(16)



SETlayout <- function(K) {
  stopifnot(K >= 2)
  
  inner_block <- matrix(NA, K, K)
  values <- (2 * K + 1):(2 * K + K^2)
  i <- 1
  
  # fill diagonal
  for (k in 1:K) {
    inner_block[k, k] <- values[i]
    i <- i + 1
  }
  
  # fill upper triangle
  for (r in 1:(K - 1)) {
    for (c in (r + 1):K) {
      inner_block[r, c] <- values[i]
      i <- i + 1
    }
  }
  
  # fill lower triangle
  for (r in 2:K) {
    for (c in 1:(r - 1)) {
      inner_block[r, c] <- values[i]
      i <- i + 1
    }
  }
  
  lmat <- rbind(0:K, cbind((K + 1):(2 * K), inner_block))
}




lmat <- bind(max(SETlayout(K))+1, SETlayout(K))

lmat <- SETlayout(K)
lmat <- cbind(lmat[, 1], max(lmat)+1, lmat[, 2:ncol(lmat)])

rm(PlotCOEFS)

   



################################################################################
##### FUNCTION TIME SERIES PLOT #####
#####################################
library(dplyr)
library(plotly)

NAHPlot3D <- function(model, 
                   nClusters, 
                   clusterToPlot, 
                   data, 
                   PID, 
                   timepoints, 
                   variable, 
                   showN = FALSE) {
  
  # ----- extract classification of best model -----
  # change here
  class_mat <- t(model$All_Models[[nClusters]][[1]][[77]]$Classification)
  
  # Create a dataframe of ID and classification label
  class_df <- data.frame(
    id = as.numeric(rownames(class_mat)),
    classification = as.integer(class_mat[, 1])
  )
  
  # Extract IDs in the chosen cluster
  cluster_ids <- class_df[class_df$classification == clusterToPlot, "id"]
  
  # Optionally print number of participants
  if (showN) {
    message("Number of participants classified into Cluster ", clusterToPlot, ": ", length(cluster_ids))
  }
  
  # Filter original data for those IDs
  cluster_data <- data[data[[PID]] %in% cluster_ids, ]
  
  # Prepare data for plotting
  plot_df <- cluster_data %>%
    select(all_of(c(PID, timepoints, variable))) %>%
    mutate(
      !!PID := as.character(.data[[PID]]),
      !!PID := paste0("ID", .data[[PID]])
    )
  
  # Create 3D line plot
  plot_ly(
    plot_df, 
    x = as.formula(paste0("~", PID)), 
    y = as.formula(paste0("~", timepoints)), 
    z = as.formula(paste0("~", variable)), 
    split = as.formula(paste0("~", PID)),
    type = "scatter3d",
    mode = "lines",
    line = list(width = 5)
  ) %>% 
    layout(scene = list(
      xaxis = list(title = PID),
      yaxis = list(title = timepoints),
      zaxis = list(title = variable),
      aspectmode = "manual",
      aspectratio = list(x = 6, y = 2, z = 0.4)
    ))
}


c4 <- out_seed1$All_Models[[4]][[1]][[77]]$Classification
table(c4)

Plot3D(
  model = out_seed1, 
  nClusters = 2, 
  clusterToPlot = 1,
  data = affect, 
  PID = "PID", 
  timepoints = "OCCASION", 
  variable = "Depressed",
  showN = TRUE
)
var_names





###### LCVAR SUMMARY TO GET BEST FIT MODEL ####

summary.ClusterVAR <- function(object, show = "BPC",  TS_criterion = "SC",
                               global_criterion = "BIC",
                               Number_of_Clusters = NULL,
                               Number_of_Lags = NULL,
                               ...) {
  
  # Fill in defaults
  # args <- list(...)
  # if(is.null(args$show)) show <- "BPC" else show <- args$show
  # if(is.null(args$TS_criterion)) TS_criterion <- "SC" else TS_criterion <- args$TS_criterion
  # if(is.null(args$global_criterion)) global_criterion <- "BIC" else global_criterion <- args$global_criterion
  # if(is.null(args$Number_of_Clusters)) Number_of_Clusters <- NULL else Number_of_Clusters <- args$Number_of_Clusters
  # if(is.null(args$Number_of_Lags)) Number_of_Lags <- min(object$Call$Lags) else Number_of_Lags <- args$Number_of_Lags
  if(is.null(Number_of_Lags)) Number_of_Lags <- min(object$Call$Lags)
  
  #TS_criterion = c("SC", "HQ")
  #Global_criterion = c("BIC", "ICL")
  # Number_of_Clusters is only used if show == "GNC"
  
  # -------------Check all input is as expected---------------
  if (!(show %in% c("BPC", "GNL", "GNC"))) {
    stop("Invalid value for 'show'. Please choose from: ", paste(c("BPC", "GNL", "GNC"), collapse = ", "))
  }
  if (!(TS_criterion %in% c("SC", "HQ"))) {
    stop("Invalid value for 'TS_criterion'. Please choose from: ", paste(c("SC", "HQ"), collapse = ", "))
  }
  if (!(global_criterion %in% c("BIC", "ICL"))) {
    stop("Invalid value for 'global_criterion'. Please choose from: ", paste(c("BIC", "ICL"), collapse = ", "))
  }
  if (show == "GNC" && is.null(Number_of_Clusters)) {
    stop("If 'show' is 'GNC', you must specify a value for 'Number_of_Clusters'.")
  }
  if(show == "GNC" && !(Number_of_Clusters %in% object$Call$Clusters)){
    stop("The value you specified for 'Number_of_Clusters' is not contained in the Cluster sequence of your fitted LCVAR Model Object.")
  }
  if(show == "GNL" && !(Number_of_Lags %in% object$Call$Lags)){
    stop("The value you specified for 'Number_of_Lags' is not contained in the Lag sequence of your fitted LCVAR Model Object.")
  }
  # -------------
  
  l_LagsPerCluster <- list()
  for(K in object$Call$Clusters)  l_LagsPerCluster[[K]] <- calculateLagList(K = K, HighestLag = max(object$Call$Lags), LowestLag = min(object$Call$Lags))
  NumberStarts = (object$Call$Rand + as.numeric(object$Call$Rational) + as.numeric(!is.null(object$Call$Initialization)) + 1)
  # last element is for PreviousSol which is TRUE as a default
  
  # -------------Create one of the three different types of FunctionOutput---------------
  
  if(show == "GNC"){
    
    LagCombinations = nrow(l_LagsPerCluster[[Number_of_Clusters]])
    FunctionOutput = data.frame(matrix(NA, nrow = LagCombinations, ncol = 7),
                                row.names = apply(l_LagsPerCluster[[Number_of_Clusters]], 1, function(x) paste(c("Lags", x), collapse = " ")))
    colnames(FunctionOutput) = c("log-likelihood", "parameters", "Lags", "SC", "HQ", "Converged", "Proportions")
    
    
    for(LagCounter in 1:LagCombinations) {
      BestModel = coef(object, Model = l_LagsPerCluster[[Number_of_Clusters]][LagCounter, ])  # Best model for this LagCounter across all starts (based on likelihood)
      ExtractedLags = BestModel$Lags
      OrderedLags = ExtractedLags[order(ExtractedLags)]
      OrderedProportions = BestModel$Proportions[order(ExtractedLags)]
      FunctionOutput[LagCounter, "log-likelihood"] = BestModel$last.loglik
      FunctionOutput[LagCounter, "parameters"] = BestModel$nParameters
      FunctionOutput[LagCounter, "Lags"] = paste(OrderedLags, collapse = " ")
      FunctionOutput[LagCounter, "SC"] = BestModel$SC
      FunctionOutput[LagCounter, "HQ"] = BestModel$HQ
      FunctionOutput[LagCounter, "Converged"] = BestModel$Converged
      FunctionOutput[LagCounter, "Proportions"] = paste(round(OrderedProportions, 2), collapse = " ")
    }
    BestOverall = switch(TS_criterion,
                         "SC" = which.min(FunctionOutput$SC),
                         "HQ" = which.min(FunctionOutput$HQ))
    message = paste0(c("---------------------------------------------------\n",
                       "All lags for number of clusters =", Number_of_Clusters,
                       "\n",
                       "For this number of clusters the", TS_criterion,"selects:", row.names(FunctionOutput)[BestOverall],
                       "\n---------------------------------------------------\n"))
    
  }
  if(show == "BPC"){
    # The below calculates the solution for (show == "BPC")
    # For each number of clusters, find the single best-fitting time-series model for each cluster number
    
    FunctionOutput = data.frame(matrix(NA, nrow = length(object$Call$Clusters), ncol = 7),
                                row.names = apply(as.matrix(object$Call$Clusters), 1, function(x) paste(c(x, "Clusters"), collapse = " ")))
    colnames(FunctionOutput) = c(paste(c("Lags selected by", TS_criterion), collapse = " "),
                                 "log-likelihood", "parameters", "BIC", "ICL", "Converged", "Proportions")
    
    ClustCount = 1
    for(K in object$Call$Clusters){
      
      LagCombinations = nrow(l_LagsPerCluster[[K]])
      FitAllLags = array(NA, dim = c(LagCombinations, 2)) # to store fit of output
      FitStartsWithinLag = array(NA, dim = c(NumberStarts))
      
      for(LagCounter in 1:LagCombinations) {
        for(StartCounter in 1:NumberStarts){
          FitStartsWithinLag[StartCounter] = object$All_Models[[ClustCount]][[LagCounter]][[StartCounter]]$last.loglik
        }
        FitAllLags[LagCounter, 1] = which.max(FitStartsWithinLag)[1] # determines the best start for each lag based on likelihood
        FitAllLags[LagCounter, 2] = switch(TS_criterion,
                                           "SC" = object$All_Models[[ClustCount]][[LagCounter]][[FitAllLags[LagCounter, 1]]]$SC,
                                           "HQ" = object$All_Models[[ClustCount]][[LagCounter]][[FitAllLags[LagCounter, 1]]]$HQ)
      }
      BestRunOneK = which.min(FitAllLags[, 2])[1]
      BPCFinalModel = object$All_Models[[ClustCount]][[BestRunOneK]][[FitAllLags[BestRunOneK, 1]]]
      ExtractedLags = BPCFinalModel$Lags
      OrderedLags = ExtractedLags[order(ExtractedLags)]
      OrderedProportions = BPCFinalModel$Proportions[order(ExtractedLags)]
      FunctionOutput[ClustCount, 1] = paste(OrderedLags, collapse = " ")
      FunctionOutput[ClustCount, "log-likelihood"] = BPCFinalModel$last.loglik
      FunctionOutput[ClustCount, "parameters"] = BPCFinalModel$nParameters
      FunctionOutput[ClustCount, "BIC"] = BPCFinalModel$BIC
      FunctionOutput[ClustCount, "ICL"] = BPCFinalModel$ICL
      FunctionOutput[ClustCount, "Converged"] = BPCFinalModel$Converged
      FunctionOutput[ClustCount, "Proportions"] = paste(round(OrderedProportions, 2), collapse = " ")
      
      ClustCount = ClustCount + 1
      
    }
    
    # remove BIC and ICL from this output, because models with unequal number of lags cannot be compared to one another
    FunctionOutput = FunctionOutput[, -c(2, 4, 5)] # remove liklike, BIC and ICL because they are not comparable for all models
    message = paste0(c("---------------------------------------------------\n",
                       "The best lags for each number of clusters as selected by the", TS_criterion,
                       "\n---------------------------------------------------\n"))
    # FunctionOutput stays as calculated above
  }
  if (show == "GNL"){
    
    FunctionOutput = data.frame(matrix(NA, nrow = length(object$Call$Clusters), ncol = 7),
                                row.names = apply(as.matrix(object$Call$Clusters), 1, function(x) paste(c(x, "Clusters"), collapse = " ")))
    colnames(FunctionOutput) = c(paste(c("Lags"), collapse = " "),
                                 "log-likelihood", "parameters", "BIC", "ICL", "Converged", "Proportions")
    
    ClustCount = 1
    for(K in object$Call$Clusters){
      
      LagCounterresult <- apply(l_LagsPerCluster[[K]], 1, function(row) all(row == Number_of_Lags))
      LagCounter <- which(LagCounterresult)
      
      FitAllLags = array(NA, dim = c(NumberStarts)) # to store fit of output
      
      for(StartCounter in 1:NumberStarts){
        FitAllLags[StartCounter] = object$All_Models[[ClustCount]][[LagCounter]][[StartCounter]]$last.loglik
      }
      BestRunOneK = which.max(FitAllLags)[1]
      ExtractedLags = object$All_Models[[ClustCount]][[LagCounter]][[BestRunOneK]]$Lags # Don't need to be ordered because they are all ordered by default of being all the same lag
      FunctionOutput[ClustCount, 1] = paste(ExtractedLags, collapse = " ")
      FunctionOutput[ClustCount, "log-likelihood"] = object$All_Models[[ClustCount]][[LagCounter]][[BestRunOneK]]$last.loglik
      FunctionOutput[ClustCount, "parameters"] = object$All_Models[[ClustCount]][[LagCounter]][[BestRunOneK]]$nParameters
      FunctionOutput[ClustCount, "BIC"] = object$All_Models[[ClustCount]][[LagCounter]][[BestRunOneK]]$BIC
      FunctionOutput[ClustCount, "ICL"] = object$All_Models[[ClustCount]][[LagCounter]][[BestRunOneK]]$ICL
      FunctionOutput[ClustCount, "Converged"] = object$All_Models[[ClustCount]][[LagCounter]][[BestRunOneK]]$Converged
      FunctionOutput[ClustCount, "Proportions"] = paste(round(object$All_Models[[ClustCount]][[LagCounter]][[BestRunOneK]]$Proportions, 2), collapse = " ")
      
      ClustCount = ClustCount + 1
    }
    
    BestOverall = switch(global_criterion,
                         "BIC" = which.min(FunctionOutput$BIC),
                         "ICL" = which.min(FunctionOutput$ICL))
    message = paste0(c("---------------------------------------------------\n",
                       "All models where all lags =", Number_of_Lags,
                       "\n",
                       "For this number of lags the", global_criterion,"selects:", row.names(FunctionOutput)[BestOverall],
                       "\n---------------------------------------------------\n"))
    
  }
  #### Return FunctionOutput here ##
  FunctionOutput = list(message = message, FunctionOutput = FunctionOutput)
  class(FunctionOutput) <- c("ClusterVARSummary", class(FunctionOutput))
  
  print(BestRunOneK)
  return(FunctionOutput)
  
  
  
} # eoF



##### PROBLEM OF WHICH MODEL TO PICK #####

FitAllLags = array(NA, dim = c(NumberStarts)) # to store fit of output

for(StartCounter in 1:NumberStarts){
  FitAllLags[StartCounter] = object$All_Models[[ClustCount]][[LagCounter]][[StartCounter]]$last.loglik
}
BestRunOneK = which.max(FitAllLags)[1]





#### 
getBestModel <- function(object) {
  Number_of_Lags <- 1
  
  NumberStarts <- object$Call$Rand +
    as.numeric(object$Call$Rational) +
    as.numeric(!is.null(object$Call$Initialization)) + 1
  
  l_LagsPerCluster <- list()
  for (K in object$Call$Clusters) {
    l_LagsPerCluster[[K]] <- calculateLagList(
      K = K,
      HighestLag = max(object$Call$Lags),
      LowestLag = min(object$Call$Lags)
    )
  }
  
  BestRunOneK <- list()
  
  for (ClustCount in seq_along(object$Call$Clusters)) {
    K <- object$Call$Clusters[ClustCount]
    
    LagCounter <- which(
      apply(l_LagsPerCluster[[K]], 1, function(row) all(row == Number_of_Lags))
    )
    
    if (length(LagCounter) == 0) {
      stop(paste("Lag 1 not found for K =", K))
    }
    
    FitAllLags <- numeric(NumberStarts)
    for (StartCounter in 1:NumberStarts) {
      FitAllLags[StartCounter] <- object$All_Models[[ClustCount]][[LagCounter]][[StartCounter]]$last.loglik
    }
    
    BestRunOneK[[paste0("K", K)]] <- which.max(FitAllLags)[1]
  }
  
  return(BestRunOneK)
}


x <- getBestModel(out_seed1)



####### 

Plot3D <- function(model, 
                   nClusters, 
                   clusterToPlot, 
                   data, 
                   PID, 
                   timepoints, 
                   variable, 
                   showN = FALSE) {
  
  # ----- extract classification of best model -----
  best_runs <- getBestModel(model)
  best_run <- best_runs[[paste0("K", nClusters)]]
  message("The best model is ", best_run)

  class_mat <- t(model$All_Models[[nClusters]][[1]][[best_run]]$Classification)
  
  # Create a dataframe of ID and classification label
  class_df <- data.frame(
    id = as.numeric(rownames(class_mat)),
    classification = as.integer(class_mat[, 1])
  )
  
  # Extract IDs in the chosen cluster
  cluster_ids <- class_df[class_df$classification == clusterToPlot, "id"]
  
  # Optionally print number of participants
  if (showN) {
    message("Number of participants classified into Cluster ", clusterToPlot, ": ", length(cluster_ids))
  }
  
  # Filter original data for those IDs
  cluster_data <- data[data[[PID]] %in% cluster_ids, ]
  
  # Prepare data for plotting
  plot_df <- cluster_data %>%
    select(all_of(c(PID, timepoints, variable))) %>%
    mutate(
      !!PID := as.character(.data[[PID]]),
      !!PID := paste0("ID", .data[[PID]])
    )
  
  # Create 3D line plot
  plot_ly(
    plot_df, 
    x = as.formula(paste0("~", PID)), 
    y = as.formula(paste0("~", timepoints)), 
    z = as.formula(paste0("~", variable)), 
    split = as.formula(paste0("~", PID)),
    type = "scatter3d",
    mode = "lines",
    line = list(width = 5)
  ) %>% 
    layout(scene = list(
      xaxis = list(title = PID),
      yaxis = list(title = timepoints),
      zaxis = list(title = variable),
      aspectmode = "manual",
      aspectratio = list(x = 6, y = 2, z = 0.4)
    ))
}


Plot3D(
  model = out_seed1, 
  nClusters = 3, 
  clusterToPlot = 1,
  data = affect, 
  PID = "PID", 
  timepoints = "OCCASION", 
  variable = "Depressed",
  showN = TRUE
)

getBestModel(out_seed1)
rm(getBestModel)




#### COLLAPSED TIME SERIES FUNCTION ####

PlotCOLLAPSED <- function(model, 
                          nClusters, 
                          clusterToPlot, 
                          data, 
                          PID, 
                          timepoints, 
                          variable, 
                          showN = FALSE) {
  
  # ----- extract classification of best model -----
  best_runs <- getBestModel(model)
  best_run <- best_runs[paste0("K", nClusters)]
  
  class_mat <- t(model$All_Models[[nClusters]][[1]][[best_run]]$Classification)
  
  
  # ----- df of id and classification -----
  class_df <- data.frame(id = as.numeric(rownames(class_mat)),
    classification = as.integer(class_mat[, 1]))
  
  
  # ----- extract id and get timeseries data of those ids -----
  
  cluster_ids <- class_df[class_df$classification == clusterToPlot, "id"]
  cluster_data <- data[data[[PID]] %in% cluster_ids, ]
  
  if (showN) {message("Number of participants classified into Cluster ", clusterToPlot, ": ", length(cluster_ids))}
  
  
  # ----- plot -----
  ggplot(cluster_data, aes_string(x = timepoints, 
                                  y = variable, 
                                  group = paste0("factor(", PID, ")"), 
                                  colour = paste0("factor(", PID, ")"))) +
    geom_line() +
    labs(x = timepoints,
         y = variable,
         title = paste0('Time series of ', variable, ' in Cluster ', clusterToPlot)) +
    scale_colour_discrete(name = 'Participant ID') +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  
} # eof





PlotCOLLAPSED(
  model = out_seed1,
  nClusters = 4,
  clusterToPlot = 4,
  data = affect,
  PID = "PID",
  timepoints = "OCCASION",
  variable = "Depressed"
)


### plot each variable of each cluster into a single plot 
# 7 plots
# each plot ave grid of 2 columns and according number of rows


variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")
cluster_range <- 3 

for (nClusters in cluster_range) {
  plot_list <- list()
  for (variable in variables) {
    for (clusterToPlot in 1:nClusters) {
      p <- PlotCOLLAPSED(
        model = out_seed1,
        nClusters = nClusters,
        clusterToPlot = clusterToPlot,
        variable = variable,
        show_legend = FALSE,
        plot_margin = c(0.05, 0.05, 0.05, 0.05),
        remove_xlab = TRUE,
        title = paste0("Cluster ", clusterToPlot)
          )
      if (is.null(p)) p <- grid::nullGrob()
      plot_list[[length(plot_list) + 1]] <- p
    }
  }
  grid.arrange(grobs = plot_list, ncol = nClusters, nrow = length(variables))
}




PlotCOLLAPSED <- function(model, 
                          nClusters, 
                          clusterToPlot, 
                          data = affect,
                          variable, 
                          PID = "PID", 
                          timepoints = "OCCASION", 
                          showN = FALSE,
                          show_legend = TRUE,
                          plot_margin = c(1,1,1,1),
                          title = paste0('Time series of ', variable, ' in Cluster ', clusterToPlot),
                          remove_xlab = FALSE) {
  
  # ----- extract classification of best model -----
  best_runs <- getBestModel(model)
  best_run <- best_runs[paste0("K", nClusters)]
  
  class_mat <- t(model$All_Models[[nClusters]][[1]][[best_run]]$Classification)
  
  # ----- df of id and classification -----
  class_df <- data.frame(id = as.numeric(rownames(class_mat)),
                         classification = as.integer(class_mat[, 1]))
  
  # ----- extract id and get timeseries data of those ids -----
  cluster_ids <- class_df[class_df$classification == clusterToPlot, "id"]
  cluster_data <- data[data[[PID]] %in% cluster_ids, ]
  
  if (showN) {
    message("Number of participants classified into Cluster ", clusterToPlot, ": ", length(cluster_ids))
  }
  
  # ----- plot -----
  ggplot(cluster_data, aes_string(x = timepoints, 
                                  y = variable, 
                                  group = paste0("factor(", PID, ")"), 
                                  colour = paste0("factor(", PID, ")"))) +
    geom_line() +
    labs(
      x = if (remove_xlab) NULL else timepoints,
      y = variable,
      title = title
    ) +
    scale_colour_discrete(name = 'Participant ID') +
    theme_minimal() +
    theme(
      legend.position = if (show_legend) "right" else "none",
      plot.margin = unit(plot_margin, "cm")
    )
}


#hello



#### plot contemp ####

nClusters <- 3
coefs <- coef(out_seed1, Model = rep(1, nClusters))$Sigma

?coef

# all contemporaneous parameters
coefs1 <- list()
K <- 6
for(k in 1:K) coefs1[[k]] <- coef.ClusterVAR(out_seed1, Model = rep(1, k))

innovations1 <- sapply(coefs1, function(sublist) sublist[["Sigma"]])
var_names <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")

contemporaneous1 <- vector("list", length(innovations1))
for (i in seq_along(innovations1)) {
  ar <- innovations1[[i]]
  dims <- dim(ar)
  
  pcor <- array(NA, dim = dims)
  for (j in 1:dims[3]) {
    pcor[,,j] <- cor2pcor(ar[,,j])
  }
  
  dimnames(pcor) <- list(var_names, var_names, NULL)
  contemporaneous1[[i]] <- pcor
}

cor2pcor(l_sigma[[1]])
length(l_sigma)
l_sigma

?array


######

l_pcor <- list()
for (i in 1:length(l_sigma)) l_pcor[[i]] <- cor2pcor(l_sigma[[i]])


#####

contemporaneous1[[3]]
x <- out_seed1

variables

PlotCONTEMPORANEOUS <- function(x, nClusters,
                                sidebar = TRUE,
                                mar_heat = c(2.5,1.5,1.5,1.5), 
                                cex.axis = 0.8, 
                                cex.val = 0.5, 
                                labels = NULL,
                                legend = TRUE,
                                variable_names = var_names,
                                small_difference_to_white = FALSE) {
  
  # ----- get coefficients -----
  coefs <- coef(x, Model = rep(1, nClusters))$Sigma # still correlation 
  dims <- dim(coefs)
  K <- dims[3]
  
  l_sigma <- list() # put coef into list form
  for(i in 1:K) l_sigma[[i]] <- coefs[, , i] 
  
  l_pcor <- list() # transform to partical correlation
  for (i in 1:length(l_sigma)) l_pcor[[i]] <- cor2pcor(l_sigma[[i]])
  
  
  # ----- set layout  -----
  lmat <- SETlayout(K)
  lmat <- cbind(max(lmat)+1, lmat)
  
  graphics::layout(mat=lmat,
                   widths = c(0.2, 0.2, rep(1, K)),
                   heights =  c(0.2, rep(1, K)))
  
  # ----- plot labels -----
  plotLabel <- function(x, srt=0, col="black",
                        xpos=.5, ypos=.5, cex=1.5) {
    par(mar=rep(0, 4))
    plot.new()
    plot.window(xlim=c(0,1), ylim=c(0,1))
    text(xpos, ypos, x, srt=srt, cex=cex, col=col)
  }
  
  for(i in 1:K) plotLabel(paste0("Cluster ", i), cex=1.5)
  for(i in 1:K) plotLabel(paste0("Cluster ", i), cex=1.5, srt=90)
  
  
  
  # ----- plot pcor -----
  for(i in 1:K) PlotHEAT(phi = l_pcor[[i]],
                         k = i,
                         main = paste0("Cluster ", i),
                         cex.axis = cex.axis,
                         cex.val = cex.val, 
                         mar = mar_heat,
                         labels = labels,
                         pcor = TRUE)
  
  
  # ----- plot coef differences -----
  for(k1 in 1:(K-1)) {
    for(k2 in 2:K) {
      
      if(k1>=k2) {
        NULL
      } else {
        pcor_diff <- l_pcor[[k1]] - l_pcor[[k2]]
        PlotHEAT(phi = pcor_diff, 
                 k = K, 
                 main = paste0("Difference: Cluster ", k1, " - Cluster ", k2),
                 cex.axis = cex.axis,
                 cex.val = cex.val, 
                 mar = mar_heat,
                 labels = labels,
                 small_difference_to_white = small_difference_to_white,
                 pcor = TRUE)
      }
      
    }
  } # end of plot differences
  
  
  # ----- legend -----
  if (legend && !is.null(var_names)) {
    par(mar=rep(1, 4))
    plot.new()
    plot.window(xlim=c(0,1), ylim=c(0,1))
    text(0, .95, "Variables:", adj = c(0,1), font = 2, cex = 1.3)
    for (i in seq_along(var_names)) {
      text(0, 0.88 - i * 0.11, paste0("Y", i, " = ", var_names[i]), adj = 0, cex = 1.3)
    }
  } 
  # ----- sidebar -----
  if (sidebar) {
    sum1 <- summary(x, show = "GNL") # get proportions
    sum1_IC <- sum1$FunctionOutput$Proportions
    
    sidebar <- data.frame(Class = LETTERS[1:nClusters],
                          Proportion = as.numeric(unlist(strsplit(sum1_IC[nClusters], " "))))
    
    sidebar$location <- 1 - (cumsum(sidebar$Proportion) - sidebar$Proportion / 2)
    sidebar$label <- paste0('C', 1:K, '\n', sidebar$Proportion)
    
    side <- ggplot(sidebar, aes(x = 1, y = Proportion, fill = Class)) +
      geom_bar(stat = 'identity', width = 1) +
      geom_text(aes(y = location, label = label), x = 1, colour = 'black', size = 8/K) +
      theme_void() +
      # ggtitle('mixing\nproportions') +
      scale_fill_brewer(palette = "Pastel1") +
      theme(
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none"
        # plot.title = element_text(hjust = 0.5, vjust = -3, size = 9)
      )
    
    vp <- viewport(
      x = 0.2 / (0.2 + 0.2 + 1 * K), y = 0.5 - (0.05 / K),
      width = (0.1 / K), height = 1,
      just = c("right", "center")
    )
    
    plot.new()
    print(side, newpage = FALSE, vp)
  }
  
  
} # eof



# ----- get coefficients -----
coefs <- coef(out_seed1, Model = rep(1, nClusters))$VAR_coefficients
dims <- dim(coefs)
K <- dims[3]

l_phi <- list() # put coef into list form
for(i in 1:K) l_phi[[i]] <- coefs[, , i] 


PlotCONTEMPORANEOUS(out_seed1, 3, labels = variables)



### change x axis lable ###


PlotHEAT <- function(phi,
                     k,
                     main,
                     labels = NULL,
                     las.x = 1,
                     cex.axis = 0.8,
                     cex.val = 0.7,
                     mar = c(2.5,2.5,2.5,2.5),
                     small_difference_to_white = FALSE,
                     pcor = FALSE) {
  
  # -- Aux Variables --
  p <- ncol(phi)
  
  # Fill in default labels
  if (is.null(labels)) labels <- paste0("Y", 1:p)
  
  # -- Make color gradient --
  color.gradient <- function(x, 
                             colors = c("#E41A1C", "white", "#377EB8"), 
                             colsteps = 201) {
    return(colorRampPalette(colors)(colsteps)[findInterval(x, seq(min(x), max(x), length.out = colsteps))])
  }
  x <- 1:201
  grad <- color.gradient(x)
  
  # Make canvas
  par(mar = mar)
  plot.new()
  plot.window(xlim = c(0,1), ylim = c(0, 1))
  
  # Auxiliary plotting variables
  sfm <- 1/(p*2)
  seq_mp_x <- seq(0, 1, length = p+1)[-(p+1)] + sfm
  
  # ----- shorten labels -----
  labels <- substr(labels, 1, 3)
  
  # Plot Axes & Axis labels
  if (!pcor) {
    labels_tm1 <- sapply(labels, function(label) bquote(.(label)[t-1]), simplify = FALSE)
  } else {
    labels_tm1 <- sapply(labels, function(label) bquote(.(label)[t]), simplify = FALSE)
  }
  labels_t1 <- sapply(labels, function(label) bquote(.(label)[t]), simplify = FALSE)
  axis(1, labels = do.call(expression, labels_tm1), at = seq_mp_x, cex.axis = cex.axis)
  axis(2, labels = do.call(expression, labels_t1[p:1]), at = seq_mp_x, las = 2, cex.axis = cex.axis)
  title(main, font.main = 1)
  
  # Plot Data
  for(i in 1:p) {
    for(j in 1:p) {
      # Get colour
      phi_ij <- phi[p:1, ][j, i]
      if (small_difference_to_white && abs(phi_ij) < 0.1) {
        col_ij <- "#FFFFFF"
      } else if (phi_ij < -1) {
        col_ij <- grad[1]
      } else if (phi_ij > 1) {
        col_ij <- grad[201]
      } else {
        col_ij <- grad[phi_ij * 100 + 101]
      }
      # Plot box
      rect(xleft = seq_mp_x[i]-sfm,
           ybottom = seq_mp_x[j]-sfm,
           xright = seq_mp_x[i]+sfm,
           ytop = seq_mp_x[j]+sfm,
           col = col_ij)
      # Plot text
      text(seq_mp_x[i], seq_mp_x[j], round(phi_ij , 2), cex = cex.val, col = "black")
    }
  }
}



ggplot(classproportions1_long, aes(x = Cluster, y = Proportion, fill = Class)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = ifelse(is.na(Proportion), "", sprintf("%.2f", Proportion))),
    position = position_stack(vjust = 1), # slightly above each section
    hjust = 0.5,   # left align
    vjust = 1.7,   # top align
    color = "black",
    size = 5
  ) +
  labs(title = "Seed 1",
       y = "Proportion of person per cluster",
       x = "",
       fill = "Clusters") +
  scale_fill_brewer(
    palette = "RdYlBu",
    labels = paste0("Cluster ", seq_along(unique(classproportions1_long$Class)))
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 12, colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        plot.title = element_text(margin = margin(r = 10), size = 14, colour = "black", hjust = 0.5),
        plot.margin = unit(c(0.5, 0.5, 0, 0.5), "cm")
  )






##### MEANS AND TRENDS #####
l_coefs <- list()
K <- 6
for(k in 1:K) l_coefs[[k]] <- coef.ClusterVAR(out_seed1, Model = rep(1, k))


k <- 4
m_ints <- l_coefs[[k]]$Exogenous_coefficients[, 1, ] # Note: since trend is included, those are means at t=0
m_ltr <- l_coefs[[k]]$Exogenous_coefficients[, 2, ] # Linear trends

# Colors
cols_k4 <- RColorBrewer::brewer.pal(k, "Set2")

nCluster <- 3
model <- out_seed1


PlotMEANS <- function(model, 
                      nCluster, 
                      variables) {

  l_coefs <- list()
  l_coefs <- coef.ClusterVAR(model, Model = rep(1, nCluster))
  
  # Means at t=0 for the selected cluster solution
  m_ints <- l_coefs$Exogenous_coefficients[, 1, ] 
  
  # Get proportions for this cluster solution
  sum_out <- summary(model, show="GNL")
  v_prop <- sum_out$FunctionOutput$Proportions
  m_float <- matrix(NA, K, K)
  for(i in 1:K) m_float[i,1:i] <- as.numeric(unlist(strsplit(v_prop[i], " ")))
  K_prop <- m_float[nCluster, 1:nCluster]
  
  cols_k <- RColorBrewer::brewer.pal(nCluster, "Set2")
  
  # Plot
  par(mar=c(3,3,2,1))
  plot.new()
  plot.window(xlim=c(1, length(variables)), ylim=c(0,100))
  axis(1, labels=variables, at=1:length(variables))
  axis(2, las=2)
  grid()
  title(main=expression("Estimated Mean at " ~ X[t] == 0), font.main=1)
  for(i in 1:nCluster) {
    lines(m_ints[, i], lwd=3, col=cols_k[i])
    points(m_ints[, i], pch=20, cex=2, col=cols_k[i])
  }
  legend(
    x = 5.4, y = 102, # try different values
    legend = paste0("Cluster ", 1:nCluster, " (", round(K_prop*100, 1), "%)"),
    text.col = cols_k,
    bty = "n"
  )
}

PlotMEANS(model = out_seed1, nCluster = 3, variable = variables)




### PLOT TREND ###
PlotTRENDS <- function(model, 
                       nCluster, 
                       variables) {
  
  # Get coefficients for the selected cluster solution
  l_coefs <- coef.ClusterVAR(model, Model = rep(1, nCluster))
  
  # Linear trends (slopes) for each cluster
  m_ltr <- l_coefs$Exogenous_coefficients[, 2, ]
  
  # Get proportions for this cluster solution
  sum_out <- summary(model, show="GNL")
  v_prop <- sum_out$FunctionOutput$Proportions
  K <- length(v_prop)
  m_float <- matrix(NA, K, K)
  for(i in 1:K) m_float[i,1:i] <- as.numeric(unlist(strsplit(v_prop[i], " ")))
  K_prop <- m_float[nCluster, 1:nCluster]
  
  cols_k <- RColorBrewer::brewer.pal(nCluster, "Set2")
  
  # Set y-axis limits automatically
  ymin <- min(m_ltr, na.rm = TRUE)
  ymax <- max(m_ltr, na.rm = TRUE)
  
  # Plot
  par(mar=c(3,3,2,1))
  plot.new()
  plot.window(xlim=c(1, length(variables)), ylim=c(ymin-.1, ymax+.1))
  axis(1, labels=variables, at=1:length(variables))
  axis(2, las=2)
  grid()
  abline(h=0, col="grey", lty=2)
  title(main="Slope of Linear Trend", font.main=1)
  for(i in 1:nCluster) {
    lines(m_ltr[, i], lwd=3, col=cols_k[i])
    points(m_ltr[, i], pch=20, cex=2, col=cols_k[i])
  }
  legend(
    "topright",
    legend = paste0("Cluster ", 1:nCluster, " (", round(K_prop*100, 1), "%)"),
    text.col = cols_k,
    bty = "n",
    cex = 0.9
  )
}

PlotTRENDS(out_seed1, 2, variables)






# ----- organise data -----
MEANS <- ddply(affect, .(PID), function(x) colMeans(x[, variables], na.rm=TRUE))
DEP <- cesd[,c(1,4)]

# ----- get classifications -----

l_coefs <- list()
K <- 6
for(k in 1:K) l_coefs[[k]] <- coef.ClusterVAR(out_seed1, Model = rep(1, k))



### --- 2 CLUSTERS --- ### 
k <- 2
cmemb2 <- as.numeric(l_coefs[[k]]$Classification)

external2 <- cbind(MEANS, DEP, as.factor(cmemb2))[, -9]
colnames(external2) <- c("PID", "Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed", "CESD", "Cluster")
head(external2)


# ----- Test whether DASS-scales are different across clusters -----
set.seed(12345)

## Baseline
# Fit models
null <- brm(CESD ~ 1, data = external2, family = gaussian(), chains = 4, iter = 2000)
alt <- brm(CESD ~ Cluster, data = external2, family = gaussian(), chains = 4, iter = 2000)

# Get BF
bf2 <- bayes_factor(alt, null)
round(bf$bf)

## With Covariates
null_cov <- brm(CESD ~ 1 + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external2, family = gaussian(), chains = 4, iter = 2000)
alt_cov <- brm(CESD ~ Cluster + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external2, family = gaussian(), chains = 4, iter = 2000)

# Get BF
bf_cov2 <- bayes_factor(alt_cov, null_cov)
round(bf_cov$bf)



### --- 3 CLUSTERS --- ### 
k <- 3
cmemb3 <- as.numeric(l_coefs[[k]]$Classification)

external3 <- cbind(MEANS, DEP, as.factor(cmemb3))[, -9]
colnames(external3) <- c("PID", "Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed", "CESD", "Cluster")
head(external3)


# ----- Test whether DASS-scales are different across clusters -----
set.seed(12345)

## Baseline
# Fit models
null3 <- brm(CESD ~ 1, data = external3, family = gaussian(), chains = 4, iter = 2000)
alt3 <- brm(CESD ~ Cluster, data = external3, family = gaussian(), chains = 4, iter = 2000)

# Get BF
bf3 <- bayes_factor(alt3, null3)
round(bf3$bf)

## With Covariates
null_cov3 <- brm(CESD ~ 1 + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external3, family = gaussian(), chains = 4, iter = 2000)
alt_cov3 <- brm(CESD ~ Cluster + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external3, family = gaussian(), chains = 4, iter = 2000)

# Get BF
bf_cov3 <- bayes_factor(alt_cov3, null_cov3)
round(bf_cov3$bf)




### --- 4 CLUSTERS --- ### 
k <- 4
cmemb4 <- as.numeric(l_coefs[[k]]$Classification)

external4 <- cbind(MEANS, DEP, as.factor(cmemb4))[, -9]
colnames(external4) <- c("PID", "Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed", "CESD", "Cluster")
head(external4)


# ----- Test whether DASS-scales are different across clusters -----
set.seed(12345)

## Baseline
# Fit models
null4 <- brm(CESD ~ 1, data = external4, family = gaussian(), chains = 4, iter = 2000)
alt4 <- brm(CESD ~ Cluster, data = external4, family = gaussian(), chains = 4, iter = 2000)

# Get BF
bf4 <- bayes_factor(alt4, null4)
round(bf$bf)

## With Covariates
null_cov4 <- brm(CESD ~ 1 + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external4, family = gaussian(), chains = 4, iter = 2000)
alt_cov4 <- brm(CESD ~ Cluster + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external4, family = gaussian(), chains = 4, iter = 2000)

# Get BF
bf_cov4 <- bayes_factor(alt_cov4, null_cov4)
round(bf_cov4$bf)


bfs <- matrix(
  c(
    round(bf2$bf),
    round(bf_cov2$bf),
    round(bf3$bf),
    round(bf_cov3$bf),
    round(bf4$bf),
    round(bf_cov4$bf)
  ),
  nrow = 3, ncol = 2, byrow = TRUE
)
rownames(bfs) <- c("2 clusters", "3 clusters", "4 clusters")
colnames(bfs) <- c("Base", "Covar")



#### MEANS AND SDS ####



descriptives4 <- as.data.frame(descriptives4)



PlotCOLLAPSED <- function(model, 
                          nClusters, 
                          clusterToPlot, 
                          data = affect,
                          variable, 
                          PID = "PID", 
                          timepoints = "OCCASION", 
                          showN = FALSE,
                          show_legend = TRUE,
                          plot_margin = c(1,1,1,1),
                          title = paste0('Time series of ', variable, ' in Cluster ', clusterToPlot),
                          remove_xlab = FALSE,
                          opaqueness = 1) {
  
  # ----- extract classification of best model -----
  best_runs <- getBestModel(model)
  best_run <- best_runs[paste0("K", nClusters)]
  
  class_mat <- t(model$All_Models[[nClusters]][[1]][[best_run]]$Classification)
  
  # ----- df of id and classification -----
  class_df <- data.frame(id = as.numeric(rownames(class_mat)),
                         classification = as.integer(class_mat[, 1]))
  
  # ----- extract id and get timeseries data of those ids -----
  cluster_ids <- class_df[class_df$classification == clusterToPlot, "id"]
  cluster_data <- data[data[[PID]] %in% cluster_ids, ]
  
  if (showN) {
    message("Number of participants classified into Cluster ", clusterToPlot, ": ", length(cluster_ids))
  }
  
  # ----- handle MSD title -----
  if (identical(title, "MSD")) {
    m <- mean(cluster_data[[variable]], na.rm = TRUE)
    s <- sd(cluster_data[[variable]], na.rm = TRUE)
    title <- paste0("Cluster ", clusterToPlot, ", M = ", round(m, 2), ", SD = ", round(s, 2))
  }
  
  # ----- plot -----
  ggplot(cluster_data, aes_string(x = timepoints, 
                                  y = variable, 
                                  group = paste0("factor(", PID, ")"), 
                                  colour = paste0("factor(", PID, ")"))) +
    geom_line(alpha = opaqueness) +
    ylim(0, 100) +
    labs(
      x = if (remove_xlab) NULL else timepoints,
      y = variable,
      title = title
    ) +
    scale_colour_discrete(name = 'Participant ID') +
    theme_minimal() +
    theme(
      legend.position = if (show_legend) "right" else "none",
      plot.margin = unit(plot_margin, "cm")
    )
}



# individual collapsed plot example
PlotCOLLAPSED(
  model = out_seed1,
  nClusters = 3,
  clusterToPlot = 1,
  data = affect,
  PID = "PID",
  timepoints = "OCCASION",
  variable = "Depressed",
  title = "MSD"
)




# descriptives for 4 clusters

pid_class <- data.frame(PID = unique(MEANS$PID), Cluster4 = cmemb4)
affect4 <- merge(affect, pid_class, by = "PID", all.x = TRUE)

descriptives4 <- affect4 %>%
  group_by(Cluster4) %>%
  summarise(across(
    all_of(variables),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))

descriptives4 <- as.data.frame(descriptives4)



ranked_list <- lapply(variables, function(var) {
  # Select cluster, mean, and sd columns for this variable
  df <- descriptives4 %>%
    select(Cluster4, 
           mean = paste0(var, "_mean"), 
           sd = paste0(var, "_sd")) %>%
    arrange(desc(mean)) %>%
    mutate(Rank = row_number()) %>%
    select(Rank, Cluster4, mean, sd)
  df
})

names(ranked_list) <- variables

ranked_list[["Happy"]]


# autocorrelations of cluster 4
coef4 <- coef(out_seed1, Model = rep(1, 4))$VAR_coefficients
diag_list <- lapply(1:dim(coef4)[3], function(k) diag(coef4[,,k]))
names(diag_list) <- paste0("Cluster ", 1:dim(coef4)[3])


# Convert diag_list to a matrix and then to a data frame
diag_mat <- do.call(rbind, diag_list)
colnames(diag_mat) <- variables
rownames(diag_mat) <- names(diag_list)
auto4 <- as.data.frame(diag_mat)


PlotHEAT()


cont4 <- contemporaneous1[[4]]

cont4[,,1]



# cluster 2 contemporaneous interpretation

c12 <- cont4[,,1] - cont4[,,2]
c32 <- cont4[,,3] - cont4[,,2]

layout(matrix(1:2, ncol=2))
PlotHEAT(c12, main = "Cluster 1 - 2", labels = variables, small_difference_to_white = T)
PlotHEAT(c32, main = "Cluster 3 - 2", labels = variables, small_difference_to_white = T)


layout(matrix(1:2, ncol=2))
PlotHEAT(c12, main = "Cluster 1 - 2", labels = variables, pcor = T)
PlotHEAT(c32, main = "Cluster 3 - 2", labels = variables, pcor = T)




cluster_range <- 4

for (nClusters in cluster_range) {
  plot_list <- list()
  for (variable in variables) {
    for (clusterToPlot in 1:nClusters) {
      set.seed(336)
      
      p <- PlotCOLLAPSED(
        model = out_seed1,
        nClusters = nClusters,
        clusterToPlot = clusterToPlot,
        variable = variable,
        show_legend = FALSE,
        plot_margin = c(0.05, 0.05, 0.05, 0.05),
        remove_xlab = TRUE,
        opaqueness = 0.8,
        random = TRUE
      )
      if (is.null(p)) p <- grid::nullGrob()
      plot_list[[length(plot_list) + 1]] <- p
    }
  }
  grid.arrange(grobs = plot_list, ncol = nClusters, nrow = length(variables))
}







###### make ind from each cluster graph ##### 

# make layout
lmat <- matrix(1:56, 7, 8, byrow = TRUE)
lmat <- rbind(c(57, 57, 58, 58, 59, 59, 60, 60), lmat)

lo <- layout(lmat, widths = c(1, .25, 1, .25, 1, .25, 1, .25), heights = c(0.5,1,1,1,1,1,1,1))
# layout.show(lo)

# all variables
for (j in variables) {
  for (i in id4) {
    PlotTS_Flex(data = data,
                IDcol = "ID",
                ID = i, # Subject number, here fixed
                variable = j, # Variable
                layout = FALSE,
                title = TRUE,
                ylab = TRUE,
                xlab = TRUE,
                xlim = c(1, 80))
  }
}

for(i in 1:4)   plotLabel(paste0("Cluster ", i, " (ID = ", id4[i], ")"))


?text





### SCALED BIC CI ###

?load

load("/Users/Lexi/Desktop/internship/5_ results/Robustness_ 1to8.RData")


# Get BIC
sum1 <- summary(out_seed1, show = "GNL")

BIC <- sum1$FunctionOutput$BIC
BIC_scaled <- BIC/max(BIC)

BIC_robustness <- sweep(BIC_1to8, 2, apply(BIC_1to8, 2, max), "/")

bic_mean <- apply(BIC_robustness, 1, mean, na.rm = TRUE)
bic_se <- apply(BIC_robustness, 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))

# 95% CI
bic_ci_lower <- bic_mean - qnorm(0.975) * bic_se
bic_ci_upper <- bic_mean + qnorm(0.975) * bic_se

bic_ci_df <- data.frame(
  Cluster = rownames(BIC_robustness),
  Mean = bic_mean,
  Lower = bic_ci_lower,
  Upper = bic_ci_upper
)




PlotScaled(out_seed1, title = "Random Seed 1")

clusters <- as.numeric(gsub(" Clusters", "", rownames(BIC_robustness)))


x <- as.numeric(gsub("[^0-9]", "", bic_ci_df$Cluster))  # Extract numeric cluster numbers
y <- bic_ci_df$Mean
lower <- bic_ci_df$Lower
upper <- bic_ci_df$Upper

# Plot mean line
plot(x, y, type = "l", col = "red", lwd = 2,
     ylim = range(c(lower, upper, y), na.rm = TRUE),
     xlab = "Number of Clusters", ylab = "Mean Scaled BIC",
     main = "Mean Scaled BIC with 95% CI")

# Add shaded confidence interval
polygon(c(x, rev(x)), c(lower, rev(upper)),
        col = rgb(1, 0, 0, 0.2), border = NA)

# Redraw mean line on top
lines(x, y, col = "black", lwd = 1)


##### plot scaled BIC
BIC_robustness <- apply(BIC_1to8, 2, function(col) col / max(col))

BIC_mean <- apply(BIC_robustness, 1, mean)
BIC_se <- apply(BIC_robustness, 1, function(x) sd(x) / sqrt(30))

# 95% CI
BIC_lower <- BIC_mean - qnorm(0.975) * BIC_se
BIC_higher <- BIC_mean + qnorm(0.975) * BIC_se



# set layout 
plot.new()
ymax <- 1
ymin <- 0.98

K <- nrow(BIC_robustness)
plot.window(xlim=c(1,K), ylim=c(ymin, ymax))

axis(1, 1:K)
axis(2, las=2)
grid()

title(xlab="Number of clusters", line=2.5)
title("Scaled BIC", font.main=1)


# plot data
points((1:K), BIC_mean, col="#E41A1C", pch=16, cex=1.25)
lines((1:K), BIC_mean, col="#E41A1C", lwd=1.5)

polygon(
  c(1:K, rev(1:K)),
  c(BIC_lower, rev(BIC_higher)),
  col = rgb(0.3, 0.6, 1, 0.2),
  border = NA
)



negLL <- -sum1$FunctionOutput$`log-likelihood`
negLL_scaled <- negLL/max(negLL)





#### matrix of three and four cluster model

#     c1  c2  c3
# c1  
# c2
# c3
# c4



# ----- extract classification of best model -----
best_runs <- getBestModel(out_seed1)

best3 <- best_runs[3]
best4 <- best_runs[4]

clas3 <- t(out_seed1$All_Models[[3]][[1]][[best3]]$Classification)
clas4 <- t(out_seed1$All_Models[[4]][[1]][[best4]]$Classification)


# ----- df of id and classification -----
class3 <- data.frame(id = as.numeric(rownames(clas3)),
                     classification = paste0("Cluster ", clas3[, 1]))

class4 <- data.frame(id = as.numeric(rownames(clas4)),
                     classification = paste0("Cluster ", clas4[, 1]))


# ----- how do i put this in  matrix -----
merged_classes <- merge(class3, class4, by = "id", suffixes = c("_3", "_4"))
cross_mat <- addmargins(table(merged_classes$classification_3, merged_classes$classification_4))


#### absolute correlation #####
# get temporal parameters
coefs1 <- list()
K <- 6
for(k in 1:K) coefs1[[k]] <- coef.ClusterVAR(out_seed1, Model = rep(1, k))

temporal1 <- sapply(coefs1, function(sublist) sublist[["VAR_coefficients"]])


# set up matrix
abs_cor <- matrix(NA, nrow = 5, ncol = 4)
colnames(abs_cor) <- c("Two clusters", "Three clusters", "Four clusters", "Five clusters")
rownames(abs_cor) <- paste("Cluster", 1:5)


# for loop to get absolute correlations
for (j in 1:4) {
  
  # cluster 1
  abs_cor[1, j] <- sum(abs(temporal1[[j+1]][,,1]))/49 
  
  # rest of clusters
  for (i in 1:j+1) abs_cor[i, j] <- sum(abs(temporal1[[j+1]][,,i]))/49
}




# NLL robustness


NLL_mean <- apply(scaledNLL_1to8, 1, mean)
NLL_se <- apply(scaledNLL_1to8, 1, function(x) sd(x) / sqrt(30))

NLL_lower <- NLL_mean - qnorm(0.975) * NLL_se
NLL_higher <- NLL_mean + qnorm(0.975) * NLL_se


# ----- set layout -----
plot.new()
ymax <- 1
ymin <- 0.96

K <- nrow(scaledNLL_1to8)
plot.window(xlim=c(1,K), ylim=c(ymin, ymax))

axis(1, 1:K)
axis(2, las=2)
grid()

title(xlab="Number of clusters", line=2.5)
title("Scaled NLL", font.main=1)


# ----- plot -----
points((1:K), NLL_mean, col="#E41A1C", pch=16, cex=1.25)
lines((1:K), NLL_mean, col="#E41A1C", lwd=1.5)

polygon(
  c(1:K, rev(1:K)),
  c(NLL_lower, rev(NLL_higher)),
  col = rgb(0.3, 0.6, 1, 0.2),
  border = NA
)



# correlate the time lag matrices

# two clusters
cor(as.vector(temporal1[[2]][,,1]), 
    as.vector(temporal1[[2]][,,2]))

# three clusters
cor(as.vector(temporal1[[3]][,,1]), 
    as.vector(temporal1[[3]][,,2]))

cor(as.vector(temporal1[[3]][,,1]), 
    as.vector(temporal1[[3]][,,3]))

cor(as.vector(temporal1[[3]][,,2]), 
    as.vector(temporal1[[3]][,,3]))



laggedCors <- function(temporal_array) {
  n_clusters <- dim(temporal_array)[3]
  cor_mat <- matrix(NA, n_clusters, n_clusters)
  for (i in 1:n_clusters) {
    for (j in i:n_clusters) {
      cor_val <- cor(
        as.vector(temporal_array[,,i]),
        as.vector(temporal_array[,,j])
      )
      cor_mat[i, j] <- cor_val
      cor_mat[j, i] <- cor_val
    }
  }
  colnames(cor_mat) <- paste0("C", 1:n_clusters)
  rownames(cor_mat) <- paste0("C", 1:n_clusters)
  return(cor_mat)
}

cor2 <- laggedCors(temporal1[[2]])
cor3 <- laggedCors(temporal1[[3]])
cor4 <- laggedCors(temporal1[[4]])
cor5 <- laggedCors(temporal1[[5]])

round(cor3, 2)
round(cor4, 2)
round(cor5, 2)



temporal1


# R^2 do this for two three and four cluster model
# -------- Compute Predictions --------

trial <- LCVARPred(object = out_seed1,
                    data = data,
                    k = 3)


# ---------- Compute R2 for each person and variable ----------

l_R2s <- lapply(l_pred$Predictions, function(x) {
  R2s <- c(cor(x$Happy_hat, x$Happy, use="complete.obs")^2,
           cor(x$Relaxed_hat, x$Relaxed, use="complete.obs")^2,
           cor(x$Sad_hat, x$Sad, use="complete.obs")^2,
           cor(x$Angry_hat, x$Angry, use="complete.obs")^2)
  return(R2s)
})
m_R2s <- do.call(rbind, l_R2s)
colnames(m_R2s) <- vars_em

# Plotting
df_R2 <- data.frame(values = as.vector(m_R2s),
                    Variables = rep(colnames(m_R2s), each = nrow(m_R2s)),
                    Clusters = rep(l_pred$Classification, times = ncol(m_R2s)))

mean(df_R2[df_R2$Clusters==3 & df_R2$Variables=="Happy",]$values)

# For plotting replace Variables with integers, to avoid sorting issue
df_R2_plot <- df_R2
df_R2_plot$Variables[df_R2_plot$Variables=="Happy"] <- 1
df_R2_plot$Variables[df_R2_plot$Variables=="Relaxed"] <- 2
df_R2_plot$Variables[df_R2_plot$Variables=="Sad"] <- 3
df_R2_plot$Variables[df_R2_plot$Variables=="Angry"] <- 4

# pdf("Figures/Fig_ResAnalysis_R2.pdf", width = 7, height=5)

boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
        data = df_R2_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k4, each=4), ylim=c(0, .7))
grid()
boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
        data = df_R2_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k4, each=4), ylim=c(0, .7), add=TRUE)
axis(1, 1:16, rep(vars_em, times=4), las=2)
axis(2, las=2)
title(ylab="Proportion of Explained Variance")
mp <- seq(1, 16, length=9)
mps <- mp[c(2, 4, 6, 8)]
for(k in 1:4) text(mps[k], 0.7, paste0("Cluster ", k), col=cols_k4[k])

# dev.off()



data_s <- data


xtm1 <- sweep(data_s[-N, p_loc], 2, mu, "-") - matrix(rep(lin, N-1), ncol=7, byrow=T) * data_s$Time[-N]
xt <- as.matrix(xtm1) %*% matrix(phi[j, ], nrow=7) + mu[j] + lin[j] * data_s$Time[-1]

xtm1 <- sweep(data_s[-N, p_loc], 2, mu, "-") - matrix(rep(lin, N-1), ncol=4, byrow=T) * data_s$Time[-N]

xt <- as.matrix(xtm1) %*% matrix(phi[j, ], nrow=7) + mu[j] + lin[j] * data_s$Time[-1]
m_pred[2:N, j] <- as.numeric(xt)

length(p_loc)



VARPred <- function(mu, lin, phi, data_s, method="matrix") {
  
  # Identify predictable time points
  N <- nrow(data_s)
  p <- 7
  p_loc <- 5:11
  ind_pred <- (data_s$DayNr[-1] == data_s$DayNr[-N]) & ((data_s$Beep[-1] - data_s$Beep[-N]) == 1)
  ind_pred <- c(NA, ind_pred) # The first is always unpredictable
  
  m_pred <- matrix(NA, N, p)
  # Predict
  for(j in 1:7) {
    
    # ----- Version 1: Element Wise -----
    if(method=="elementwise") {
      for(t in 2:N) {
        xtm1 <- data_s[t-1, p_loc] - mu - lin * data_s$Time[t-1]
        xt <- sum(phi[j, ] * xtm1) + mu[j] + lin[j] * data_s$Time[t]
        m_pred[t, j] <- xt
      }
    }
    # ----- Version 2: Vectorized -----
    if(method=="matrix") {
      # print(lin)
      # print(N)
    
      xtm1 <- sweep(data_s[-N, p_loc], 2, mu, "-") - matrix(rep(lin, N-1), ncol=p, byrow=TRUE) * data_s$Time[-N]
      
      # print(dim(xtm1))
      # print(length(phi[j, ]))
      xt <- as.matrix(xtm1) %*% matrix(phi[j, ], nrow=p) + mu[j] + lin[j] * data_s$Time[-1]
      m_pred[2:N, j] <- as.numeric(xt)
    }
  }
  # Delete inadmissible rows
  
  # data_s$Happy[207]
  # is.na(data_s$Happy)[207]
  
  # browser()
  
  m_pred[!ind_pred, ] <- NA
  NAs <- apply(data_s, 1, function(x) any(is.na(x[p_loc])))
  m_pred[NAs, ] <- NA
  
  
  # Make dataframe
  out_df <- data.frame(cbind(data_s[, 1:11], m_pred))
  colnames(out_df)[1:11] <- colnames(data_s)[1:11]
  colnames(out_df)[12:18] <- paste0(colnames(data_s)[5:11], "_hat")
  
  return(out_df)
  
} # eoF


# Function 2: Get Individual Models
# Input:
# LCVAR output object
# Output:
# Predictions for all predicable time points of all persons, Using Function 1



k <- 4
model_k <- coef.ClusterVAR(out_seed1, Model = rep(1, k))
c_i <- as.numeric(model_k$Classification)[1]

mu <- model_k$Exogenous_coefficients[, 1, c_i]
lin <- model_k$Exogenous_coefficients[, 2, c_i]
phi <- model_k$VAR_coefficients[, , c_i]



LCVARPred <- function(object, data, k) {
  
  # Loop over people
  u_pers <- unique(data$ID)
  n_pers <- length(u_pers)
  
  l_pred <- list()
  for(i in 1:n_pers) {
    
    ### Get model for person i
    # Get classification for person i
    model_k <- coef.ClusterVAR(object, Model = rep(1, k))
    c_i <- as.numeric(model_k$Classification)[i]
    # Get model of cluster c for person i
    mu <- model_k$Exogenous_coefficients[, 1, c_i]
    lin <- model_k$Exogenous_coefficients[, 2, c_i]
    phi <- model_k$VAR_coefficients[, , c_i]
    
    
    ### Subset Data
    data_s <- data[data$ID==u_pers[i], ]
    
    ### Make Predictions
    l_pred[[i]] <- VARPred(mu=mu, lin=lin, phi=phi, data_s=data_s)
    
  } # end for
  
  ## Return
  outlist <- list("Predictions"=l_pred,
                  "Classification"=as.numeric(model_k$Classification))
  return(outlist)
  
} # eof





# actually get r^2 now

#### TWO CLUSTER MODEL #### 
# -------- Compute Predictions --------
pred2 <- LCVARPred(object = out_seed1,
                   data = data,
                   k = 2)

# ---------- Compute R2 for each person and variable ----------
c2_R2s <- lapply(pred2$Predictions, function(x) {
  R2s <- c(cor(x$Happy_hat, x$Happy, use="complete.obs")^2,
           cor(x$Relaxed_hat, x$Relaxed, use="complete.obs")^2,
           cor(x$Sad_hat, x$Sad, use="complete.obs")^2,
           cor(x$Angry_hat, x$Angry, use="complete.obs")^2,
           cor(x$Anxious_hat, x$Anxious, use="complete.obs")^2,
           cor(x$Depressed_hat, x$Depressed, use="complete.obs")^2,
           cor(x$Stressed_hat, x$Stressed, use="complete.obs")^2)
  return(R2s)
})

m2_R2s <- do.call(rbind, c2_R2s)
colnames(m2_R2s) <- variables

# Plotting
df2_R2 <- data.frame(values = as.vector(m2_R2s),
                     Variables = rep(colnames(m2_R2s), each = nrow(m2_R2s)),
                     Clusters = rep(pred2$Classification, times = ncol(m2_R2s)))

# For plotting replace Variables with integers, to avoid sorting issue
df2_plot <- df2_R2
df2_plot$Variables[df2_plot$Variables=="Happy"] <- 1
df2_plot$Variables[df2_plot$Variables=="Relaxed"] <- 2
df2_plot$Variables[df2_plot$Variables=="Sad"] <- 3
df2_plot$Variables[df2_plot$Variables=="Angry"] <- 4
df2_plot$Variables[df2_plot$Variables=="Anxious"] <- 5
df2_plot$Variables[df2_plot$Variables=="Depressed"] <- 6
df2_plot$Variables[df2_plot$Variables=="Stressed"] <- 7

# plot
pdf("/Users/Lexi/Desktop/internship/4_ plots/19a_ r2 two clusters.pdf", width=5, height=5)
cols_k2 <- RColorBrewer::brewer.pal(2, "Set2")

boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
        data = df2_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k2, each=7), ylim=c(0, .8))
grid()
boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
        data = df2_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k2, each=7), ylim=c(0, .8), add = TRUE)

# axis and title
axis(1, 1:14, rep(variables, times=2), las=2)
axis(2, las=2)
title(ylab="Proportion of Explained Variance")
title("Two Cluster Model")

# add cluster names
nClusters <- 2  
nVars <- 7      
mp <- seq(1, nClusters * nVars, length.out = nClusters + 1)
mps <- (mp[-1] + mp[-length(mp)]) / 2  # centers of each cluster group
for(k in 1:nClusters) {
  text(mps[k], 0.8, paste0("Cluster ", k), col=cols_k2[k])
}

dev.off()



#### THREE CLUSTER MODEL #### 
# -------- Compute Predictions --------
pred3 <- LCVARPred(object = out_seed1,
                   data = data,
                   k = 3)

# ---------- Compute R2 for each person and variable ----------

c3_R2s <- lapply(pred3$Predictions, function(x) {
  R2s <- c(cor(x$Happy_hat, x$Happy, use="complete.obs")^2,
           cor(x$Relaxed_hat, x$Relaxed, use="complete.obs")^2,
           cor(x$Sad_hat, x$Sad, use="complete.obs")^2,
           cor(x$Angry_hat, x$Angry, use="complete.obs")^2,
           cor(x$Anxious_hat, x$Anxious, use="complete.obs")^2,
           cor(x$Depressed_hat, x$Depressed, use="complete.obs")^2,
           cor(x$Stressed_hat, x$Stressed, use="complete.obs")^2)
           
  return(R2s)
})


m3_R2s <- do.call(rbind, c3_R2s)
colnames(m3_R2s) <- variables

# Plotting
df3_R2 <- data.frame(values = as.vector(m3_R2s),
                     Variables = rep(colnames(m3_R2s), each = nrow(m3_R2s)),
                     Clusters = rep(pred3$Classification, times = ncol(m3_R2s)))

# mean(df3_R2[df3_R2$Clusters==3 & df3_R2$Variables=="Happy",]$values)

# For plotting replace Variables with integers, to avoid sorting issue
df3_plot <- df3_R2
df3_plot$Variables[df3_plot$Variables=="Happy"] <- 1
df3_plot$Variables[df3_plot$Variables=="Relaxed"] <- 2
df3_plot$Variables[df3_plot$Variables=="Sad"] <- 3
df3_plot$Variables[df3_plot$Variables=="Angry"] <- 4
df3_plot$Variables[df3_plot$Variables=="Anxious"] <- 5
df3_plot$Variables[df3_plot$Variables=="Depressed"] <- 6
df3_plot$Variables[df3_plot$Variables=="Stressed"] <- 7


# plot
pdf("/Users/Lexi/Desktop/internship/4_ plots/19b_ r2 three clusters.pdf", width=7, height=5)
cols_k7 <- RColorBrewer::brewer.pal(7, "Set2")

boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
        data = df3_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k7, each=7), ylim=c(0, .8))
grid()
boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
        data = df3_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k7, each=7), ylim=c(0, .8), add = TRUE)

# axis and title
axis(1, 1:21, rep(variables, times=3), las=2)
axis(2, las=2)
title(ylab="Proportion of Explained Variance")
title("Three Cluster Model")

# add cluster names
nClusters <- 3  
nVars <- 7      
mp <- seq(1, nClusters * nVars, length.out = nClusters + 1)
mps <- (mp[-1] + mp[-length(mp)]) / 2  # centers of each cluster group
for(k in 1:nClusters) {
  text(mps[k], 0.8, paste0("Cluster ", k), col=cols_k7[k])
}

dev.off()





#### FOUR CLUSTER MODEL #### 
# -------- Compute Predictions --------
pred4 <- LCVARPred(object = out_seed1,
                   data = data,
                   k = 4)

# ---------- Compute R2 for each person and variable ----------
c4_R2s <- lapply(pred4$Predictions, function(x) {
  R2s <- c(cor(x$Happy_hat, x$Happy, use="complete.obs")^2,
           cor(x$Relaxed_hat, x$Relaxed, use="complete.obs")^2,
           cor(x$Sad_hat, x$Sad, use="complete.obs")^2,
           cor(x$Angry_hat, x$Angry, use="complete.obs")^2,
           cor(x$Anxious_hat, x$Anxious, use="complete.obs")^2,
           cor(x$Depressed_hat, x$Depressed, use="complete.obs")^2,
           cor(x$Stressed_hat, x$Stressed, use="complete.obs")^2)
  return(R2s)
})

m4_R2s <- do.call(rbind, c4_R2s)
colnames(m4_R2s) <- variables

# Plotting
df4_R2 <- data.frame(values = as.vector(m4_R2s),
                     Variables = rep(colnames(m4_R2s), each = nrow(m4_R2s)),
                     Clusters = rep(pred4$Classification, times = ncol(m4_R2s)))

# For plotting replace Variables with integers, to avoid sorting issue
df4_plot <- df4_R2
df4_plot$Variables[df4_plot$Variables=="Happy"] <- 1
df4_plot$Variables[df4_plot$Variables=="Relaxed"] <- 2
df4_plot$Variables[df4_plot$Variables=="Sad"] <- 3
df4_plot$Variables[df4_plot$Variables=="Angry"] <- 4
df4_plot$Variables[df4_plot$Variables=="Anxious"] <- 5
df4_plot$Variables[df4_plot$Variables=="Depressed"] <- 6
df4_plot$Variables[df4_plot$Variables=="Stressed"] <- 7


# plot
pdf("/Users/Lexi/Desktop/internship/4_ plots/19c_ r2 four clusters.pdf", width=10, height=5)
cols_k7 <- RColorBrewer::brewer.pal(7, "Set2")

boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
        data = df4_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k7, each=7), ylim=c(0, .8))
grid()
boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
        data = df4_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k7, each=7), ylim=c(0, .8), add = TRUE)

# axis and title
axis(1, 1:28, rep(variables, times=4), las=2)
axis(2, las=2)
title(ylab="Proportion of Explained Variance")
title("Four Cluster Model")

# add cluster names
nClusters <- 4  
nVars <- 7      
mp <- seq(1, nClusters * nVars, length.out = nClusters + 1)
mps <- (mp[-1] + mp[-length(mp)]) / 2  # centers of each cluster group
for(k in 1:nClusters) {
  text(mps[k], 0.8, paste0("Cluster ", k), col=cols_k7[k])
}

dev.off()






# PLOT PREDICTION VERSUS ACTUAL ON THE 4 PARTICIPANTS

# ---------- Plotting Residuals for 3 Time Series in Paper ----------

# Draw same three random subjects as above

pdf("/Users/Lexi/Desktop/internship/4_ plots/20_ predictions 4p.pdf", width=8, height = 9)

# Layout
lmat <- matrix(5:16, 4, 3, byrow = TRUE)
lmat <- cbind(1:4, lmat)
lo <- layout(lmat, widths = c(0.15, 1, .25, 1), heights = c(1,1,1,1))
# layout.show(lo)

for(j in 1:4)   plotLabel(paste0("Cluster ", j), srt = 90)


# plot
indices <- match(id4, id)

for(j in 1:4) {
  data_res_j <- pred4$Predictions[[indices[j]]]
  
  PlotTS_Flex(data = data_res_j,
              IDcol = "ID",
              ID = id4[j], # Subject number, here fixed
              variable = "Happy", # Variable
              variable2 = "Happy_hat",
              layout = FALSE,
              title = FALSE,
              ylab = TRUE,
              xlim = c(1,70),
              trend=FALSE,
              resLegend = c(FALSE, TRUE, FALSE, FALSE)[j])
  plotBV_flex(data=data_res_j,
              IDcol = "ID",
              ID = id4[j], # Subject number, here fixed
              variable1 = "Happy_hat",
              variable2 = "Happy", # Variable
              lag=FALSE,
              title=FALSE,
              para=FALSE,
              fit= FALSE,
              diag = TRUE,
              R2=TRUE)
}

dev.off()



###### mean anxiety cluster 2 four cluster model ###### 
# example of low
# ex 1
four_c2 <- affect4[affect4$Cluster4 == 2,]
mean(four_c2$Anxious, na.rm = TRUE)


four_c2_anxietyr2 <- df4_plot[df4_plot$Variables == 5 & df4_plot$Clusters == 2, 1]
boxplot.stats(four_c2_anxietyr2)$stats

# ex 2
mean(four_c2$Stressed, na.rm = TRUE)

four_c2_stressr2 <- df4_plot[df4_plot$Variables == 7 & df4_plot$Clusters == 2, 1]
boxplot.stats(four_c2_stressr2)$stats



# example of high
# ex 3
four_c1 <- affect4[affect4$Cluster4 == 1,]
mean(four_c2$Sad, na.rm = TRUE)

four_c1_sad <- df4_plot[df4_plot$Variables == 3 & df4_plot$Clusters == 1, 1]
boxplot.stats(four_c1_sad)$stats

# ex 4
four_c3 <- affect4[affect4$Cluster4 == 3,]
mean(four_c3$Sad, na.rm = TRUE)

four_c3_sad <- df4_plot[df4_plot$Variables == 3 & df4_plot$Clusters == 3, 1]
boxplot.stats(four_c3_sad)$stats


residual_v <- c(12.41935, 0.03225707155, 0.00330420734, 0.06042089675,
                27.27605, 0.0705222811, 0.0329697880, 0.1319959783,
                14.24055, 0.166146556, 0.111047965, 0.389560063,
                30.90565, 0.1526054926, 0.0334452887, 0.2957418391)

residual_mat <- matrix(round(residual_v, 3), nrow = 4, 4, byrow = TRUE)

residual_df <- as.data.frame(residual_mat)







### descriptives for 4 cluster model


# Combine all ranked tables into one
ranked_table <- bind_rows(
  lapply(names(ranked4), function(var) {
    df <- ranked4[[var]][, c("Rank", "Cluster4", "mean")]
    df$Variable <- var
    df
  })
)

# Reorder columns for APA style
ranked_table <- ranked_table %>%
  select(Variable, Rank, Cluster4, mean) %>%
  arrange(Variable, Rank)

# Print APA-style table using xtable
library(xtable)
print(
  xtable(ranked_table, 
         caption = "Ranked means for each variable by cluster (4-cluster solution).",
         label = "tab:ranked_means",
         digits = c(0, 0, 0, 0, 2)),
  include.rownames = FALSE,
  caption.placement = "top",
  align = c("l", "l", "c", "c", "c"),
  hline.after = c(-1,0,nrow(ranked_table))
)



# ...existing code...

# Calculate mean PA and mean NA for each cluster
descriptives4$mean_PA <- rowMeans(descriptives4[, c("Happy_mean", "Relaxed_mean")], na.rm = TRUE)
descriptives4$mean_NA <- rowMeans(descriptives4[, c("Sad_mean", "Angry_mean", "Anxious_mean", "Depressed_mean", "Stressed_mean")], na.rm = TRUE)

# Make concise table
concise_table <- descriptives4 %>%
  select(Cluster4, mean_PA, mean_NA)

# Print APA-style table
library(xtable)
print(
  xtable(concise_table, 
         caption = "Mean Positive Affect (PA) and Negative Affect (NA) by cluster (4-cluster solution).",
         label = "tab:mean_pa_na",
         digits = c(0, 0, 2, 2)),
  include.rownames = FALSE,
  caption.placement = "top",
  align = c("l", "c", "c", "c"),
  hline.after = c(-1,0,nrow(concise_table))
)
# ...existing code...





# ...existing code...

# Calculate mean PA and mean NA for each cluster
descriptives4$mean_PA <- rowMeans(descriptives4[, c("Happy_mean", "Relaxed_mean")], na.rm = TRUE)
descriptives4$mean_NA <- rowMeans(descriptives4[, c("Sad_mean", "Angry_mean", "Anxious_mean", "Depressed_mean", "Stressed_mean")], na.rm = TRUE)

# Make concise table and order by mean_PA descending
concise_table <- descriptives4 %>%
  select(Cluster4, mean_PA, mean_NA) %>%
  arrange(desc(mean_PA))

# Print APA-style table
library(xtable)
print(
  xtable(concise_table, 
         caption = "Mean Positive Affect (PA) and Negative Affect (NA) by cluster (ordered by PA, 4-cluster solution).",
         label = "tab:mean_pa_na",
         digits = c(0, 0, 2, 2)),
  include.rownames = FALSE,
  caption.placement = "top",
  align = c("l", "c", "c", "c"),
  hline.after = c(-1,0,nrow(concise_table))
)
# ...existing code...





# ...existing code...

# Table of PA variables (Happy, Relaxed), ordered by mean PA
pa_table <- descriptives4 %>%
  select(Cluster4, Happy = Happy_mean, Relaxed = Relaxed_mean) %>%
  mutate(mean_PA = rowMeans(select(., Happy, Relaxed), na.rm = TRUE)) %>%
  arrange(desc(mean_PA)) %>%
  select(Cluster4, Happy, Relaxed) # Drop mean_PA from output

# Print APA-style table
library(xtable)
print(
  xtable(pa_table,
         caption = "Cluster means for Happy and Relaxed (ordered by mean PA, 4-cluster solution).",
         label = "tab:pa_means",
         digits = c(0, 0, 2, 2)),
  include.rownames = FALSE,
  caption.placement = "top",
  align = c("l", "c", "c", "c"),
  hline.after = c(-1,0,nrow(pa_table))
)
# ...existing code...



# ...existing code...

# Table of NA variables (Sad, Angry, Anxious, Depressed, Stressed), ordered by mean NA
na_vars <- c("Sad", "Angry", "Anxious", "Depressed", "Stressed")
na_table <- descriptives4 %>%
  select(Cluster4, 
         Sad = Sad_mean, 
         Angry = Angry_mean, 
         Anxious = Anxious_mean, 
         Depressed = Depressed_mean, 
         Stressed = Stressed_mean) %>%
  mutate(mean_NA = rowMeans(select(., Sad, Angry, Anxious, Depressed, Stressed), na.rm = TRUE)) %>%
  arrange(desc(mean_NA)) %>%
  select(Cluster4, Sad, Angry, Anxious, Depressed, Stressed) # Drop mean_NA from output

# Print APA-style table
library(xtable)
print(
  xtable(na_table,
         caption = "Cluster means for Negative Affect variables (ordered by mean NA, 4-cluster solution).",
         label = "tab:na_means",
         digits = c(0, 0, 2, 2, 2, 2, 2)),
  include.rownames = FALSE,
  caption.placement = "top",
  align = c("l", "c", "c", "c", "c", "c", "c"),
  hline.after = c(-1,0,nrow(na_table))
)
# ...existing code...




length(unique(affect4$PID[affect4$Cluster4 == 4]))



library(mnet)
head(dataKoval13)
dataKoval13 <- dataKoval13

above16 <- cesd$PpID[cesd$original >= 16]




# where did dep and not dep people end up 

head(class4)
head(cesd)
library(dplyr)

depndep <- left_join(class4, cesd, by = c("id" = "PpID")) %>%
  select(id, classification, original)

group_low <- depndep %>% filter(original <= 16)
group_high <- depndep %>% filter(original > 16)

table(group_low$classification)
table(group_high$classification)


group_low27 <- depndep %>% filter(original <= 27)
group_high27 <- depndep %>% filter(original > 27)

table(group_low27$classification)
table(group_high27$classification)





##################################################################
#                13. CLASSIFICATION CROSS MATRIX                 #
##################################################################
# ----- extract classification of best model for all k -----
best_runs <- getBestModel(out_seed1)

# Store classification data.frames for each k (2:6)
class_list <- list()
for (k in 2:6) {
  best_idx <- best_runs[k]
  clas <- t(out_seed1$All_Models[[k]][[1]][[best_idx]]$Classification)
  class_list[[k]] <- data.frame(
    id = as.numeric(rownames(clas)),
    classification = paste0("Cluster ", clas[, 1])
  )
}

# Generate all pairwise cross-classification matrices
cross_matrices <- list()
for (k1 in 2:6) {
  for (k2 in (k1+1):6) {
    merged_classes <- merge(class_list[[k1]], class_list[[k2]], by = "id", suffixes = c(paste0("_", k1), paste0("_", k2)))
    cross_mat <- addmargins(table(merged_classes[[paste0("classification_", k1)]], merged_classes[[paste0("classification_", k2)]]))
    cross_matrices[[paste0(k1, "vs", k2)]] <- cross_mat
    
  }
}

cross_matrices["4vs5"]




plotBV_flex(data=data_res_j,
            IDcol = "ID",
            ID = id4[j], # Subject number, here fixed
            variable1 = "Happy_hat",
            variable2 = "Happy", # Variable
            lag=FALSE,
            title=FALSE,
            para=FALSE,
            fit= FALSE,
            diag = TRUE,
            R2=TRUE)

head(clas4)
class(clas4)



pdf("/Users/Lexi/Desktop/internship/4_ plots/repo_ res4.pdf", width=14, height=14)

lmat <- matrix(1:16, 4, 4, byrow = TRUE)
lo <- layout(lmat, widths = c(1,1,1,1), heights = c(1,1,1,1))

variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")
nClusters <- max(clas4) # Number of clusters
ids_by_cluster <- lapply(1:nClusters, function(cl) as.numeric(rownames(clas4)[clas4[, 1] == cl]))


for (var in variables) {
  for (cl in 1:nClusters) {
    ids <- ids_by_cluster[[cl]]
    for (id_val in ids) {
      idx <- which(id == as.numeric(id_val))
      data_res_j <- pred4$Predictions[[idx]]
      plotBV_flex(
        data = data_res_j,
        IDcol = "ID",
        ID = as.numeric(id_val),
        variable1 = paste0(var, "_hat"),
        variable2 = var,
        lag = FALSE,
        title = TRUE,
        para = FALSE,
        fit = FALSE,
        diag = TRUE,
        R2 = TRUE
      )
    }
  }
}

dev.off()



RColorBrewer::brewer.pal(4, "Set2")




# ...existing code...

# Add a 'point_col' argument to plotBV_flex
plotBV_flex <- function(data,
                        IDcol,
                        ID,
                        variable1,
                        variable2,
                        lag = FALSE,
                        title = TRUE,
                        fit = TRUE,
                        para = FALSE,
                        diag = FALSE,
                        xlim = NULL,
                        R2=FALSE,
                        point_col = "black") {  # <-- new argument
  
  # Subset data
  data_ss <- data[data[[IDcol]] == ID, ]
  Nt_ss <- nrow(data_ss)
  if(is.null(xlim)) xlim <- c(1, Nt_ss)
  
  # Canvas
  par(mar=c(4,4,2,1))
  plot.new()
  plot.window(xlim=c(0, 100), ylim=c(0, 100))
  axis(1)
  axis(2, las=2)
  grid()
  if(diag) abline(0, 1, col="grey")
  if(lag) {
    title(xlab = bquote(.(variable1)[t-1]),
          ylab = bquote(.(variable2)[t]),
          line = 2.5)
  } else {
    title(xlab=variable1, ylab=variable2, line=2.5)
  }
  if(title) title(main=paste0("Person ", ID), font.main=1)
  
  # Plot Data
  if(lag) {
    x1 <- data_ss[-Nt_ss, variable1]
    x2 <- data_ss[-1, variable2]
  } else {
    x1 <- data_ss[, variable1]
    x2 <- data_ss[, variable2]
  }
  points(x1, x2, pch=20, cex=1, col=point_col)  # <-- use color
  lm_obj <- lm(x2 ~ x1)
  if(fit) abline(lm_obj, lwd=2, col=point_col)  # <-- use color
  
  # Add regression results
  if(para) text(80, 7, paste0("a = ",
                              round(coef(lm_obj)[1], 2),
                              ", b = ",
                              round(coef(lm_obj)[2], 2)),
                col=point_col)
  
  # ADD R2
  if(R2) {
    r2 <- cor(x1, x2, use="complete.obs")^2
    r2 <- round(r2, 2)
    text(20, 80, bquote(R^2 == .(r2)))
  }
  
} # eoF

# ...existing code...
pdf("/Users/Lexi/Desktop/internship/4_ plots/repo_ res4.pdf", width=14, height=14)

lmat <- matrix(1:16, 4, 4, byrow = TRUE)
lo <- layout(lmat, widths = c(1,1,1,1), heights = c(1,1,1,1))

variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")
nClusters <- max(clas4) # Number of clusters
ids_by_cluster <- lapply(1:nClusters, function(cl) as.numeric(rownames(clas4)[clas4[, 1] == cl]))

cluster_cols <- RColorBrewer::brewer.pal(nClusters, "Set2")  # Get colors

for (var in variables) {
  for (cl in 1:nClusters) {
    ids <- ids_by_cluster[[cl]]
    for (id_val in ids) {
      idx <- which(id == as.numeric(id_val))
      data_res_j <- pred4$Predictions[[idx]]
      plotBV_flex(
        data = data_res_j,
        IDcol = "ID",
        ID = as.numeric(id_val),
        variable1 = paste0(var, "_hat"),
        variable2 = var,
        lag = FALSE,
        title = FALSE,
        para = FALSE,
        fit = FALSE,
        diag = TRUE,
        R2 = TRUE,
        point_col = cluster_cols[cl]  # Pass color for cluster
      )
    }
  }
}

dev.off()





## add new before each variables

pdf("/Users/Lexi/Desktop/internship/4_ plots/repo_ res4.pdf", width=14, height=14)

variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")
nClusters <- max(clas4) # Number of clusters
ids_by_cluster <- lapply(1:nClusters, function(cl) as.numeric(rownames(clas4)[clas4[, 1] == cl]))

for (var in variables) {
  lmat <- matrix(1:16, 4, 4, byrow = TRUE)
  layout(lmat, widths = c(1,1,1,1), heights = c(1,1,1,1))
  for (cl in 1:nClusters) {
    ids <- ids_by_cluster[[cl]]
    for (id_val in ids) {
      idx <- which(id == as.numeric(id_val))
      data_res_j <- pred4$Predictions[[idx]]
      plotBV_flex(
        data = data_res_j,
        IDcol = "ID",
        ID = as.numeric(id_val),
        variable1 = paste0(var, "_hat"),
        variable2 = var,
        lag = FALSE,
        title = TRUE,
        para = FALSE,
        fit = FALSE,
        diag = TRUE,
        R2 = TRUE,
        point_col = cluster_cols[cl]  # color by cluster
      )
    }
  }
}

dev.off()

pdf("/Users/Lexi/Desktop/internship/4_ plots/repo_ res4.pdf", width = 14, height = 14)

variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")
nClusters <- max(clas4)
ids_by_cluster <- lapply(1:nClusters, function(cl) as.numeric(rownames(clas4)[clas4[, 1] == cl]))
cluster_cols <- RColorBrewer::brewer.pal(nClusters, "Set2")

# Helper function to plot a page of BV plots
plot_one_page <- function(ids, cols, var, show_label = FALSE) {
  layout(matrix(1:25, 5, 5, byrow = TRUE))
  
  if (show_label) {
    plot.new()
    text(0.5, 0.5, var, cex = 2, font = 2)
  }
  
  for (i in seq_along(ids)) {
    idx <- which(id == ids[i])
    data_res_j <- pred4$Predictions[[idx]]
    plotBV_flex(
      data = data_res_j,
      IDcol = "ID",
      ID = ids[i],
      variable1 = paste0(var, "_hat"),
      variable2 = var,
      lag = FALSE,
      title = TRUE,
      para = FALSE,
      fit = FALSE,
      diag = TRUE,
      R2 = TRUE,
      point_col = cols[i]
    )
  }
  
  # Fill remaining plots with empty panels
  n_filled <- length(ids) + as.integer(show_label)
  for (j in seq_len(25 - n_filled)) {
    plot.new()
  }
}

# Loop through each variable and generate plots
for (var in variables) {
  all_ids <- unlist(ids_by_cluster)
  all_cols <- rep(cluster_cols, times = sapply(ids_by_cluster, length))
  n_ids <- length(all_ids)
  
  # Page 1: label + up to 24 plots
  if (n_ids > 0) {
    plot_one_page(all_ids[1:min(24, n_ids)], all_cols[1:min(24, n_ids)], var, show_label = TRUE)
  }
  
  # Additional pages: 25 plots each
  if (n_ids > 24) {
    remaining_ids <- all_ids[-(1:24)]
    remaining_cols <- all_cols[-(1:24)]
    pages <- split(seq_along(remaining_ids), ceiling(seq_along(remaining_ids) / 25))
    for (pg in pages) {
      plot_one_page(remaining_ids[pg], remaining_cols[pg], var, show_label = FALSE)
    }
  }
}

dev.off()





cols_k4 <- RColorBrewer::brewer.pal(4, "Set2")

boxplot(values ~ Clusters + Variables,  # Group by variables, then clusters
        data = df4_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k4, times=4), ylim=c(0, .8))
grid()
boxplot(values ~ Clusters + Variables,  # Group by variables, then clusters
        data = df4_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k4, times=4), ylim=c(0, .8), add = TRUE)

axis(1, 1:28, rep(variables, each=4), las=2)
axis(2, las=2)
title(ylab="Proportion of Explained Variance")
title("Four Cluster Model")


rep(cols_k4, times=4)




# ...existing code...

cols_k4 <- RColorBrewer::brewer.pal(4, "Set2")
nClusters <- 4
nVars <- length(variables)

# Calculate positions for boxes with gaps
gap <- 0.7  # gap size between variable groups
box_positions <- unlist(lapply(0:(nVars-1), function(i) i*(nClusters+gap) + 1:nClusters))

# Plot boxplots with manual positions
boxplot(values ~ Clusters + Variables,
        data = df4_plot, las=2, axes=FALSE, xlab="", ylab="",
        col=rep(cols_k4, times=nVars),
        ylim=c(0, .8),
        at=box_positions)
grid()
boxplot(values ~ Clusters + Variables,
        data = df4_plot, las=2, axes=FALSE, xlab="", ylab="",
        col=rep(cols_k4, times=nVars),
        ylim=c(0, .8),
        add=TRUE,
        at=box_positions)

# Set axis labels: one per variable, centered
var_centers <- sapply(0:(nVars-1), function(i) mean(box_positions[(i*nClusters+1):((i+1)*nClusters)]))
axis(1, at=var_centers, labels=variables, las=2)
axis(2, las=2)
title(ylab="Proportion of Explained Variance")
title("Four Cluster Model")

# ...existing code...



library(dplyr)

# Calculate median for each variable and cluster
median_table <- df4_plot %>%
  group_by(Variables, Clusters) %>%
  summarise(median_value = median(values, na.rm = TRUE)) %>%
  ungroup()

max(median_table$median_value, na.rm = TRUE) 
min(median_table$median_value, na.rm = TRUE)

overall_median <- median(df4_plot$values, na.rm = TRUE)


# -------- Variables Together: Two Cluster Model --------
cols_k2 <- RColorBrewer::brewer.pal(2, "Set2")
cols_k2 <- cols_k2[1:2]
nClusters <- 2
nVars <- length(variables)

gap <- 0.7
box_positions <- unlist(lapply(0:(nVars-1), function(i) i*(nClusters+gap) + 1:nClusters))

boxplot(values ~ Clusters + Variables,
        data = df2_plot, las=2, axes=FALSE, xlab="", ylab="",
        col=rep(cols_k2, times=nVars),
        ylim=c(0, .8),
        at=box_positions)
grid()
boxplot(values ~ Clusters + Variables,
        data = df2_plot, las=2, axes=FALSE, xlab="", ylab="",
        col=rep(cols_k2, times=nVars),
        ylim=c(0, .8),
        add=TRUE,
        at=box_positions)

var_centers <- sapply(0:(nVars-1), function(i) mean(box_positions[(i*nClusters+1):((i+1)*nClusters)]))
axis(1, at=var_centers, labels=variables, las=2)
axis(2, las=2)
title(ylab="Proportion of Explained Variance")
title("Two Cluster Model")

legend(x = max(box_positions) - 1.5,
       y = 1.05,
       legend = paste("Cluster", 1:nClusters),
       fill = cols_k2,
       horiz = FALSE,
       cex = 0.9,
       xpd = TRUE,
       bty = "n")


# -------- Variables Together: Three Cluster Model --------
cols_k3 <- RColorBrewer::brewer.pal(3, "Set2")
nClusters <- 3
nVars <- length(variables)

gap <- 0.7
box_positions <- unlist(lapply(0:(nVars-1), function(i) i*(nClusters+gap) + 1:nClusters))

boxplot(values ~ Clusters + Variables,
        data = df3_plot, las=2, axes=FALSE, xlab="", ylab="",
        col=rep(cols_k3, times=nVars),
        ylim=c(0, .8),
        at=box_positions)
grid()
boxplot(values ~ Clusters + Variables,
        data = df3_plot, las=2, axes=FALSE, xlab="", ylab="",
        col=rep(cols_k3, times=nVars),
        ylim=c(0, .8),
        add=TRUE,
        at=box_positions)

var_centers <- sapply(0:(nVars-1), function(i) mean(box_positions[(i*nClusters+1):((i+1)*nClusters)]))
axis(1, at=var_centers, labels=variables, las=2)
axis(2, las=2)
title(ylab="Proportion of Explained Variance")
title("Three Cluster Model")

legend(x = max(box_positions) - 2,
       y = 1.05,
       legend = paste("Cluster", 1:nClusters),
       fill = cols_k3,
       horiz = FALSE,
       cex = 0.9,
       xpd = TRUE,
       bty = "n")




pdf("/Users/Lexi/Desktop/internship/4_ plots/repo_ ts4.pdf", width = 16, height = 14)

variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")
nClusters <- max(clas4)
ids_by_cluster <- lapply(1:nClusters, function(cl) as.numeric(rownames(clas4)[clas4[, 1] == cl]))
cluster_cols <- RColorBrewer::brewer.pal(nClusters, "Set2")

plot_one_page_ts <- function(ids, cols, var, show_label = FALSE) {
  layout(matrix(1:30, 5, 6, byrow = TRUE))
  
  if (show_label) {
    plotLabel(var, cex = 2)
  }
  
  for (i in seq_along(ids)) {
    idx <- which(id == ids[i])
    data_res_j <- pred4$Predictions[[idx]]
    PlotTS_Flex(
      data = data_res_j,
      IDcol = "ID",
      ID = ids[i],
      variable = var,
      variable2 = paste0(var, "_hat"),
      layout = FALSE,
      title = TRUE,
      ylab = TRUE,
      xlim = c(1, nrow(data_res_j)),
      trend = FALSE,
      resLegend = TRUE
    )
  }
  
  n_filled <- length(ids) + as.integer(show_label)
  for (j in seq_len(25 - n_filled)) {
    plot.new()
  }
}

for (var in variables) {
  all_ids <- unlist(ids_by_cluster)
  all_cols <- rep(cluster_cols, times = sapply(ids_by_cluster, length))
  n_ids <- length(all_ids)
  
  # Page 1: label + up to 24 plots
  if (n_ids > 0) {
    plot_one_page_ts(all_ids[1:min(24, n_ids)], all_cols[1:min(24, n_ids)], var, show_label = FALSE)
  }
  
  # Additional pages: 25 plots each
  if (n_ids > 24) {
    remaining_ids <- all_ids[-(1:24)]
    remaining_cols <- all_cols[-(1:24)]
    pages <- split(seq_along(remaining_ids), ceiling(seq_along(remaining_ids) / 25))
    for (pg in pages) {
      plot_one_page_ts(remaining_ids[pg], remaining_cols[pg], var, show_label = FALSE)
    }
  }
}

dev.off()



PlotTS_Flex <- function(data,
                        IDcol, # column with subject ID
                        ID, # subject ID
                        variable, # variable colname
                        variable2 = NULL,
                        layout = FALSE,
                        title = TRUE,
                        xlab = FALSE,
                        ylab = FALSE,
                        xlim = NULL,
                        trend = TRUE,
                        resLegend = FALSE,
                        col_pred = "orange") {  # <-- new argument
  
  # Subset data
  data_ss <- data[data[[IDcol]] == ID, ] # get data of specified participant
  Nt_ss <- nrow(data_ss)
  if(is.null(xlim)) xlim <- c(1, Nt_ss) # set xlim if wasn't defined
  
  # Layout
  if(layout) layout(matrix(1:2, ncol=2), widths = c(1, .35)) # 2 plots width 1 & .35
  
  # LinePlot
  par(mar=c(4,4,2,1))
  plot.new()
  plot.window(xlim=xlim, ylim=c(0, 100))
  if(ylab) title(ylab=variable, line=2.25)
  if(xlab) title(xlab='Time', line=2.25)
  axis(1)
  axis(2, las=2)
  grid()
  # Plot Data
  lines(data_ss[, variable])
  
  # Second variable (predictions)
  if(!is.null(variable2)) lines(data_ss[, variable2], col=col_pred)
  
  if(trend) {
    time <- 1:nrow(data_ss)
    lm_obj <- lm(data_ss[, variable]~time)
    abline(lm_obj, lwd=1, col="black", lty=2)
  }
  
  if(resLegend) legend("topright", legend=c("Data", "Predictions"), bty="n", text.col=c("black", col_pred))
  
  if(!is.null(title)) {
    if(title==TRUE) title(main=paste0(variable), font.main=1)
    if(class(title) == "character") title(title, , font.main=1)
  }
  
  # Marginal
  par(mar=c(4,0,2,2))
  hist_data <- hist(data_ss[, variable], plot = FALSE, breaks=seq(0, 100, length=20))
  barplot(hist_data$counts,
          horiz = TRUE,  # Horizontal bars
          names.arg = NULL,
          axes=FALSE)
  x_seq <- seq(0, 100, length=1000)
  gauss_den <- dnorm(x_seq,
                     mean = mean(data_ss[, variable], na.rm = TRUE),
                     sd = sd(data_ss[, variable], na.rm = TRUE))
  scaled_den <- (gauss_den/max(gauss_den) ) * max(hist_data$counts)
  lines(scaled_den, seq(0, 24, length=1000), col="black")
}



pdf("/Users/Lexi/Desktop/internship/4_ plots/repo_ ts4.pdf", width = 16, height = 14)

variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")
nClusters <- max(clas4)
ids_by_cluster <- lapply(1:nClusters, function(cl) as.numeric(rownames(clas4)[clas4[, 1] == cl]))
cluster_cols <- RColorBrewer::brewer.pal(nClusters, "Set2")

plot_one_page_ts <- function(ids, var, show_label = FALSE) {
  layout(matrix(1:30, 5, 6, byrow = TRUE))
  
  if (show_label) {
    plotLabel(var, cex = 2)
  }
  
  for (i in seq_along(ids)) {
    idx <- which(id == ids[i])
    data_res_j <- pred4$Predictions[[idx]]
    # Find cluster for this ID
    cluster_i <- clas4[as.character(ids[i]), 1]
    pred_col <- cluster_cols[cluster_i]
    PlotTS_Flex(
      data = data_res_j,
      IDcol = "ID",
      ID = ids[i],
      variable = var,
      variable2 = paste0(var, "_hat"),
      layout = FALSE,
      title = TRUE,
      ylab = TRUE,
      xlim = c(1, nrow(data_res_j)),
      trend = FALSE,
      resLegend = TRUE,
      col_pred = pred_col   # pass color for prediction line
    )
  }
  
  n_filled <- length(ids) + as.integer(show_label)
  for (j in seq_len(30 - n_filled)) {  # fill up to 30 panels
    plot.new()
  }
}

for (var in variables) {
  all_ids <- unlist(ids_by_cluster)
  n_ids <- length(all_ids)
  
  # Page 1: label + up to 30 plots
  if (n_ids > 0) {
    plot_one_page_ts(all_ids[1:min(30, n_ids)], var, show_label = FALSE)
  }
  
  # Additional pages: 30 plots each
  if (n_ids > 30) {
    remaining_ids <- all_ids[-(1:30)]
    pages <- split(seq_along(remaining_ids), ceiling(seq_along(remaining_ids) / 30))
    for (pg in pages) {
      plot_one_page_ts(remaining_ids[pg], var, show_label = FALSE)
    }
  }
}

dev.off()





pdf("/Users/Lexi/Desktop/internship/4_ plots/repo_ combined.pdf", width = 14, height = 14)

variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")
nClusters <- max(clas4)
ids_by_cluster <- lapply(1:nClusters, function(cl) as.numeric(rownames(clas4)[clas4[, 1] == cl]))
cluster_cols <- RColorBrewer::brewer.pal(nClusters, "Set2")

plot_one_page_combined <- function(ids, var) {
  layout(matrix(1:30, 5, 6, byrow = TRUE), widths = c(1, 0.3, 1, 1, 0.3, 1))
  
  for (i in seq_along(ids)) {
    idx <- which(id == ids[i])
    data_res_j <- pred4$Predictions[[idx]]
    cluster_i <- clas4[as.character(ids[i]), 1]
    pred_col <- cluster_cols[cluster_i]
    
    # 1. TS + marginal (PlotTS_Flex does both)
    PlotTS_Flex(
      data = data_res_j,
      IDcol = "ID",
      ID = ids[i],
      variable = var,
      variable2 = paste0(var, "_hat"),
      layout = FALSE,
      title = TRUE,
      ylab = TRUE,
      xlim = c(1, nrow(data_res_j)),
      trend = FALSE,
      resLegend = TRUE,
      col_pred = pred_col
    )
    
    # 2. BV plot
    plotBV_flex(
      data = data_res_j,
      IDcol = "ID",
      ID = ids[i],
      variable1 = paste0(var, "_hat"),
      variable2 = var,
      lag = FALSE,
      title = TRUE,
      para = FALSE,
      fit = FALSE,
      diag = TRUE,
      R2 = TRUE,
      point_col = pred_col
    )
  }
  
  # Fill remaining panels if less than 10 participants (30 panels)
  n_filled <- length(ids) * 3
  for (j in seq_len(30 - n_filled)) {
    plot.new()
  }
}

for (var in variables) {
  all_ids <- unlist(ids_by_cluster)
  n_ids <- length(all_ids)
  
  # Each page: up to 10 participants (10*3=30 panels)
  ids_per_page <- 10
  pages <- split(all_ids, ceiling(seq_along(all_ids) / ids_per_page))
  for (pg in pages) {
    plot_one_page_combined(pg, var)
  }
}

dev.off()




# ID 162 (Relaxed Cluster 2)
# Stressed, ID 504, Cluster 3
# Relaxed, ID 181, Cluster 3
# Happy, ID 13, Cluster 4
# Angry, ID 447, Cluster 2


plotLabel("ID 162 (Relaxed Cluster 2)")
plotLabel("ID 504 (Stressed Cluster 3)")
plotLabel("ID 181 (Relaxed Cluster 3)")
plotLabel("ID 13 (Happy Cluster 4)")
plotLabel("ID 447 (Angry Cluster 2)")


pdf("/Users/Lexi/Desktop/internship/4_ plots/o_pred_examples.pdf", width=8, height = 10)

id5 <- c(162, 504, 181, 13, 447)
indices5 <- match(id5, id)

vb1 <- c("Relaxed", "Stressed", "Relaxed", "Happy", "Angry")
vb2 <- c("Relaxed_hat", "Stressed_hat", "Relaxed_hat", "Happy_hat", "Angry_hat")

cols_pred <- c("#FC8D62","#8DA0CB", "#8DA0CB","#E78AC3","#FC8D62")



# Layout
lmat <- matrix(6:20, 5, 3, byrow = TRUE)
lmat <- cbind(1:5, lmat)
lo <- layout(lmat, widths = c(0.15, 1, .25, 1), heights = c(1,1,1,1,1))
# layout.show(lo)

plotLabel("ID 162 (Cluster 2)", srt = 90)
plotLabel("ID 504 (Cluster 3)", srt = 90)
plotLabel("ID 181 (Cluster 3)", srt = 90)
plotLabel("ID 13 (Cluster 4)", srt = 90)
plotLabel("ID 447 (Cluster 2)", srt = 90)

# plot
for(j in 1:5) {
  data_res_j <- pred4$Predictions[[indices5[j]]]
  
  print(j)
  PlotTS_Flex(data = data_res_j,
              IDcol = "ID",
              ID = id5[j], # Subject number, here fixed
              variable = vb1[j],
              variable2 = vb2[j],
              layout = FALSE,
              title = FALSE,
              ylab = TRUE,
              xlim = c(1,70),
              trend=FALSE,
              resLegend = c(FALSE, TRUE, FALSE, FALSE, FALSE)[j],
              col_pred = cols_pred[j])
  
  plotBV_flex(data=data_res_j,
              IDcol = "ID",
              ID = id5[j], # Subject number, here fixed
              variable1 = vb2[j],
              variable2 = vb1[j],
              lag=FALSE,
              title=FALSE,
              para=FALSE,
              fit= FALSE,
              diag = TRUE,
              R2=TRUE)
}

dev.off()




PlotTRENDS(out_seed1, 2, variables)
PlotTRENDS(out_seed1, 3, variables)
PlotTRENDS(out_seed1, 4, variables)
PlotTRENDS(out_seed1, 5, variables)
PlotTRENDS(out_seed1, 6, variables)

