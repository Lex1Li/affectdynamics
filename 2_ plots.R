# ----------------------------------------#
#                  PLOTS                  #
# ----------------------------------------#


######################################################
#                  1. LOAD PACKAGES                  #
######################################################
library(plyr)
library(ClusterVAR)
library(tidyverse)
library(xtable)
library(brms)
library(gridExtra)
library(ggtext)



source("/Users/Lexi/Desktop/internship/internship/Lexi_Helpers.R")
load("/Users/Lexi/Desktop/internship/1_ data/datalexi.RData")



####################################################
#                2. TS AND BV PLOTS                #
####################################################
sc = .95
pdf("/Users/Lexi/Desktop/internship/4_ plots/3_ 3randomparts.pdf", width = 8*sc, height = 8*sc)

# Layout
lmat <- matrix(4:12, 3, 3, byrow = TRUE)
lmat <- cbind(1:3, lmat)
lo <- layout(lmat, widths = c(0.15, 1, .25, 1), heights = c(1,1,1))
# layout.show(lo)

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
              xlab = TRUE,
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

dev.off()


# ----- not used # RANDOM ID FROM FOUR CLUSTER MODEL ----- #

lmat <- matrix(4:12, 3, 3, byrow = TRUE)
lmat <- cbind(1:3, lmat)
lo <- layout(lmat, widths = c(0.15, 1, .25, 1), heights = c(1,1,1))
layout.show(lo)

for(i in 1:3)   plotLabel(paste0("     Person ", LETTERS[i]), srt = 90)

id4 <- c(449, 559, 531, 518)

for(i in id4) {
  PlotTS_Flex(data = data,
              IDcol = "ID",
              ID = i, # Subject number, here fixed
              variable = "Happy", # Variable
              layout = FALSE,
              title = TRUE,
              ylab = TRUE,
              xlab = TRUE,
              xlim = c(1, 80))
  
  plotBV_flex(data = data,
              IDcol = "ID",
              ID = i, # Subject number, here fixed
              variable1 = "Happy",
              variable2 = "Happy", # Variable
              lag = TRUE,
              title = FALSE,
              para = TRUE)
}

# ----- #



######################################################################
#                3. TS PLOTS FOR ALL AFFECT AND ALL N                #
######################################################################
variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")

source("/Users/Lexi/Desktop/internship/2_ code/Lexi_Helpers.R")
pdf("/Users/Lexi/Desktop/internship/4_ plots/1_ timeseries.pdf", width=9, height=4)

# Loop over persons
for(j in 1:n) {
  # Make Layout
  lmat <- matrix(1:16, 2, 8, byrow = TRUE)
  lo <- layout(lmat, widths = c(1, .25, 1, .25, 1, .25, 1, .25), heights = c(1,1))
  
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



# ----- RANDOM ID FROM FOUR CLUSTER MODEL ----- # 
id4 <- c(449, 559, 531, 518)


pdf("/Users/Lexi/Desktop/internship/4_ plots/17_ id4.pdf", width=12, height=14)

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

for(i in 1:4)   plotLabel(paste0("Cluster ", i, " (ID = ", id4[i], ")"), ypos = 0.3)

dev.off()


# participant 39 for paper


# Make Layout
lmat <- matrix(1:16, 2, 8, byrow = TRUE)
lo <- layout(lmat, widths = c(1, .25, 1, .25, 1, .25, 1, .25), heights = c(1,1))

# ID panel
par(mar = c(0, 0, 0, 0))  # remove all margins
plot.new()
text(0.6, 0.6, paste("Participant ID: 39"), cex = 1, font = 2)
plot.new()

# Make plot for person j
for(i in 1:7)   PlotTS_Flex(data = data,
                            IDcol = "ID",
                            ID = 39, # Subject number, here fixed
                            variable = variables[i], # Variable
                            layout = FALSE,
                            title = TRUE,
                            ylab = TRUE,
                            xlab = TRUE,
                            xlim = c(1, 80))




######################################################################
#                4. BV PLOTS FOR ALL AFFECT AND ALL N                #
######################################################################
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



#################################################################################
#                5. PLOT VAR COEFFICIENTS AND CLUSTER DIFFERENCES               #
#################################################################################
var_names <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")


pdf("/Users/Lexi/Desktop/internship/4_ plots/8a_ cluster coefs.pdf", width=8, height=8)
PlotCOEFS(out_seed1, 2, labels = variables, small_difference_to_white = TRUE)
dev.off()


pdf("/Users/Lexi/Desktop/internship/4_ plots/8b_ cluster coefs.pdf", width=8, height=8)
PlotCOEFS(out_seed1, 3, labels = variables)
PlotCOEFS(out_seed1, 3, labels = variables, small_difference_to_white = TRUE)
dev.off()


pdf("/Users/Lexi/Desktop/internship/4_ plots/8c_ cluster coefs.pdf", width=12, height=12)
PlotCOEFS(out_seed1, 4, labels = variables)
PlotCOEFS(out_seed1, 4, labels = variables, small_difference_to_white = TRUE)
dev.off()


pdf("/Users/Lexi/Desktop/internship/4_ plots/8d_ cluster coefs.pdf", width=14, height=14)
PlotCOEFS(out_seed1, 5)
dev.off()


pdf("/Users/Lexi/Desktop/internship/4_ plots/8e_ cluster coefs.pdf", width=16, height=16)
PlotCOEFS(out_seed1, 6)
dev.off()


pdf("/Users/Lexi/Desktop/internship/4_ plots/9_ cluster coef white.pdf", width=16, height=16)
PlotCOEFS(out_seed1, 2, small_difference_to_white = TRUE, labels = variables)
PlotCOEFS(out_seed1, 3, small_difference_to_white = TRUE, labels = variables)
PlotCOEFS(out_seed1, 4, small_difference_to_white = TRUE, labels = variables)
PlotCOEFS(out_seed1, 5, small_difference_to_white = TRUE, labels = variables)
PlotCOEFS(out_seed1, 6, small_difference_to_white = TRUE, labels = variables)
dev.off()



pdf("/Users/Lexi/Desktop/internship/4_ plots/8_ cluster coef.pdf", width=16, height=16)
PlotCOEFS(out_seed1, 2, labels = variables)
PlotCOEFS(out_seed1, 3, labels = variables)
PlotCOEFS(out_seed1, 4, labels = variables)
PlotCOEFS(out_seed1, 5, labels = variables)
PlotCOEFS(out_seed1, 6, labels = variables)
dev.off()



###############################################################
#                5. COLLAPSED TIME SERIES PLOTS               #
###############################################################
pdf("/Users/Lexi/Desktop/internship/4_ plots/o_ts.pdf", width=12, height=14)

variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")
cluster_range <- 2:6

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
        title = paste0("Cluster ", clusterToPlot),
        remove_xlab = TRUE,
        opaqueness = 1
      )
      if (is.null(p)) p <- grid::nullGrob()
      plot_list[[length(plot_list) + 1]] <- p
    }
  }
  grid.arrange(grobs = plot_list, ncol = nClusters, nrow = length(variables))
}

dev.off()


# individual collapsed plot example
set.seed(443)
PlotCOLLAPSED(
  model = out_seed1,
  nClusters = 4,
  clusterToPlot = 4,
  data = affect,
  PID = "PID",
  timepoints = "OCCASION",
  variable = "Stressed",
  random = TRUE
)

set.seed(443)
PlotCOLLAPSED(
  model = out_seed1,
  nClusters = 4,
  clusterToPlot = 4,
  data = affect,
  PID = "PID",
  timepoints = "OCCASION",
  variable = "Sad",
  random = TRUE
)




# ----- RANDOM HIGHLIGHTED FOUR CLUSTER MODEL ----- #
pdf("/Users/Lexi/Desktop/internship/4_ plots/16_ highlighted timeseries.pdf", width=12, height=14)
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

dev.off()


# Selected id: 449
# Selected id: 559
# Selected id: 531
# Selected id: 518


# ----- # 

################################################################
#                6. PLOT CONTEMPORANEOUS EFFECTS               #
################################################################
variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")

pdf("/Users/Lexi/Desktop/internship/4_ plots/11a_ contemporaneous effect.pdf", width=6, height=6)
PlotCONTEMPORANEOUS(out_seed1, 2, labels = variables, small_difference_to_white = TRUE)
dev.off()

pdf("/Users/Lexi/Desktop/internship/4_ plots/o_compt2.pdf", width=6, height=6)
PlotCONTEMPORANEOUS(out_seed1, 2, labels = variables)
dev.off()


pdf("/Users/Lexi/Desktop/internship/4_ plots/11b_ contemporaneous effect.pdf", width=8, height=8)
PlotCONTEMPORANEOUS(out_seed1, 3, labels = variables)
dev.off()


pdf("/Users/Lexi/Desktop/internship/4_ plots/11c_ contemporaneous effect.pdf", width=12, height=12)
PlotCONTEMPORANEOUS(out_seed1, 4, labels = variables)
dev.off()


pdf("/Users/Lexi/Desktop/internship/4_ plots/11d_ contemporaneous effect.pdf", width=14, height=14)
PlotCONTEMPORANEOUS(out_seed1, 5, labels = variables)
dev.off()


pdf("/Users/Lexi/Desktop/internship/4_ plots/11e_ contemporaneous effect.pdf", width=16, height=16)
PlotCONTEMPORANEOUS(out_seed1, 6, labels = variables)
dev.off()


pdf("/Users/Lexi/Desktop/internship/4_ plots/11_ contemporaneous effect.pdf", width=16, height=16)
PlotCONTEMPORANEOUS(out_seed1, 2, labels = variables)
PlotCONTEMPORANEOUS(out_seed1, 3, labels = variables)
PlotCONTEMPORANEOUS(out_seed1, 4, labels = variables)
PlotCONTEMPORANEOUS(out_seed1, 5, labels = variables)
PlotCONTEMPORANEOUS(out_seed1, 6, labels = variables)
dev.off()

pdf("/Users/Lexi/Desktop/internship/4_ plots/12_ contemporaneous effect.pdf white.pdf", width=16, height=16)
PlotCONTEMPORANEOUS(out_seed1, 2, small_difference_to_white = TRUE, labels = variables)
PlotCONTEMPORANEOUS(out_seed1, 3, small_difference_to_white = TRUE, labels = variables)
PlotCONTEMPORANEOUS(out_seed1, 4, small_difference_to_white = TRUE, labels = variables)
PlotCONTEMPORANEOUS(out_seed1, 5, small_difference_to_white = TRUE, labels = variables)
PlotCONTEMPORANEOUS(out_seed1, 6, small_difference_to_white = TRUE, labels = variables)
dev.off()



##############################################
#                7. PLOT MEANS               #
##############################################
pdf("/Users/Lexi/Desktop/internship/4_ plots/13_ means.pdf", width=8, height=6)
PlotMEANS(model = out_seed1, nCluster = 2, variable = variables)
PlotMEANS(model = out_seed1, nCluster = 3, variable = variables)
PlotMEANS(model = out_seed1, nCluster = 4, variable = variables)
PlotMEANS(model = out_seed1, nCluster = 5, variable = variables)
PlotMEANS(model = out_seed1, nCluster = 6, variable = variables)
dev.off()



####################################################
#                7. PLOT TIME TRENDS               #
####################################################
pdf("/Users/Lexi/Desktop/internship/4_ plots/14_ trends.pdf", width=8, height=6)
PlotTRENDS(out_seed1, 2, variables)
PlotTRENDS(out_seed1, 3, variables)
PlotTRENDS(out_seed1, 4, variables)
PlotTRENDS(out_seed1, 5, variables)
PlotTRENDS(out_seed1, 6, variables)
dev.off()




############################################################
#                8. PLOTS OF 4 CLUSTER MODEL               #
############################################################

pdf("/Users/Lexi/Desktop/internship/4_ plots/15_ 4clustermodel.pdf", width=12, height=12)

# lagged correlations
PlotCOEFS(out_seed1, 4, labels = variables)
PlotCOEFS(out_seed1, 4, labels = variables, small_difference_to_white = TRUE)

# contemporaneous effects
PlotCONTEMPORANEOUS(out_seed1, 4, labels = variables)
PlotCONTEMPORANEOUS(out_seed1, 4, labels = variables, small_difference_to_white = TRUE)

# time series 
variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")
cluster_range <- 4

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
        title = "MSD",
        opaqueness = 0.8
      )
      if (is.null(p)) p <- grid::nullGrob()
      plot_list[[length(plot_list) + 1]] <- p
    }
  }
  grid.arrange(grobs = plot_list, ncol = nClusters, nrow = length(variables))
}

dev.off()





#########################################################
#                8. SCALED BIC AND NLL CI               #
#########################################################
BIC_robustness <- apply(BIC_1to8, 2, function(col) col / max(col))

BIC_mean <- apply(BIC_robustness, 1, mean)
BIC_se <- apply(BIC_robustness, 1, function(x) sd(x) / sqrt(30))

BIC_lower <- BIC_mean - qnorm(0.975) * BIC_se
BIC_higher <- BIC_mean + qnorm(0.975) * BIC_se


# ----- set layout -----
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


# ----- plot -----
points((1:K), BIC_mean, col="#E41A1C", pch=16, cex=1.25)
lines((1:K), BIC_mean, col="#E41A1C", lwd=1.5)

polygon(
  c(1:K, rev(1:K)),
  c(BIC_lower, rev(BIC_higher)),
  col = rgb(0.3, 0.6, 1, 0.2),
  border = NA
)




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





#########################################################################
#                8. PREDICTION VS OBSERVED 4 PARTICIPANTS               #
#########################################################################
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






#############################################################################
#                9. CLUSTER 2 CONTEMPORANEOUS INTERPRETATION                #
#############################################################################
cont4 <- contemporaneous1[[4]]
cont4[,,1]

c12 <- cont4[,,1] - cont4[,,2]
c32 <- cont4[,,3] - cont4[,,2]

layout(matrix(1:2, ncol=2))
PlotHEAT(c12, main = "Cluster 1 - 2", labels = variables, small_difference_to_white = T)
PlotHEAT(c32, main = "Cluster 3 - 2", labels = variables, small_difference_to_white = T)


layout(matrix(1:2, ncol=2))
PlotHEAT(c12, main = "Cluster 1 - 2", labels = variables, pcor = T)
PlotHEAT(c32, main = "Cluster 3 - 2", labels = variables, pcor = T)
