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
PlotCOEFS(out_seed1, 2, labels = variables)
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
pdf("/Users/Lexi/Desktop/internship/4_ plots/10_ collapsed timeseries.pdf", width=12, height=14)

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


# ----- # 

################################################################
#                6. PLOT CONTEMPORANEOUS EFFECTS               #
################################################################
variables <- c("Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed")

pdf("/Users/Lexi/Desktop/internship/4_ plots/11a_ contemporaneous effect.pdf", width=6, height=6)
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
