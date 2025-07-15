# ----------------------------------------------------#
#                  FITTING THE MODEL                  #
# ----------------------------------------------------#


######################################################
#                  1. LOAD PACKAGES                  #
######################################################
library(plyr)
library(ClusterVAR)
library(tidyverse)
library(xtable)
library(brms)
library(RColorBrewer) 
library(corpcor)
library(psych)



source("/Users/Lexi/Desktop/internship/2_ code/Lexi_Helpers.R")
load("/Users/Lexi/Desktop/internship/1_ data/datalexi.RData")



###################################################
#                  2. FIT MODELS                  #
###################################################
out_1to6_1 <- LCVAR(Data = data,
                  yVars = 5:11,
                  xContinuous = 4,
                  Beep = 3,
                  Day = 2,
                  ID = 1,
                  Lags = 1,
                  Clusters = 1:6,
                  Cores = 9,
                  Rand = 75,
                  RndSeed = 1) 

out2_1to6 <- LCVAR(Data = data,
                  yVars = 5:11,
                  xContinuous = 4,
                  Beep = 3,
                  Day = 2,
                  ID = 1,
                  Lags = 1,
                  Clusters = 1:6,
                  Cores = 9,
                  Rand = 75,
                  RndSeed = 2) 

out3_1to6 <- LCVAR(Data = data,
                   yVars = 5:11,
                   xContinuous = 4,
                   Beep = 3,
                   Day = 2,
                   ID = 1,
                   Lags = 1,
                   Clusters = 1:6,
                   Cores = 9,
                   Rand = 75,
                   RndSeed = 3) 

out4_1to6 <- LCVAR(Data = data,
                   yVars = 5:11,
                   xContinuous = 4,
                   Beep = 3,
                   Day = 2,
                   ID = 1,
                   Lags = 1,
                   Clusters = 1:6,
                   Cores = 9,
                   Rand = 75,
                   RndSeed = 4) 

out5_1to6 <- LCVAR(Data = data,
                   yVars = 5:11,
                   xContinuous = 4,
                   Beep = 3,
                   Day = 2,
                   ID = 1,
                   Lags = 1,
                   Clusters = 1:6,
                   Cores = 9,
                   Rand = 75,
                   RndSeed = 5) 

out6_1to6 <- LCVAR(Data = data,
                   yVars = 5:11,
                   xContinuous = 4,
                   Beep = 3,
                   Day = 2,
                   ID = 1,
                   Lags = 1,
                   Clusters = 1:6,
                   Cores = 9,
                   Rand = 75,
                   RndSeed = 6) 


### SAVE 6 MODEL RESULTS
# save(out1_1to6, out2_1to6, out3_1to6, out4_1to6, out5_1to6, out6_1to6, file = "/Users/Lexi/Desktop/internship/5_ results/1to6.RData")
load("/Users/Lexi/Desktop/internship/5_ results/1to6.RData")



###################################################
#                  3. PLOT MODELS                  #
###################################################
lmat <- matrix(1:6, 2, 3, byrow = TRUE)
lo <- layout(lmat, heights = c(1, 1), widths = c(1,1,1))
layout.show(lo)

PlotIC(out1_1to6, title = "Seed 1")
PlotIC(out2_1to6, title = "Seed 2")
PlotIC(out3_1to6, title = "Seed 3")
PlotIC(out4_1to6, title = "Seed 4")
PlotIC(out5_1to6, title = "Seed 5")
PlotIC(out6_1to6, title = "Seed 6")



###################################################################
#                  4. ROBUSTNESS 1 TO 8 CLUSTERS                  #
###################################################################
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


##########################################################
#                5. SAMPLE 3 RANDOM SEEDS                #
##########################################################
set.seed(13535099)
random_seed <- sample(1:1e7, 3)



#########################################################
#                6. RANDOM SEED 1, 2 & 3                #
#########################################################
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
load("/Users/Lexi/Desktop/internship/5_ results/3outs.RData")


######################################################
#                7. SEED 1 EVALUATION                #
######################################################
PlotScaled(out_seed1, title = "Random Seed 1")



####################################################################
#                8. SEED 1 STACKED PROPORTION PLOT                 #
####################################################################
# calculate proportions
sum1 <- summary(out_seed1, show = "GNL")
sum1_IC <- sum1$FunctionOutput$Proportions

classproportions1 <- matrix(NA, 6, 6)
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
  geom_text(
    aes(label = ifelse(is.na(Proportion), "", sprintf("%.2f", Proportion))),
    position = position_stack(vjust = 1), # slightly above each section
    hjust = 0.5,   # left align
    vjust = 1.7,   # top align
    color = "black",
    size = 5
  ) +
  labs(title = "Distribution of participants across models",
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


######################################################################################
#                9. EXTRACT THE TEMPORAL AND CONTEMPORANEOUS EFFECTS                 #
######################################################################################
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

 

##################################################################################
#                10. PLOT VAR COEFFICIENTS AND CLUSTER DIFFERENCES               #
##################################################################################

PlotCOEFS(out_seed1, 3)



##########################################################
#                11. EXTERNAL VALIDATION                 #
##########################################################
# ----- organise data -----
MEANS <- ddply(affect, .(PID), function(x) colMeans(x[, variables], na.rm=TRUE))
DEP <- cesd[,c(1,4)]

# ----- get classifications -----

l_coefs <- list()
K <- 6
for(k in 1:K) l_coefs[[k]] <- coef.ClusterVAR(out_seed1, Model = rep(1, k))



# ### --- 2 CLUSTERS --- ### 
# k <- 2
# cmemb2 <- as.numeric(l_coefs[[k]]$Classification)
# 
# external2 <- cbind(MEANS, DEP, as.factor(cmemb2))[, -9]
# colnames(external2) <- c("PID", "Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed", "CESD", "Cluster")
# head(external2)
# 
# 
# # ----- Test whether DASS-scales are different across clusters -----
# set.seed(12345)
# 
# ## Baseline
# # Fit models
# null <- brm(CESD ~ 1, data = external2, family = gaussian(), chains = 4, iter = 2000)
# alt <- brm(CESD ~ Cluster, data = external2, family = gaussian(), chains = 4, iter = 2000)
# 
# # Get BF
# bf2 <- bayes_factor(alt, null)
# round(bf2$bf)
# 
# ## With Covariates
# null_cov <- brm(CESD ~ 1 + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external2, family = gaussian(), chains = 4, iter = 2000)
# alt_cov <- brm(CESD ~ Cluster + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external2, family = gaussian(), chains = 4, iter = 2000)
# 
# # Get BF
# bf_cov2 <- bayes_factor(alt_cov, null_cov)
# # round(bf_cov2$bf)
# 
# 
# 
# ### --- 3 CLUSTERS --- ### 
# k <- 3
# cmemb3 <- as.numeric(l_coefs[[k]]$Classification)
# 
# external3 <- cbind(MEANS, DEP, as.factor(cmemb3))[, -9]
# colnames(external3) <- c("PID", "Happy", "Relaxed", "Sad", "Angry", "Anxious", "Depressed", "Stressed", "CESD", "Cluster")
# head(external3)
# 
# 
# # ----- Test whether DASS-scales are different across clusters -----
# set.seed(12345)
# 
# ## Baseline
# # Fit models
# null3 <- brm(CESD ~ 1, data = external3, family = gaussian(), chains = 4, iter = 2000)
# alt3 <- brm(CESD ~ Cluster, data = external3, family = gaussian(), chains = 4, iter = 2000)
# 
# # Get BF
# bf3 <- bayes_factor(alt3, null3)
# # round(bf3$bf)
# 
# ## With Covariates
# null_cov3 <- brm(CESD ~ 1 + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external3, family = gaussian(), chains = 4, iter = 2000)
# alt_cov3 <- brm(CESD ~ Cluster + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external3, family = gaussian(), chains = 4, iter = 2000)
# 
# # Get BF
# bf_cov3 <- bayes_factor(alt_cov3, null_cov3)
# # round(bf_cov3$bf)
# 



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
# round(bf4$bf)

## With Covariates
null_cov4 <- brm(CESD ~ 1 + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external4, family = gaussian(), chains = 4, iter = 2000)
alt_cov4 <- brm(CESD ~ Cluster + Happy + Relaxed + Sad + Angry + Anxious + Depressed + Stressed, data = external4, family = gaussian(), chains = 4, iter = 2000)

# Get BF
bf_cov4 <- bayes_factor(alt_cov4, null_cov4)
# round(bf_cov4$bf)


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



### mean cesd by cluster
# tapply(external2$CESD, external2$Cluster, mean, na.rm = TRUE)
# tapply(external3$CESD, external3$Cluster, mean, na.rm = TRUE)
tapply(external4$CESD, external4$Cluster, mean, na.rm = TRUE)






################################################
#                11. DISCUSSION                #
################################################

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


# ranked list
ranked4 <- lapply(variables, function(var) {
  df <- descriptives4 %>%
    select(Cluster4, 
           mean = paste0(var, "_mean"), 
           sd = paste0(var, "_sd")) %>%
    arrange(desc(mean)) %>%
    mutate(Rank = row_number()) %>%
    select(Rank, Cluster4, mean, sd)
  df
})

names(ranked4) <- variables

ranked4[["Happy"]]






####################################################
#                12. NLL ROBUSTNESS                #
####################################################

scaledNLL_1to8 <- as.data.frame(
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
  nll <- -model_sum$FunctionOutput$`log-likelihood`
  scaledNLL_1to8[ , i] <- nll/max(nll)
  
  print(paste0("Finished seed ", i))
}

# save(scaledNLL_1to8, file = "/Users/Lexi/Desktop/internship/5_ results/RobustnessNLL_ 1to8.RData")



##################################################################
#                13. CLASSIFICATION CROSS MATRIX                 #
##################################################################
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

library(xtable)
print(xtable(cross_mat, 
             caption = "Cross-classification matrix of cluster assignments between the 3-cluster and 4-cluster models.",
             label = "tab:crossmat",
             align = c("l", rep("c", ncol(cross_mat)))), 
      include.rownames = TRUE, 
      include.colnames = TRUE, 
      caption.placement = "top",
      hline.after = c(-1,0,nrow(cross_mat)))

print(
  xtable(
    cross_matrices[["4vs5"]],
    caption = "Cross-classification matrix of cluster assignments between the 4-cluster and 5-cluster models.",
    label = "tab:crossmat_4_5",
    align = c("l", rep("c", ncol(cross_matrices[["4vs5"]])))
  ),
  include.rownames = TRUE,
  include.colnames = TRUE,
  caption.placement = "top",
  hline.after = c(-1, 0, nrow(cross_matrices[["4vs5"]]))
)



###################################################################
#                14. ABSOLUTE CORRELATIONS MATRIX                 #
###################################################################
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




###############################################################
#                15. CORRELATE LAGGED EFFECTS                 #
###############################################################
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




################################################################################
#                15. R^2 FOR TWO THREE AND FOUR CLUSTER MODELS                 #
################################################################################
# #### TWO CLUSTER MODEL #### 
# # -------- Compute Predictions --------
# pred2 <- LCVARPred(object = out_seed1,
#                    data = data,
#                    k = 2)
# 
# # ---------- Compute R2 for each person and variable ----------
# c2_R2s <- lapply(pred2$Predictions, function(x) {
#   R2s <- c(cor(x$Happy_hat, x$Happy, use="complete.obs")^2,
#            cor(x$Relaxed_hat, x$Relaxed, use="complete.obs")^2,
#            cor(x$Sad_hat, x$Sad, use="complete.obs")^2,
#            cor(x$Angry_hat, x$Angry, use="complete.obs")^2,
#            cor(x$Anxious_hat, x$Anxious, use="complete.obs")^2,
#            cor(x$Depressed_hat, x$Depressed, use="complete.obs")^2,
#            cor(x$Stressed_hat, x$Stressed, use="complete.obs")^2)
#   return(R2s)
# })
# 
# m2_R2s <- do.call(rbind, c2_R2s)
# colnames(m2_R2s) <- variables
# 
# # Plotting
# df2_R2 <- data.frame(values = as.vector(m2_R2s),
#                      Variables = rep(colnames(m2_R2s), each = nrow(m2_R2s)),
#                      Clusters = rep(pred2$Classification, times = ncol(m2_R2s)))
# 
# # For plotting replace Variables with integers, to avoid sorting issue
# df2_plot <- df2_R2
# df2_plot$Variables[df2_plot$Variables=="Happy"] <- 1
# df2_plot$Variables[df2_plot$Variables=="Relaxed"] <- 2
# df2_plot$Variables[df2_plot$Variables=="Sad"] <- 3
# df2_plot$Variables[df2_plot$Variables=="Angry"] <- 4
# df2_plot$Variables[df2_plot$Variables=="Anxious"] <- 5
# df2_plot$Variables[df2_plot$Variables=="Depressed"] <- 6
# df2_plot$Variables[df2_plot$Variables=="Stressed"] <- 7
# 
# # plot
# pdf("/Users/Lexi/Desktop/internship/4_ plots/19a_ r2 two clusters.pdf", width=7, height=5)
# cols_k2 <- RColorBrewer::brewer.pal(2, "Set2")
# 
# boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
#         data = df2_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k2, each=7), ylim=c(0, .8))
# grid()
# boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
#         data = df2_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k2, each=7), ylim=c(0, .8), add = TRUE)
# 
# # axis and title
# axis(1, 1:14, rep(variables, times=2), las=2)
# axis(2, las=2)
# title(ylab="Proportion of Explained Variance")
# title("Two Cluster Model")
# 
# # add cluster names
# nClusters <- 2  
# nVars <- 7      
# mp <- seq(1, nClusters * nVars, length.out = nClusters + 1)
# mps <- (mp[-1] + mp[-length(mp)]) / 2  # centers of each cluster group
# for(k in 1:nClusters) {
#   text(mps[k], 0.8, paste0("Cluster ", k), col=cols_k2[k])
# }
# 
# dev.off()
# 
# 
# 
# #### THREE CLUSTER MODEL #### 
# # -------- Compute Predictions --------
# pred3 <- LCVARPred(object = out_seed1,
#                    data = data,
#                    k = 3)
# 
# # ---------- Compute R2 for each person and variable ----------
# 
# c3_R2s <- lapply(pred3$Predictions, function(x) {
#   R2s <- c(cor(x$Happy_hat, x$Happy, use="complete.obs")^2,
#            cor(x$Relaxed_hat, x$Relaxed, use="complete.obs")^2,
#            cor(x$Sad_hat, x$Sad, use="complete.obs")^2,
#            cor(x$Angry_hat, x$Angry, use="complete.obs")^2,
#            cor(x$Anxious_hat, x$Anxious, use="complete.obs")^2,
#            cor(x$Depressed_hat, x$Depressed, use="complete.obs")^2,
#            cor(x$Stressed_hat, x$Stressed, use="complete.obs")^2)
#   
#   return(R2s)
# })
# 
# 
# m3_R2s <- do.call(rbind, c3_R2s)
# colnames(m3_R2s) <- variables
# 
# # Plotting
# df3_R2 <- data.frame(values = as.vector(m3_R2s),
#                      Variables = rep(colnames(m3_R2s), each = nrow(m3_R2s)),
#                      Clusters = rep(pred3$Classification, times = ncol(m3_R2s)))
# 
# # mean(df3_R2[df3_R2$Clusters==3 & df3_R2$Variables=="Happy",]$values)
# 
# # For plotting replace Variables with integers, to avoid sorting issue
# df3_plot <- df3_R2
# df3_plot$Variables[df3_plot$Variables=="Happy"] <- 1
# df3_plot$Variables[df3_plot$Variables=="Relaxed"] <- 2
# df3_plot$Variables[df3_plot$Variables=="Sad"] <- 3
# df3_plot$Variables[df3_plot$Variables=="Angry"] <- 4
# df3_plot$Variables[df3_plot$Variables=="Anxious"] <- 5
# df3_plot$Variables[df3_plot$Variables=="Depressed"] <- 6
# df3_plot$Variables[df3_plot$Variables=="Stressed"] <- 7
# 
# 
# # plot
# pdf("/Users/Lexi/Desktop/internship/4_ plots/19b_ r2 three clusters.pdf", width=7, height=5)
# cols_k7 <- RColorBrewer::brewer.pal(7, "Set2")
# 
# boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
#         data = df3_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k7, each=7), ylim=c(0, .8))
# grid()
# boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
#         data = df3_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k7, each=7), ylim=c(0, .8), add = TRUE)
# 
# # axis and title
# axis(1, 1:21, rep(variables, times=3), las=2)
# axis(2, las=2)
# title(ylab="Proportion of Explained Variance")
# title("Three Cluster Model")
# 
# # add cluster names
# nClusters <- 3  
# nVars <- 7      
# mp <- seq(1, nClusters * nVars, length.out = nClusters + 1)
# mps <- (mp[-1] + mp[-length(mp)]) / 2  # centers of each cluster group
# for(k in 1:nClusters) {
#   text(mps[k], 0.8, paste0("Cluster ", k), col=cols_k7[k])
# }
# 
# dev.off()





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


# -------- Plot --------
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




# -------- Variables Together --------
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

legend(x = max(box_positions) - 3,  # push far right
       y = 1.05,                     # top of y-axis
       legend = paste("Cluster", 1:nClusters),
       fill = cols_k4,
       horiz = FALSE,               # vertical stack (better for far-right)
       cex = 0.9,
       xpd = TRUE,
       bty = "n")



#### MEDIAN FOR EACH MODEL #### 
median(df2_plot$values, na.rm = TRUE)
median(df3_plot$values, na.rm = TRUE)
median(df4_plot$values, na.rm = TRUE)


max(df2_plot$values, na.rm = T)
max(df3_plot$values, na.rm = T)
max(df4_plot$values, na.rm = T)



#####################################################################
#                15. SKEWNESS FOR RESIDUAL ANALYSIS                 #
#####################################################################
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


### skewness
skew(four_c2$Anxious, na.rm = TRUE)
skew(four_c2$Stressed, na.rm = TRUE)

skew(four_c2$Sad, na.rm = TRUE)
skew(four_c3$Sad, na.rm = TRUE)


skew(four_c1[6:12])
skew(four_c2[6:12])
skew(four_c3[6:12])
skew(four_c4[6:12])




######################################################################
#                16. COMPARE CESD AND CLASSIFICATION                 #
######################################################################

above16 <- cesd$PpID[cesd$original >= 16]
cluster3 <- external4[external4$Cluster == 3,]

length(above16)
length(cluster3$PID)

common_ill <- intersect(above16, cluster3$PID)
length(common_ill)/36


sort(cluster3$CESD)
sum(cluster3$CESD<16)



## above and below 16 classification
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

