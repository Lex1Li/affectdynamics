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
        plot.margin = unit(c(1, 0.5, 0, 0.5), "cm")
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



### mean cesd by cluster
tapply(external2$CESD, external2$Cluster, mean, na.rm = TRUE)
tapply(external3$CESD, external3$Cluster, mean, na.rm = TRUE)
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
