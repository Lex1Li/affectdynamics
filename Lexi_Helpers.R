#######################################################
#                  TIME SERIES PLOTS                  #
#######################################################
plotLabel <- function(x, srt=0, col="black",
                      xpos=.5, ypos=.5, cex=1.5) {
  par(mar=rep(0, 4))
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(0,1))
  text(xpos, ypos, x, srt=srt, cex=cex, col=col)
}

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
                        col_pred = "orange") {  
  
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
  ymin <- min(c(negLL_scaled, BIC_scaled)-0.005)
  
  plot.window(xlim=c(1,K), ylim=c(ymin, ymax))
  axis(1, 1:K)
  axis(2, las=2)
  grid()
  title(xlab="Number of Clusters", line=2.5)
  title(title, font.main=1)
  
  # plot
  shift <- 0 # why add/subtract shift?
  
  # negLL
  points((1:K)-shift, negLL_scaled, pch=15, cex=1.25, col = "#377EB8")
  lines((1:K)-shift, negLL_scaled, lwd=2, col = "#377EB8")
  
  # BIC
  points((1:K)+shift, BIC_scaled, col= "#E41A1C", pch=16, cex=1.25)
  lines((1:K)+shift, BIC_scaled, col= "#E41A1C", lwd=2)
  
  # legend
  legend("topright",
         legend = c("BIC", "NLL"),
         col = c("#E41A1C", "#377EB8"),
         pch = c(16, 15),
         lwd = 2,
         bty = "n"
  )
}


PlotScaled(out_seed1, title = "BIC and NLL")



#############################################
#                  BV PLOT                  #
#############################################
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
  if(title) title(main=paste0("ID ", ID), font.main=1)
  
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


#############################################
#                  IC PLOT                  #
#############################################
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



##############################################
#                  BIC PLOT                  #
##############################################

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



#############################################################
#                  SCALED BIC AND NLL PLOT                  #
#############################################################
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
  title(xlab="Number of clusters", line=2.5)
  title(title, font.main=1)
  
  # plot
  shift <- 0 # why add/subtract shift?
  
  # negLL
  points((1:K)-shift, negLL_scaled, pch=16, cex=1.25, col = "#377EB8")
  lines((1:K)-shift, negLL_scaled, lwd=2, col = "#377EB8")
  
  # BIC
  points((1:K)+shift, BIC_scaled, col="#E41A1C", pch=16, cex=1.25)
  lines((1:K)+shift, BIC_scaled, col="#E41A1C", lwd=2)
  
  # legend
  legend("topright", legend=c("Negative LL (scaled)", "BIC (scaled)"),
         text.col=c("#377EB8", "#E41A1C"), bty="n", cex=1)
}



###############################################################
#                  SET LAYOUT COEF DIFF PLOT                  #
###############################################################
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



###############################################
#                  PLOT HEAT                  #
###############################################
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



#######################################################################
#                  PLOT COEF AND CLUSTER DIFFERENCES                  #
#######################################################################
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
                         labels = labels,
                         small_difference_to_white = small_difference_to_white)
  
  
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




#######################################################################
#                  GET BEST MODEL FOR CLASSIFICATION                  #
#######################################################################
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
  
  BestRunOneK <- numeric(length(object$Call$Clusters))
  names(BestRunOneK) <- paste0("K", object$Call$Clusters)
  
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
    
    BestRunOneK[ClustCount] <- which.max(FitAllLags)[1]
  }
  
  return(BestRunOneK)
}




################################################################
#                  3D PLOT OF TIMESERIES DATA                  #
################################################################
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
  
  
  # ----- df of id and classification -----
  class_df <- data.frame(
    id = as.numeric(rownames(class_mat)),
    classification = as.integer(class_mat[, 1])
  )
  
  
  # ----- extract id and get timeseries data of those ids -----
  cluster_ids <- class_df[class_df$classification == clusterToPlot, "id"]
  cluster_data <- data[data[[PID]] %in% cluster_ids, ]
  
  if (showN) {
    message("Number of participants classified into Cluster ", clusterToPlot, ": ", length(cluster_ids))
  }
  

  # ----- prep data for plotting -----
  plot_df <- cluster_data %>%
    select(all_of(c(PID, timepoints, variable))) %>%
    mutate(
      !!PID := as.character(.data[[PID]]),
      !!PID := paste0("ID", .data[[PID]])
    )
  
  
  # ----- plot -----
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



############################################################################
#                  COLLAPSED TIME SERIES PLOT BY CLUSTERS                  #
############################################################################
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
                          opaqueness = 1,
                          random = FALSE) {   
  
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
  
  # ----- handle random highlighting and legend labels -----
  unique_ids <- unique(cluster_data[[PID]])
  legend_labels <- as.character(unique_ids)
  if (random) {
    random_id <- sample(unique_ids, 1)
    cluster_data$alpha_plot <- ifelse(cluster_data[[PID]] == random_id, 1, 0.1)
    legend_labels <- ifelse(unique_ids == random_id,
                            paste0("**", unique_ids, "**"),
                            as.character(unique_ids))
    
    message("Selected id: ", random_id)
  } else {
    cluster_data$alpha_plot <- opaqueness
  }
  
  # Use ggtext for bold legend labels (requires ggtext package)
  if (requireNamespace("ggtext", quietly = TRUE)) {
    theme_legend <- theme(
      legend.text = ggtext::element_markdown(),
      legend.position = if (show_legend) "right" else "none",
      plot.margin = unit(plot_margin, "cm"),
      plot.title = element_text(size = 12)
    )
  } else {
    theme_legend <- theme(
      legend.position = if (show_legend) "right" else "none",
      plot.margin = unit(plot_margin, "cm"),
      plot.title = element_text(size = 12)
    )
  }
  
  ggplot(cluster_data, aes_string(x = timepoints, 
                                  y = variable, 
                                  group = paste0("factor(", PID, ")"), 
                                  colour = paste0("factor(", PID, ")"))) +
    geom_line(aes(alpha = alpha_plot)) +
    ylim(0, 100) +
    labs(
      x = if (remove_xlab) NULL else timepoints,
      y = variable,
      title = title
    ) +
    scale_colour_discrete(name = 'Participant ID', labels = legend_labels) +
    scale_alpha_identity() +  
    theme_minimal() +
    theme_legend
}


##################################################################
#                  PLOT CONTEMPORANEOUS EFFECTS                  #
##################################################################
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
                         pcor = TRUE,
                         small_difference_to_white = small_difference_to_white)
  
  
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



################################################
#                  PLOT MEANS                  #
################################################
PlotMEANS <- function(model, 
                      nCluster, 
                      variables) {
  
  l_coefs <- list()
  l_coefs <- coef.ClusterVAR(model, Model = rep(1, nCluster))
  
  # Means at t=0 for the selected cluster solution
  m_ints <- l_coefs$Exogenous_coefficients[, 1, ] 
  
  K <- nCluster
  
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



#################################################
#                  PLOT TRENDS                  #
#################################################
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





########################################################
#                  VAR PRED FUNCTIONS                  #
########################################################
# hardcoded

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
  
  m_pred[!ind_pred, ] <- NA
  NAs <- apply(data_s, 1, function(x) any(is.na(x[p_loc])))
  m_pred[NAs, ] <- NA
  
  
  # Make dataframe
  out_df <- data.frame(cbind(data_s[, 1:11], m_pred))
  colnames(out_df)[1:11] <- colnames(data_s)[1:11]
  colnames(out_df)[12:18] <- paste0(colnames(data_s)[5:11], "_hat")
  
  return(out_df)
  
} # eoF


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


