plotGamQap <- function(results, rug = FALSE, plotTitles = NULL, xLabels = NULL, display = "terms", legend = TRUE){
  # Non-parametric plot for distance
  x <- results

  # plot functions for GAM models
  plotGam_Pois_response <- function(x){
    cols <- c("original estimate" = "red", "95%-quantile\ninterval" = "darkblue", "permuted estimates" = "grey")

    df3 <- as.data.frame(x$plotDataPois_resp[[1]])

    # plot data from glm linear effect
    dataGlm <- as.data.frame(x$plotData_origPois_resp_Glm)

    g2 <- ggplot2::ggplot()
    for (i in 2:length(x$plotDataPois_resp)){
      df_new <- as.data.frame(x$plotDataPois_resp[[i]])
      g2 <- g2 +
        geom_line(data = df_new, ggplot2::aes(x, fit, color = "permuted estimates"), size = 0.2, alpha = 0.6)
    }

    g2 <- g2 +
      geom_ribbon(data = x$dataQuantile_Pois_resp, ggplot2::aes(x = x, ymin = perm_low, ymax = perm_up),
                  alpha = 0.3, fill = "cornflowerblue") +
      geom_line(data = x$dataQuantile_Pois_resp, ggplot2::aes(x = x, y = perm_low, color = "95%-quantile\ninterval")) +
      geom_line(data = x$dataQuantile_Pois_resp, ggplot2::aes(x = x, y = perm_up, color = "95%-quantile\ninterval"))

    g3 <- g2 +
      # original estimate
      geom_line(data = df3, ggplot2::aes(x, fit, color = "original estimate"), size = 1.2) +
      geom_ribbon(data = df3, ggplot2::aes(x = x, ymin = fit-se, ymax = fit+se),
                  alpha = 0.2, fill = "red") +
      scale_color_manual(name = "", values = cols) +
      theme_bw() +
      scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
      theme(legend.position = "None",
            axis.text = element_text(size = 17),
            plot.title = element_text(size = 17),
            axis.title.x =  element_text(size = 17),
            axis.title.y = element_blank()
      ) +
      geom_line(data = dataGlm, ggplot2::aes(x = x, y = fit), col = "darkred", size = 1.2, lty = 3)

    if(rug == TRUE){
      g3 <- g3 + geom_rug(data = as.data.frame(unique(x$data_orig_Pois$xpois_1)), ggplot2::aes(x = unique(x$data_orig_Pois$xpois_1)), na.rm = TRUE, sides = "b", alpha = 0.3, size = 1)
    }
    return(g3)
  }

  # terms
  plotGam_Pois_terms <- function(x){
    cols <- c("original estimate" = "red", "95%-quantile\ninterval" = "darkblue", "permuted estimates" = "grey")
    # plot data from glm linear effect
    dataGlm <- as.data.frame(x$plotData_origPois_terms_Glm)

    df3 <- as.data.frame(x$plotDataPois_terms[[1]])

    g2 <- ggplot2::ggplot()
    for (i in 2:length(x$plotDataPois_terms)){
      df_new <- as.data.frame(x$plotDataPois_terms[[i]])
      g2 <- g2 +
        geom_line(data = df_new, ggplot2::aes(x, fit, color = "permuted estimates"), size = 0.2, alpha = 0.6)
    }

    g2 <- g2 +
      geom_ribbon(data = x$dataQuantile_Pois_terms, ggplot2::aes(x = x, ymin = perm_low, ymax = perm_up),
                  alpha = 0.3, fill = "cornflowerblue") +
      geom_line(data = x$dataQuantile_Pois_terms, ggplot2::aes(x = x, y = perm_low, color = "95%-quantile\ninterval")) +
      geom_line(data = x$dataQuantile_Pois_terms, ggplot2::aes(x = x, y = perm_up, color = "95%-quantile\ninterval"))

    g3 <- g2 +
      # original estimate
      geom_line(data = df3, ggplot2::aes(x = x, y = fit, color = "original estimate"), size = 1.2) +
      geom_ribbon(data = df3, ggplot2::aes(x = x, ymin = (fit-se), ymax = (fit+se)),
                  alpha = 0.2, fill = "red") +
      annotate(geom = "text", x = 300, y = 0.3,
               label = paste0("edf = ", round(summary(x$modelGamPois)$edf, 2)),
               color = "darkgrey", size = 6) +
      scale_color_manual(name = "", values = cols) +
      theme_bw() +
      scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
      theme(legend.position = "None",
            axis.text = element_text(size = 17),
            plot.title = element_text(size = 17),
            axis.title.x =  element_text(size = 17),
            axis.title.y = element_blank()
      ) +
      geom_line(data = dataGlm, ggplot2::aes(x = x, y = fit), col = "darkred", size = 1.2, lty = 3)

    if(rug == TRUE){
      g3 <- g3 + geom_rug(data = as.data.frame(unique(x$data_orig_Pois$xpois_1)), ggplot2::aes(x = unique(x$data_orig_Pois$xpois_1)), na.rm = TRUE, sides = "b", alpha = 0.3, size = 1)
    }
    return(g3)
  }

  #________________________________________________
  #________________________________________________

  ### Binomial
  # response

  plotGam_Bin_response <- function(x){
    cols <- c("original estimate" = "red", "95%-quantile\ninterval" = "darkblue", "permuted estimates" = "grey")

    df3 <- as.data.frame(x$plotDataBin_resp[[1]])

    # plot data from glm linear effect
    dataGlm <- as.data.frame(x$plotData_origBin_resp_Glm)

    g2 <- ggplot2::ggplot()
    for (i in 2:length(x$plotDataBin_resp)){
      df_new <- as.data.frame(x$plotDataBin_resp[[i]])
      g2 <- g2 +
        geom_line(data = df_new, ggplot2::aes(x, fit, color = "permuted estimates"), size = 0.2, alpha = 0.6)
    }

    g2 <- g2 +
      geom_ribbon(data = x$dataQuantile_Bin_resp, ggplot2::aes(x = x, ymin = perm_low, ymax = perm_up),
                  alpha = 0.3, fill = "cornflowerblue") +
      geom_line(data = x$dataQuantile_Bin_resp, ggplot2::aes(x = x, y = perm_low, color = "95%-quantile\ninterval")) +
      geom_line(data = x$dataQuantile_Bin_resp, ggplot2::aes(x = x, y = perm_up, color = "95%-quantile\ninterval"))

    g3 <- g2 +
      # original estimate
      geom_line(data = df3, ggplot2::aes(x, fit, color = "original estimate"), size = 1.2) +
      geom_ribbon(data = df3, ggplot2::aes(x = x, ymin = fit-se, ymax = fit+se),
                  alpha = 0.2, fill = "red") +
      scale_color_manual(name = "", values = cols) +
      theme_bw() +
      scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
      theme(legend.position = "None",
            axis.text = element_text(size = 17),
            plot.title = element_text(size = 17),
            axis.title.x =  element_text(size = 17),
            axis.title.y = element_blank()
      ) +
      geom_line(data = dataGlm, ggplot2::aes(x = x, y = fit), col = "darkred", size = 1.2, lty = 3)

    if(rug == TRUE){
      g3 <- g3 + geom_rug(data = as.data.frame(unique(x$data_orig_Bin$xbin_1)), ggplot2::aes(x = unique(x$data_orig_Bin$xbin_1)), na.rm = TRUE, sides = "b", alpha = 0.3, size = 1)
    }
    return(g3)
  }

  # terms
  plotGam_Bin_terms <- function(x){
    cols <- c("original estimate" = "red", "95%-quantile\ninterval" = "darkblue", "permuted estimates" = "grey")

    df3 <- as.data.frame(x$plotDataBin_terms[[1]])

    # plot data from glm linear effect
    dataGlm <- as.data.frame(x$plotData_origBin_terms_Glm)

    g2 <- ggplot2::ggplot()
    for (i in 2:length(x$plotDataBin_terms)){
      df_new <- as.data.frame(x$plotDataBin_terms[[i]])
      g2 <- g2 +
        geom_line(data = df_new, ggplot2::aes(x, fit, color = "permuted estimates"), size = 0.2, alpha = 0.6)
    }

    g2 <- g2 +
      geom_ribbon(data = x$dataQuantile_Bin_terms, ggplot2::aes(x = x, ymin = perm_low, ymax = perm_up),
                  alpha = 0.3, fill = "cornflowerblue") +
      geom_line(data = x$dataQuantile_Bin_terms, ggplot2::aes(x = x, y = perm_low, color = "95%-quantile\ninterval")) +
      geom_line(data = x$dataQuantile_Bin_terms, ggplot2::aes(x = x, y = perm_up, color = "95%-quantile\ninterval"))

    g3 <- g2 +
      # original estimate
      geom_line(data = df3, ggplot2::aes(x = x, y = fit, color = "original estimate"), size = 1.2) +
      geom_ribbon(data = df3, ggplot2::aes(x = x, ymin = (fit-se), ymax = (fit+se)),
                  alpha = 0.2, fill = "red") +
      annotate(geom = "text", x = 300, y = 0.3,
               label = paste0("edf = ", round(summary(x$modelGamBin)$edf, 2)),
               color = "darkgrey", size = 6) +
      scale_color_manual(name = "", values = cols) +
      theme_bw() +
      scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
      theme(legend.position = "None",
            axis.text = element_text(size = 17),
            plot.title = element_text(size = 17),
            axis.title.x =  element_text(size = 17),
            axis.title.y = element_blank()
      ) +
      geom_line(data = dataGlm, ggplot2::aes(x = x, y = fit), col = "darkred", size = 1.2, lty = 3)

    if(rug == TRUE){
      g3 <- g3 + geom_rug(data = as.data.frame(unique(x$data_orig_Bin$xbin_1)), ggplot2::aes(x = unique(x$data_orig_Bin$xbin_1)), na.rm = TRUE, sides = "b", alpha = 0.3, size = 1)
    }
    return(g3)
  }

  nCovs <- length(results$data_orig_Bin)-1
  plots <- list()

  if(display == "terms"){
    plots[[1]] <- plotGam_Bin_terms(results) +
      theme(axis.title.y = element_text(size = 17, angle = 90)) + labs(y = "s(distance)")
    plots[[2]] <- plotGam_Pois_terms(results)
  }

  if(display == "response"){
    plots[[1]] <- plotGam_Bin_response(results) +
      theme(axis.title.y = element_text(size = 17, angle = 90)) + labs(y = "predicted response")
    plots[[2]] <- plotGam_Pois_response(results)
  }

  if(nCovs > 1){
  for(i in 1:(nCovs-1)){
    linPlots <- list()
    # binomial
    testData <- data.frame(dist = x$qaplist$zeroGam[[i]]$dist)
    linPlots$bin <- ggplot2::ggplot(testData, ggplot2::aes(dist)) +
      geom_density() +
      theme_bw() +
      geom_vline(xintercept = x$qaplist$zeroGam[[i]]$testval, linetype = "dashed") +
      geom_hline(yintercept = 0) +
      coord_cartesian(xlim = c(min(x$qaplist$zeroGam[[i]]$testval, min(x$qaplist$zeroGam[[i]]$dist))-0.0001,
                               max(x$qaplist$zeroGam[[i]]$testval, max(x$qaplist$zeroGam[[i]]$dist))),
                      ylim = c(0,max(density(x$qaplist$zeroGam[[i]]$dist)$y) + 10),
                      expand = FALSE) +
      scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
      theme(axis.text = element_text(size = 17),
            plot.title = element_text(size = 17),
            axis.title.x =  element_text(size = 17),
            axis.title.y = element_blank()
      )

    #poisson
    testData <- data.frame(dist = x$qaplist$countGam[[i]]$dist)
    linPlots$pois <- ggplot2::ggplot(testData, ggplot2::aes(dist)) +
      geom_density() +
      theme_bw() +
      geom_vline(xintercept = x$qaplist$countGam[[i]]$testval, linetype = "dashed") +
      geom_hline(yintercept = 0) +
      coord_cartesian(xlim = c(min(x$qaplist$countGam[[i]]$testval, min(x$qaplist$countGam[[i]]$dist))-0.0001,
                               max(x$qaplist$countGam[[i]]$testval, max(x$qaplist$countGam[[i]]$dist))),
                      ylim = c(0,max(density(x$qaplist$countGam[[i]]$dist)$y) + 10),
                      expand = FALSE) +
      scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
      theme(axis.text = element_text(size = 17),
            plot.title = element_text(size = 17),
            axis.title.x =  element_text(size = 17),
            axis.title.y = element_blank()
      )

    plots <- rlist::list.append(plots, linPlots[[1]])
    plots <- rlist::list.append(plots, linPlots[[2]])
  }
  plots[[3]] <- plots[[3]] +
    theme(axis.title.y = element_text(size = 17, angle = 90)) +
    labs(y = "Density")
}

  if(!is.null(plotTitles)){
    for(i in 1:length(plots)){
      plots[[i]] <- plots[[i]] + ggtitle(plotTitles[[i]])
    }
  }

  if(!is.null(xLabels)){
    for(i in 1:length(plots)){
      plots[[i]] <- plots[[i]] + labs(x = xLabels[[i]])
    }
  }
  if(legend == TRUE){
      cols <- c("original estimate" = "red", "95%-quantile\ninterval" = "darkblue", "permuted estimates" = "grey", "linear estimate" = "darkred")
      ltys <- c("original estimate" = "solid", "95%-quantile\ninterval" = "solid", "permuted estimates" = "solid", "linear estimate" = "dotted")
      override.linetype <- c(1, 1, 1, 3)

      legendPlot <- plots[[2]] +
        theme(legend.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.key.width = unit(1.5, "cm"),
              legend.position = "bottom",
              legend.box.margin = margin(0.5,-20, 0.5,-1, unit = "cm"),
              legend.background = element_blank(),
              legend.key = element_blank()) +
        scale_linetype_manual(name = " ", values = ltys) +
        scale_color_manual(name = " ", values = cols) +
        guides(colour = guide_legend(override.aes = list(linetype = override.linetype)))
      leg <- cowplot::get_legend(legendPlot)
  plots[[length(plots)+1]] <- leg
    do.call("grid.arrange", args = list(grobs = plots, ncol = (length(plots)-1), nrow = 2, heights = c(1, 0.2)))
  } else {
    do.call("grid.arrange", args = list(grobs = plots, ncol = length(plots)))
  }
}


#--------------------------------------------------
#--------------------------------------------------
plotGlmQap <- function(results, rug = FALSE, plotTitles = NULL, xLabels = NULL, ...){

  nCovs <- length(results$data_orig_Bin)-1
  plots <- list()

  # binomial
  x_bin <- results
  x_bin$data_orig <- x_bin$data_orig_Bin
  x_bin$pred_data <- x_bin$pred_data_Bin
  x_bin$plotData <- x_bin$plotDataBin
  x_bin$CIdata <- x_bin$CIdataBin
  x_bin$myGlm <- x_bin$qaplist$modelBin
  x_bin$qaplistGlm <- x_bin$qaplist$zeroGlm
  x_bin$matrix_bin_coefficients <- x_bin$matrix_bin_coefficientsBin

  # possion
  x_pois <- results
  x_pois$data_orig <- x_pois$data_orig_Pois
  x_pois$pred_data <- x_pois$pred_data_Pois
  x_pois$plotData <- x_pois$plotDataPois
  x_pois$CIdata <- x_pois$CIdataPois
  x_pois$myGlm <- x_pois$qaplist$modelPois
  x_pois$qaplistGlm <- x_pois$qaplist$countGlm
  x_pois$matrix_pois_coefficients <- x_pois$matrix_pois_coefficientsPois

  for(i in 1:(nCovs)){
    linPlots <- list()
    # binomial
    testData <- data.frame(dist = x_bin$qaplistGlm[[i]]$dist)
    linPlots$bin <- ggplot2::ggplot(testData, ggplot2::aes(dist)) +
      geom_density() +
      theme_bw() +
      geom_vline(xintercept = x_bin$qaplistGlm[[i]]$testval, linetype = "dashed") +
      geom_hline(yintercept = 0) +
      coord_cartesian(xlim = c(min(x_bin$qaplistGlm[[i]]$testval, min(x_bin$qaplistGlm[[i]]$dist))-0.0005,
                               max(x_bin$qaplistGlm[[i]]$testval, max(x_bin$qaplistGlm[[i]]$dist))+0.0005),
                      ylim = c(0,max(density(x_bin$qaplistGlm[[i]]$dist)$y) + 10),
                      expand = FALSE) +
      scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
      theme(axis.text = element_text(size = 17),
            plot.title = element_text(size = 20),
            axis.title.x =  element_text(size = 20),
            axis.title.y = element_blank()
      )

    #poisson
    testData <- data.frame(dist = x_pois$qaplistGlm[[i]]$dist)
    linPlots$pois <- ggplot2::ggplot(testData, ggplot2::aes(dist)) +
      geom_density() +
      theme_bw() +
      geom_vline(xintercept = x_pois$qaplistGlm[[i]]$testval, linetype = "dashed") +
      geom_hline(yintercept = 0) +
      coord_cartesian(xlim = c(min(x_pois$qaplistGlm[[i]]$testval, min(x_pois$qaplistGlm[[i]]$dist))-0.0005,
                               max(x_pois$qaplistGlm[[i]]$testval, max(x_pois$qaplistGlm[[i]]$dist))+0.0005),
                      ylim = c(0,max(density(x_pois$qaplistGlm[[i]]$dist)$y) + 10),
                      expand = FALSE) +
      scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
      theme(axis.text = element_text(size = 17),
            plot.title = element_text(size = 20),
            axis.title.x =  element_text(size = 20),
            axis.title.y = element_blank()
      )

    plots <- rlist::list.append(plots, linPlots[[1]])
    plots <- rlist::list.append(plots, linPlots[[2]])
  }

  plots[[1]] <- plots[[1]] +
    theme(axis.title.y = element_text(size = 17, angle = 90)) +
    labs(y = "Density")

  if(!is.null(plotTitles)){
    for(i in 1:length(plots)){
      plots[[i]] <- plots[[i]] + ggtitle(plotTitles[[i]])
    }
  }

  if(!is.null(xLabels)){
    for(i in 1:length(plots)){
      plots[[i]] <- plots[[i]] + labs(x = xLabels[[i]])
    }
  }

  do.call("grid.arrange", args = list(grobs = plots, ncol = length(plots)))
}

plotHurdleQap <- function(results, method = "parametric", rug = FALSE, plotTitles = NULL, xLabels = NULL, display = "terms", legend = TRUE){
  if(method == "parametric"){
    plotPar <- plotGlmQap(results = results, rug = rug,
                               plotTitles = plotTitles, xLabels = xLabels)
    return(plotPar)
  }
  if(method == "nonparametric"){
    plotNonP <- plotGamQap(results = results, rug = rug,
                            plotTitles = plotTitles,
                            xLabels = xLabels, display = display)
    return(plotNonP)
  }
}



