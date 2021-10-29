# load packages
library(rlist)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(mgcv)

#____________________________________________________________________________
#____________________________________________________________________________

plot.GamQap.Pub <- function(results, rug = FALSE, distance, plotTitles = NULL, xTitles = NULL, xLabels = NULL, display = "terms", ...){
  # Non-parametric plot for distance
  x <- results
  
  # plot functions for GAM models
  plotGam_Pois_response <- function(x){
    cols <- c("Original Fit" = "red", "95%-Quantile\nInterval" = "darkblue", "Permuted Fits" = "grey")
    
    df3 <- as.data.frame(x$plotDataPois_resp[[1]])
    
    # plot data from glm linear effect
    dataGlm <- as.data.frame(x$plotData_origPois_resp_Glm)
    
    g2 <- ggplot()
    for (i in 2:length(x$plotDataPois_resp)){
      df_new <- as.data.frame(x$plotDataPois_resp[[i]])
      g2 <- g2 +
        geom_line(data = df_new, aes(x, fit, color = "Permuted Fits"), size = 0.2, alpha = 0.6)
    }
    
    g2 <- g2 +
      geom_ribbon(data = x$dataQuantile_Pois_resp, aes(x = x, ymin = perm_low, ymax = perm_up),
                  alpha = 0.3, fill = "cornflowerblue") +
      geom_line(data = x$dataQuantile_Pois_resp, aes(x = x, y = perm_low, color = "95%-Quantile\nInterval")) +
      geom_line(data = x$dataQuantile_Pois_resp, aes(x = x, y = perm_up, color = "95%-Quantile\nInterval"))
    
    g3 <- g2 +
      # Original fit
      geom_line(data = df3, aes(x, fit, color = "Original Fit"), size = 1.2) +
      geom_ribbon(data = df3, aes(x = x, ymin = fit-se, ymax = fit+se), 
                  alpha = 0.2, fill = "red") +
       scale_color_manual(name = "", values = cols) +
      coord_cartesian(xlim= c(0, distance), expand = FALSE) +
      theme_bw() +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) + 
      theme(legend.position = "None",
            axis.text = element_text(size = 17),
            plot.title = element_text(size = 24),
            axis.title.x =  element_text(size = 20),
            axis.title.y = element_blank()
      ) +
      geom_line(data = dataGlm, aes(x = x, y = fit), col = "darkred", size = 1.2, lty = 3)
    
    if(rug == TRUE){
      g3 <- g3 + geom_rug(data = as.data.frame(unique(x$data_orig_Pois$xpois_1)), aes(x = unique(x$data_orig_Pois$xpois_1)), na.rm = TRUE, sides = "b", alpha = 0.3, size = 1)
    }
    return(g3)
  }

    # terms
  plotGam_Pois_terms <- function(x){
    cols <- c("Original Fit" = "red", "95%-Quantile\nInterval" = "darkblue", "Permuted Fits" = "grey")
    # plot data from glm linear effect
    dataGlm <- as.data.frame(x$plotData_origPois_terms_Glm)
    
    df3 <- as.data.frame(x$plotDataPois_terms[[1]])
    
    g2 <- ggplot()
    for (i in 2:length(x$plotDataPois_terms)){
      df_new <- as.data.frame(x$plotDataPois_terms[[i]])
      g2 <- g2 +
        geom_line(data = df_new, aes(x, fit, color = "Permuted Fits"), size = 0.2, alpha = 0.6)
    }
    
    g2 <- g2 +
      geom_ribbon(data = x$dataQuantile_Pois_terms, aes(x = x, ymin = perm_low, ymax = perm_up),
                  alpha = 0.3, fill = "cornflowerblue") +
      geom_line(data = x$dataQuantile_Pois_terms, aes(x = x, y = perm_low, color = "95%-Quantile\nInterval")) +
      geom_line(data = x$dataQuantile_Pois_terms, aes(x = x, y = perm_up, color = "95%-Quantile\nInterval"))
    
    g3 <- g2 +
      # Original fit
      geom_line(data = df3, aes(x = x, y = fit, color = "Original Fit"), size = 1.2) +
      geom_ribbon(data = df3, aes(x = x, ymin = (fit-se), ymax = (fit+se)), 
                  alpha = 0.2, fill = "red") +
        annotate(geom = "text", x = 300, y = 0.3,
               label = paste0("edf = ", round(summary(x$modelGamPois)$edf, 2)),
               color = "darkgrey", size = 6) +
      scale_color_manual(name = "", values = cols) +
      coord_cartesian(xlim= c(0, distance), expand = FALSE) +
      theme_bw() +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) + 
      theme(legend.position = "None",
            axis.text = element_text(size = 17),
            plot.title = element_text(size = 24),
            axis.title.x =  element_text(size = 20),
            axis.title.y = element_blank()
      ) +
      geom_line(data = dataGlm, aes(x = x, y = fit), col = "darkred", size = 1.2, lty = 3)
    
    if(rug == TRUE){
      g3 <- g3 + geom_rug(data = as.data.frame(unique(x$data_orig_Pois$xpois_1)), aes(x = unique(x$data_orig_Pois$xpois_1)), na.rm = TRUE, sides = "b", alpha = 0.3, size = 1)
    }
    return(g3)
  }
  
  #________________________________________________
  #________________________________________________
  
  ### Binomial
  # response
  
  plotGam_Bin_response <- function(x){
    cols <- c("Original Fit" = "red", "95%-Quantile\nInterval" = "darkblue", "Permuted Fits" = "grey")
    
    df3 <- as.data.frame(x$plotDataBin_resp[[1]])
    
    # plot data from glm linear effect
    dataGlm <- as.data.frame(x$plotData_origBin_resp_Glm)
    
    g2 <- ggplot()
    for (i in 2:length(x$plotDataBin_resp)){
      df_new <- as.data.frame(x$plotDataBin_resp[[i]])
      g2 <- g2 +
        geom_line(data = df_new, aes(x, fit, color = "Permuted Fits"), size = 0.2, alpha = 0.6)
    }
    
    g2 <- g2 +
      geom_ribbon(data = x$dataQuantile_Bin_resp, aes(x = x, ymin = perm_low, ymax = perm_up),
                  alpha = 0.3, fill = "cornflowerblue") +
      geom_line(data = x$dataQuantile_Bin_resp, aes(x = x, y = perm_low, color = "95%-Quantile\nInterval")) +
      geom_line(data = x$dataQuantile_Bin_resp, aes(x = x, y = perm_up, color = "95%-Quantile\nInterval"))
    
    g3 <- g2 +
      # Original fit
      geom_line(data = df3, aes(x, fit, color = "Original Fit"), size = 1.2) +
      geom_ribbon(data = df3, aes(x = x, ymin = fit-se, ymax = fit+se), 
                  alpha = 0.2, fill = "red") +
       scale_color_manual(name = "", values = cols) +
      coord_cartesian(xlim= c(0, distance), expand = FALSE) +
      theme_bw() +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) + 
      theme(legend.position = "None",
            axis.text = element_text(size = 17),
            plot.title = element_text(size = 24),
            axis.title.x =  element_text(size = 20),
            axis.title.y = element_blank()
      ) +
      geom_line(data = dataGlm, aes(x = x, y = fit), col = "darkred", size = 1.2, lty = 3)
    
    if(rug == TRUE){
      g3 <- g3 + geom_rug(data = as.data.frame(unique(x$data_orig_Bin$xbin_1)), aes(x = unique(x$data_orig_Bin$xbin_1)), na.rm = TRUE, sides = "b", alpha = 0.3, size = 1)
    }
    return(g3)
  }
  
  # terms 
  plotGam_Bin_terms <- function(x){
    cols <- c("Original Fit" = "red", "95%-Quantile\nInterval" = "darkblue", "Permuted Fits" = "grey")
    
    df3 <- as.data.frame(x$plotDataBin_terms[[1]])
    
    # plot data from glm linear effect
    dataGlm <- as.data.frame(x$plotData_origBin_terms_Glm)
    
    g2 <- ggplot()
    for (i in 2:length(x$plotDataBin_terms)){
      df_new <- as.data.frame(x$plotDataBin_terms[[i]])
      g2 <- g2 +
        geom_line(data = df_new, aes(x, fit, color = "Permuted Fits"), size = 0.2, alpha = 0.6)
    }
    
    g2 <- g2 +
      geom_ribbon(data = x$dataQuantile_Bin_terms, aes(x = x, ymin = perm_low, ymax = perm_up),
                  alpha = 0.3, fill = "cornflowerblue") +
      geom_line(data = x$dataQuantile_Bin_terms, aes(x = x, y = perm_low, color = "95%-Quantile\nInterval")) +
      geom_line(data = x$dataQuantile_Bin_terms, aes(x = x, y = perm_up, color = "95%-Quantile\nInterval"))
    
    g3 <- g2 +
      # Original fit
      geom_line(data = df3, aes(x = x, y = fit, color = "Original Fit"), size = 1.2) +
      geom_ribbon(data = df3, aes(x = x, ymin = (fit-se), ymax = (fit+se)), 
                  alpha = 0.2, fill = "red") +
      annotate(geom = "text", x = 300, y = 0.3,
               label = paste0("edf = ", round(summary(x$modelGamBin)$edf, 2)),
               color = "darkgrey", size = 6) +
      scale_color_manual(name = "", values = cols) +
      coord_cartesian(xlim= c(0, distance), expand = FALSE) +
      theme_bw() +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) + 
      theme(legend.position = "None",
            axis.text = element_text(size = 17),
            plot.title = element_text(size = 24),
            axis.title.x =  element_text(size = 20),
            axis.title.y = element_blank()
      ) +
      geom_line(data = dataGlm, aes(x = x, y = fit), col = "darkred", size = 1.2, lty = 3)
    
    if(rug == TRUE){
      g3 <- g3 + geom_rug(data = as.data.frame(unique(x$data_orig_Bin$xbin_1)), aes(x = unique(x$data_orig_Bin$xbin_1)), na.rm = TRUE, sides = "b", alpha = 0.3, size = 1)
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
  
  for(i in 1:(nCovs-1)){
    linPlots <- list()
    # binomial
    testData <- data.frame(dist = x$qaplist$zeroGam[[i]]$dist)
    linPlots$bin <- ggplot(testData, aes(dist)) + 
      geom_density() + 
      theme_bw() + 
      geom_vline(xintercept = x$qaplist$zeroGam[[i]]$testval, linetype = "dashed") +
      geom_hline(yintercept = 0) +
      coord_cartesian(xlim = c(min(x$qaplist$zeroGam[[i]]$testval, min(x$qaplist$zeroGam[[i]]$dist))-0.0001,
                               max(x$qaplist$zeroGam[[i]]$testval, max(x$qaplist$zeroGam[[i]]$dist))), 
                      ylim = c(0,max(density(x$qaplist$zeroGam[[i]]$dist)$y) + 10),
                      expand = FALSE) +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
      theme(axis.text = element_text(size = 17),
            plot.title = element_text(size = 24),
            axis.title.x =  element_text(size = 20),
            axis.title.y = element_blank()
      )
    
    #poisson
    testData <- data.frame(dist = x$qaplist$countGam[[i]]$dist)
    linPlots$pois <- ggplot(testData, aes(dist)) + 
      geom_density() + 
      theme_bw() + 
      geom_vline(xintercept = x$qaplist$countGam[[i]]$testval, linetype = "dashed") +
      geom_hline(yintercept = 0) +
      coord_cartesian(xlim = c(min(x$qaplist$countGam[[i]]$testval, min(x$qaplist$countGam[[i]]$dist))-0.0001,
                               max(x$qaplist$countGam[[i]]$testval, max(x$qaplist$countGam[[i]]$dist))), 
                      ylim = c(0,max(density(x$qaplist$countGam[[i]]$dist)$y) + 10),
                      expand = FALSE) +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
      theme(axis.text = element_text(size = 17),
            plot.title = element_text(size = 24),
            axis.title.x =  element_text(size = 20),
            axis.title.y = element_blank()
            )
    
    plots <- list.append(plots, linPlots[[1]])
    plots <- list.append(plots, linPlots[[2]])
  }
  plots[[3]] <- plots[[3]] + 
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

#-----------------------------------------------------------

load("Workspaces/server/Original_interd_all_x2_wrapper_Bielefeld.RData")

# plot for legend
x <- Original_interd_all_x2
cols <- c("original estimate" = "red", "95%-quantile\ninterval" = "darkblue", "permuted estimates" = "grey", "linear estimate" = "darkred")
ltys <- c("original estimate" = "solid", "95%-quantile\ninterval" = "solid", "permuted estimates" = "solid", "linear estimate" = "dotted")
override.linetype <- c(1, 1, 1, 3)

df3 <- as.data.frame(x$plotDataBin_terms[[1]])

# plot data from glm linear effect
dataGlm <- as.data.frame(x$plotData_origBin_terms_Glm)

g2 <- ggplot()
for (i in 2:length(x$plotDataBin_terms)){
  df_new <- as.data.frame(x$plotDataBin_terms[[i]])
  g2 <- g2 +
    geom_line(data = df_new, aes(x, fit, color = "permuted fits"), size = 0.2, alpha = 0.6)
}

g2 <- g2 +
  geom_ribbon(data = x$dataQuantile_Bin_terms, aes(x = x, ymin = perm_low, ymax = perm_up),
              alpha = 0.3, fill = "cornflowerblue") +
  geom_line(data = x$dataQuantile_Bin_terms, aes(x = x, y = perm_low, color = "95%-quantile\ninterval")) +
  geom_line(data = x$dataQuantile_Bin_terms, aes(x = x, y = perm_up, color = "95%-quantile\ninterval"))

legPlot <- g2 +
  # Original fit
  geom_line(data = df3, aes(x = x, y = fit, color = "original estimate"), size = 1.2) +
  geom_ribbon(data = df3, aes(x = x, ymin = (fit-se), ymax = (fit+se)), 
              alpha = 0.2, fill = "red")  +
  geom_line(data = dataGlm, aes(x = x, y = fit, color = "linear estimate"), lty = 3, size = 1.2) +
  scale_linetype_manual(name = " ", values = ltys) +
  scale_color_manual(name = " ", values = cols) +
  guides(colour = guide_legend(override.aes = list(linetype = override.linetype))) +
  theme_bw() + 
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.key.width = unit(1.5, "cm"), 
        legend.justification = c(1,0), 
        legend.position = "bottom",  
        legend.background = element_blank(),
        legend.box.margin = margin(0.5,6, 0.5,-1, unit = "cm"),
        legend.key = element_blank())

leg <- get_legend(legPlot)
save(file = "Graphics/plotLegend.Rdata", leg)

#-----------------------------------------------------------
# Bielefeld

# two covariates
### all buildings
load("Workspaces/server/Original_all_all_x2_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_interd_all_x2_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_inst_all_x2_wrapper_Bielefeld.RData")
load("Graphics/plotLegend.Rdata")

# terms
p1 <- plot.GamQap.Pub(results = Original_all_all_x2, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "terms", 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength"),
                      xLabels = list("", "", "", ""))
p2 <- plot.GamQap.Pub(results = Original_interd_all_x2, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "terms",
                      xLabels = list("", "", "", ""))
p3 <- plot.GamQap.Pub(results = Original_inst_all_x2, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "terms", 
                      xLabels = list("Distance in m",
                                     "Distance in m",
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub])))

combine_Gam_Bielefeld_all_x2_terms <- grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                             arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                             arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                             leg, 
                                             heights = c(2.5, 2, 2, 0.6))


png("Graphics/Bielefeld/combine_Gam_Bielefeld_all_x2_terms.png", width = 1000, height = 700)
grid.draw(combine_Gam_Bielefeld_all_x2_terms) 
dev.off()

pdf("Graphics/Bielefeld/combine_Gam_Bielefeld_all_x2_terms.pdf", width = 16, height = 11)
grid.draw(combine_Gam_Bielefeld_all_x2_terms) 
dev.off()

# response
p1 <- plot.GamQap.Pub(results = Original_all_all_x2, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "response", 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength"), 
                      xLabels = list("", "", "", ""))
p2 <- plot.GamQap.Pub(results = Original_interd_all_x2, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "response",
                      xLabels = list("", "", "", ""))
p3 <- plot.GamQap.Pub(results = Original_inst_all_x2, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "response", 
                      xLabels = list("Distance in m",
                                     "Distance in m",
                                     bquote(hat(beta)[binPub]),
                                     bquote(hat(beta)[poisPub])))

combine_Gam_Bielefeld_all_x2_response <- grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                   arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                   arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                   leg, 
                                                   heights = c(2.5, 2, 2, 0.6))

pdf("Graphics/Bielefeld/combine_Gam_Bielefeld_all_x2_response.pdf", width = 16, height = 11)
grid.draw(combine_Gam_Bielefeld_all_x2_response) 
dev.off()


### main buildings
load("Workspaces/server/Original_all_main_x2_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_interd_main_x2_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_inst_main_x2_wrapper_Bielefeld.RData")

# terms 
p4 <- plot.GamQap.Pub(results = Original_all_main_x2, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "terms", 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength"),
                      xLabels = list("", "", "", ""))
p5 <- plot.GamQap.Pub(results = Original_interd_main_x2, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "terms",
                      xLabels = list("", "", "", ""))
p6 <- plot.GamQap.Pub(results = Original_inst_main_x2, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "terms",
                      xLabels = list("Distance in m",
                                        "Distance in m",
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub])))

combine_Gam_Bielefeld_main_x2_terms <- grid.arrange(arrangeGrob(p4, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                              arrangeGrob(p5, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                              arrangeGrob(p6, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                              leg, 
                                              heights = c(2.5, 2, 2, 0.6))

pdf("Graphics/Bielefeld/combine_Gam_Bielefeld_main_x2_terms.pdf", width = 16, height = 11)
grid.draw(combine_Gam_Bielefeld_main_x2_terms) 
dev.off()

# response
p4 <- plot.GamQap.Pub(results = Original_all_main_x2, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "response", 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength"),
                      xLabels = list("", "", "", ""))
p5 <- plot.GamQap.Pub(results = Original_interd_main_x2, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "response",
                      xLabels = list("", "", "", ""))
p6 <- plot.GamQap.Pub(results = Original_inst_main_x2, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "response",
                      xLabels = list("Distance in m",
                                     "Distance in m",
                                     bquote(hat(beta)[binPub]),
                                     bquote(hat(beta)[poisPub])))

combine_Gam_Bielefeld_main_x2_response <- grid.arrange(arrangeGrob(p4, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                    arrangeGrob(p5, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                    arrangeGrob(p6, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                    leg, 
                                                    heights = c(2.5, 2, 2, 0.6))

pdf("Graphics/Bielefeld/combine_Gam_Bielefeld_main_x2_response.pdf", width = 16, height = 11)
grid.draw(combine_Gam_Bielefeld_main_x2_response) 
dev.off()



#------------------------
# three covariates
### all buildings
load("Workspaces/server/Original_all_all_x3_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_interd_all_x3_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_inst_all_x3_wrapper_Bielefeld.RData")


# terms
p1 <- plot.GamQap.Pub(results = Original_all_all_x3, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "terms", 
                      plotTitles = list(bquote(hat(beta)[binDist]),
                                        bquote(hat(beta)[poisDist]),
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub]),
                                        bquote(hat(beta)[binCat]),
                                        bquote(hat(beta)[poisCat])),
                      xLabels = list("", "", "", "", "", ""))
p2 <- plot.GamQap.Pub(results = Original_interd_all_x3, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "terms",
                      xLabels = list("", "", "", "", "", ""))
p3 <- plot.GamQap.Pub(results = Original_inst_all_x3, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "terms")

combine_Gam_Bielefeld_all_x3_terms <- grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                             arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                             arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                             leg, 
                                             heights = c(2.5, 2, 2, 0.6))

pdf("Graphics/Bielefeld/combine_Gam_Bielefeld_all_x3_terms.pdf", width = 16, height = 11)
grid.draw(combine_Gam_Bielefeld_all_x3_terms) 
dev.off()

# response
p1 <- plot.GamQap.Pub(results = Original_all_all_x3, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "response", 
                      plotTitles = list(bquote(hat(beta)[binDist]),
                                        bquote(hat(beta)[poisDist]),
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub]),
                                        bquote(hat(beta)[binCat]),
                                        bquote(hat(beta)[poisCat])),
                      xLabels = list("", "", "", "", "", ""))
p2 <- plot.GamQap.Pub(results = Original_interd_all_x3, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "response",
                      xLabels = list("", "", "", "", "", ""))
p3 <- plot.GamQap.Pub(results = Original_inst_all_x3, rug = TRUE, distance = 1000, xTitles = c("Distance", "numPub"), display = "response")

combine_Gam_Bielefeld_all_x3_response <- grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                   arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                   arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                   leg, 
                                                   heights = c(2.5, 2, 2, 0.6))

pdf("Graphics/Bielefeld/combine_Gam_Bielefeld_all_x3_response.pdf", width = 16, height = 11)
grid.draw(combine_Gam_Bielefeld_all_x3_response) 
dev.off()


### main building
load("Workspaces/server/Original_all_main_x3_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_interd_main_x3_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_inst_main_x3_wrapper_Bielefeld.RData")

# terms
p4 <- plot.GamQap.Pub(results = Original_all_main_x3, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "terms", 
                      plotTitles = list(bquote(hat(beta)[binDist]),
                                        bquote(hat(beta)[poisDist]),
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub]),
                                        bquote(hat(beta)[binCat]),
                                        bquote(hat(beta)[poisCat])),
                      xLabels = list("", "", "", "", "", ""))
p5 <- plot.GamQap.Pub(results = Original_interd_main_x3, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "terms",
                      xLabels = list("", "", "", "", "", ""))
p6 <- plot.GamQap.Pub(results = Original_inst_main_x3, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "terms")

combine_Gam_Bielefeld_main_x3_terms <- grid.arrange(arrangeGrob(p4, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                              arrangeGrob(p5, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                              arrangeGrob(p6, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                              leg, 
                                              heights = c(2.5, 2, 2, 0.6))

pdf("Graphics/Bielefeld/combine_Gam_Bielefeld_main_x3_terms.pdf", width = 16, height = 11)
grid.draw(combine_Gam_Bielefeld_main_x3_terms) 
dev.off()


# response
p4 <- plot.GamQap.Pub(results = Original_all_main_x3, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "response", 
                      plotTitles = list(bquote(hat(beta)[binDist]),
                                        bquote(hat(beta)[poisDist]),
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub]),
                                        bquote(hat(beta)[binCat]),
                                        bquote(hat(beta)[poisCat])),
                      xLabels = list("", "", "", "", "", ""))
p5 <- plot.GamQap.Pub(results = Original_interd_main_x3, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "response",
                      xLabels = list("", "", "", "", "", ""))
p6 <- plot.GamQap.Pub(results = Original_inst_main_x3, rug = TRUE, distance = 140, xTitles = c("Distance", "numPub"), display = "response")

combine_Gam_Bielefeld_main_x3_response <- grid.arrange(arrangeGrob(p4, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                    arrangeGrob(p5, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                    arrangeGrob(p6, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                                    leg, 
                                                    heights = c(2.5, 2, 2, 0.6))

pdf("Graphics/Bielefeld/combine_Gam_Bielefeld_main_x3_response.pdf", width = 16, height = 11)
grid.draw(combine_Gam_Bielefeld_main_x3_response) 
dev.off()

#---------------------------------------------------
#---------------------------------------------------
# Helmholtz
# two covariates
load("Workspaces/server/Original_all_x2_wrapper.RData")
load("Workspaces/server/Original_interd_x2.RData")
load("Workspaces/server/Original_inst_x2.RData")
load("Graphics/plotLegend.Rdata")

# terms

p1 <- plot.GamQap.Pub(results = Original_all_x2, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "terms", 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength"),
                      xLabels = list("", "", "", ""))
p2 <- plot.GamQap.Pub(results = Original_interd_x2, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "terms",
                      xLabels = list("", "", "", ""))
p3 <- plot.GamQap.Pub(results = Original_inst_x2, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "terms",
                      xLabels = list("Distance in m",
                                     "Distance in m",
                                     bquote(hat(beta)[binPub]),
                                     bquote(hat(beta)[poisPub])))


combine_Gam_HMGU_x2_terms <- grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    leg, 
                                    heights = c(2.5, 2, 2, 0.6))

pdf("Graphics/Helmholtz/combine_Gam_HMGU_x2_terms.pdf", width = 16, height = 11)
grid.draw(combine_Gam_HMGU_x2_terms) 
dev.off()


# response
p1 <- plot.GamQap.Pub(results = Original_all_x2, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "response", 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength"),
                      xLabels = list("", "", "", ""))
p2 <- plot.GamQap.Pub(results = Original_interd_x2, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "response",
                      xLabels = list("", "", "", ""))
p3 <- plot.GamQap.Pub(results = Original_inst_x2, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "response",
                      xLabels = list("Distance in m",
                                     "Distance in m",
                                     bquote(hat(beta)[binPub]),
                                     bquote(hat(beta)[poisPub])))

combine_Gam_HMGU_x2_response <- grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    leg, 
                                    heights = c(2.5, 2, 2, 0.6))

pdf("Graphics/Helmholtz/combine_Gam_HMGU_x2_response.pdf", width = 16, height = 11)
grid.draw(combine_Gam_HMGU_x2_response) 
dev.off()



# three covariates
load("Workspaces/server/Original_all_x3_wrapper.RData")
load("Workspaces/server/Original_interd_x3.RData")
load("Workspaces/server/Original_inst_x3.RData")

# terms
p1 <- plot.GamQap.Pub(results = Original_all_x3, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "terms",
                      plotTitles = list(bquote(hat(beta)[binDist]),
                                        bquote(hat(beta)[poisDist]),
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub]),
                                        bquote(hat(beta)[binCat]),
                                        bquote(hat(beta)[poisCat])),
                      xLabels = list("", "", "", "", "", ""))
p2 <- plot.GamQap.Pub(results = Original_interd_x3, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "terms",
                      xLabels = list("", "", "", "", "", ""))
p3 <- plot.GamQap.Pub(results = Original_inst_x3, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "terms")

combine_Gam_HMGU_x3_terms <- grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    leg, 
                                    heights = c(2.5, 2, 2, 0.6))

pdf("Graphics/Helmholtz/combine_Gam_HMGU_x3_terms.pdf", width = 19, height = 11)
grid.draw(combine_Gam_HMGU_x3_terms) 
dev.off()

# response
p1 <- plot.GamQap.Pub(results = Original_all_x3, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "response",
                      plotTitles = list(bquote(hat(beta)[binDist]),
                                        bquote(hat(beta)[poisDist]),
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub]),
                                        bquote(hat(beta)[binCat]),
                                        bquote(hat(beta)[poisCat])),
                      xLabels = list("", "", "", "", "", ""))
p2 <- plot.GamQap.Pub(results = Original_interd_x3, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "response",
                      xLabels = list("", "", "", "", "", ""))
p3 <- plot.GamQap.Pub(results = Original_inst_x3, rug = TRUE, distance = 850, xTitles = c("Distance", "numPub"), display = "response")

combine_Gam_HMGU_x3_response <- grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    leg, 
                                    heights = c(2.5, 2, 2, 0.6))

pdf("Graphics/Helmholtz/combine_Gam_HMGU_x3_response.pdf", width = 19, height = 11)
grid.draw(combine_Gam_HMGU_x3_response) 
dev.off()


