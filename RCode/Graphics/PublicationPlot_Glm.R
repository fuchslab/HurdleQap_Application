library(ggplot2)
library(mgcv)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(rlist)

plot.GlmQap.Pub <- function(results, rug = FALSE, plotTitles = NULL, xTitles = NULL, xLabels = NULL, ...){

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
    linPlots$bin <- ggplot(testData, aes(dist)) + 
      #theme(.....) + 
      geom_density() + 
      theme_bw() + 
      geom_vline(xintercept = x_bin$qaplistGlm[[i]]$testval, linetype = "dashed") +
      geom_hline(yintercept = 0) +
      coord_cartesian(xlim = c(min(x_bin$qaplistGlm[[i]]$testval, min(x_bin$qaplistGlm[[i]]$dist))-0.0005,
                               max(x_bin$qaplistGlm[[i]]$testval, max(x_bin$qaplistGlm[[i]]$dist))+0.0005), 
                      ylim = c(0,max(density(x_bin$qaplistGlm[[i]]$dist)$y) + 10),
                      expand = FALSE) +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
      theme(axis.text = element_text(size = 17),
            plot.title = element_text(size = 20),
             axis.title.x =  element_text(size = 20),
            axis.title.y = element_blank()
      )
    
    #poisson
    testData <- data.frame(dist = x_pois$qaplistGlm[[i]]$dist)
    linPlots$pois <- ggplot(testData, aes(dist)) + 
      geom_density() + 
      theme_bw() + 
      geom_vline(xintercept = x_pois$qaplistGlm[[i]]$testval, linetype = "dashed") +
      geom_hline(yintercept = 0) +
      coord_cartesian(xlim = c(min(x_pois$qaplistGlm[[i]]$testval, min(x_pois$qaplistGlm[[i]]$dist))-0.0005,
                               max(x_pois$qaplistGlm[[i]]$testval, max(x_pois$qaplistGlm[[i]]$dist))+0.0005), 
                      ylim = c(0,max(density(x_pois$qaplistGlm[[i]]$dist)$y) + 10),
                      expand = FALSE) +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
      theme(axis.text = element_text(size = 17),
            plot.title = element_text(size = 20),
             axis.title.x =  element_text(size = 20),
            axis.title.y = element_blank()
      )
    
    plots <- list.append(plots, linPlots[[1]])
    plots <- list.append(plots, linPlots[[2]])
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

#-----------------------------------------------------------

#-----------------------------------------------------------
# Bielefeld

# two covariates
### all buildings
load("Workspaces/server/Original_all_all_x2_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_interd_all_x2_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_inst_all_x2_wrapper_Bielefeld.RData")

p1 <- plot.GlmQap.Pub(results = Original_all_all_x2, rug = FALSE, 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength"),
                      xLabels = c("", "", "", ""))

p2 <- plot.GlmQap.Pub(results = Original_interd_all_x2, rug = FALSE,
                      xLabels = c("", "", "", ""))
p3 <- plot.GlmQap.Pub(results = Original_inst_all_x2, rug = FALSE,
                      xLabels = list(bquote(hat(beta)[binDist]),
                                    bquote(hat(beta)[poisDist]),
                                    bquote(hat(beta)[binPub]),
                                    bquote(hat(beta)[poisPub])))

combineGlm_all_x2_BI <-  grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                             arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                             arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                             heights = c(2.4, 2, 2))


pdf("Grafiken/Bielefeld/combineGlm_all_x2_BI.pdf", width = 16, height = 11)
grid.draw(combineGlm_all_x2_BI) 
dev.off()

### main building
load("Workspaces/server/Original_all_main_x2_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_interd_main_x2_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_inst_main_x2_wrapper_Bielefeld.RData")

p1 <- plot.GlmQap.Pub(results = Original_all_main_x2, rug = FALSE, 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength"),
                      xLabels = c("", "", "", ""))

p2 <- plot.GlmQap.Pub(results = Original_interd_main_x2, rug = FALSE,
                      xLabels = c("", "", "", ""))
p3 <- plot.GlmQap.Pub(results = Original_inst_main_x2, rug = FALSE, 
                      xLabels = list(bquote(hat(beta)[binDist]),
                                        bquote(hat(beta)[poisDist]),
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub])))

combineGlm_main_x2_BI <-  grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                      arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                      arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                      heights = c(2.4, 2, 2))


pdf("Grafiken/Bielefeld/combineGlm_main_x2_BI.pdf", width = 16, height = 11)
grid.draw(combineGlm_main_x2_BI) 
dev.off()


# three covariates
### all buildings
load("Workspaces/server/Original_all_all_x3_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_interd_all_x3_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_inst_all_x3_wrapper_Bielefeld.RData")

p1 <- plot.GlmQap.Pub(results = Original_all_all_x3, rug = FALSE, 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength",
                                        "Binomial part\nSame Category",
                                        "Poissonian part\nSame Category"),
                      xLabels = c("", "", "", "", "", ""))

p2 <- plot.GlmQap.Pub(results = Original_interd_all_x3, rug = FALSE,
                      xLabels = c("", "", "", "", "", ""))
p3 <- plot.GlmQap.Pub(results = Original_inst_all_x3, rug = FALSE, 
                      xLabels = list(bquote(hat(beta)[binDist]),
                                        bquote(hat(beta)[poisDist]),
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub]),
                                        bquote(hat(beta)[binCat]),
                                        bquote(hat(beta)[poisCat])))

combineGlm_all_x3_BI <-  grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                      arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                      arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                      heights = c(2.4, 2, 2))


pdf("Grafiken/Bielefeld/combineGlm_all_x3_BI.pdf", width = 19, height = 11)
grid.draw(combineGlm_all_x3_BI) 
dev.off()

### main building
load("Workspaces/server/Original_all_main_x3_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_interd_main_x3_wrapper_Bielefeld.RData")
load("Workspaces/server/Original_inst_main_x3_wrapper_Bielefeld.RData")

p1 <- plot.GlmQap.Pub(results = Original_all_main_x3, rug = FALSE, 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength",
                                        "Binomial part\nSame Category",
                                        "Poissonian part\nSame Category"),
                      xLabels = c("", "", "", "", "", ""))

p2 <- plot.GlmQap.Pub(results = Original_interd_main_x3, rug = FALSE,
                      xLabels = c("", "", "", "", "", ""))
p3 <- plot.GlmQap.Pub(results = Original_inst_main_x3, rug = FALSE, 
                      xLabels = list(bquote(hat(beta)[binDist]),
                                        bquote(hat(beta)[poisDist]),
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub]),
                                        bquote(hat(beta)[binCat]),
                                        bquote(hat(beta)[poisCat])))

combineGlm_main_x3_BI <-  grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                       arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                       arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                       heights = c(2.4, 2, 2))


pdf("Grafiken/Bielefeld/combineGlm_main_x3_BI.pdf", width = 19, height = 11)
grid.draw(combineGlm_main_x3_BI) 
dev.off()

#---------------------------------------------------
#---------------------------------------------------
# Helmholtz
# two covariates
load("Workspaces/server/Original_all_x2_wrapper.RData")
load("Workspaces/server/Original_interd_x2.RData")
load("Workspaces/server/Original_inst_x2.RData")

p1 <- plot.GlmQap.Pub(results = Original_all_x2, rug = FALSE, 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength"),
                      xLabels = c("", "", "", ""))

p2 <- plot.GlmQap.Pub(results = Original_interd_x2, rug = FALSE,
                      xLabels = c("", "", "", ""))
p3 <- plot.GlmQap.Pub(results = Original_inst_x2, rug = FALSE, 
                      xLabels = list(bquote(hat(beta)[binDist]),
                                        bquote(hat(beta)[poisDist]),
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub])))

combineGlm_x2_HMGU <-  grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                      arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                      arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                      heights = c(2.4, 2, 2))


pdf("Grafiken/Helmholtz/combineGlm_x2_HMGU.pdf", width = 16, height = 11)
grid.draw(combineGlm_x2_HMGU) 
dev.off()

# three covariates
load("Workspaces/server/Original_all_x3_wrapper.RData")
load("Workspaces/server/Original_interd_x3.RData")
load("Workspaces/server/Original_inst_x3.RData")

p1 <- plot.GlmQap.Pub(results = Original_all_x3, rug = FALSE, 
                      plotTitles = list("Binomial part\nDistance",
                                        "Poissonian part\nDistance", 
                                        "Binomial part\nPublication strength",
                                        "Poissonian part\nPublication strength",
                                        "Binomial part\nSame Category",
                                        "Poissonian part\nSame Category"),
                      xLabels = c("", "", "", "", "", ""))

p2 <- plot.GlmQap.Pub(results = Original_interd_x3, rug = FALSE,
                      xLabels = c("", "", "", "", "", ""))
p3 <- plot.GlmQap.Pub(results = Original_inst_x3, rug = FALSE, 
                      xLabels = list(bquote(hat(beta)[binDist]),
                                        bquote(hat(beta)[poisDist]),
                                        bquote(hat(beta)[binPub]),
                                        bquote(hat(beta)[poisPub]),
                                        bquote(hat(beta)[binCat]),
                                        bquote(hat(beta)[poisCat])))

combineGlm_x3_HMGU <-  grid.arrange(arrangeGrob(p1, left = textGrob("Overall\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    arrangeGrob(p2, left = textGrob("Interdisciplinary\nauthor network", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    arrangeGrob(p3, left = textGrob("Institute\nnetwork", gp = gpar(fontsize = 20, fontface = 'bold'), rot = 90)), 
                                    heights = c(2.4, 2, 2))


pdf("Grafiken/Helmholtz/combineGlm_x3_HMGU.pdf", width = 19, height = 11)
grid.draw(combineGlm_x3_HMGU) 
dev.off()
