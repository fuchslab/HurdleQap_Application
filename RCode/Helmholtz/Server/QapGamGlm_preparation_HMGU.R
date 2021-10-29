library(pscl)
library(statnet)
library(mgcv)
library(countreg)
library(rlist)

load(paste0("Workspaces/CompleteAnalysis_HMGU.Rdata"))

# Prepare data and matrices
out2 <- out
# vector of Helmholtz authors which have at least one collaboration (use only those for analysis)
colAuthors <- unique(c(out$Author, out$Collaborator))

out2$Author <- unlist(lapply(out$Author,  function(x) which(colAuthors == x)))
out2$Collaborator <- unlist(lapply(out$Collaborator,  function(x) which(colAuthors == x)))


# total Papers per Institute
totalPaperPI <- unlist(lapply(IPP, unique))
sort(table(totalPaperPI))
# institutes without publications (included in FacDistMe)
rownames(FacDistMe)[which(rownames(FacDistMe) %in% names(table(totalPaperPI)) == FALSE)]

# create matrices which contain number of publications, weight and dist for all combinations of two authors
a <- length(colAuthors)
dist <- matInst <- matCat <- locMat <- matrix(NA, a, a)
mat <- weight <- meanPub <- matrix(0, a, a)

colnames(mat) <- rownames(mat) <- colnames(matInst) <- rownames(matInst) <-  colnames(dist) <- colnames(matCat) <- rownames(matCat) <- 
  rownames(dist) <- colnames(weight) <- rownames(weight) <- colnames(locMat) <- rownames(locMat) <- 
  colnames(meanPub) <- rownames(meanPub) <- colAuthors[1:a]

# Number of papers and coauthorship index
for (k in 1:nrow(out)){
  mat[out$Author[k], out$Collaborator[k]] <-  out[k, "NrCommonPapers"]
  weight[out$Author[k], out$Collaborator[k]] <-  out[k, "weight"]
}

facultiesHMGU <- unique(totalPaperPI)[!is.na(unique(totalPaperPI))]

# matrix for meanPublications of two institutes
meanPubFac <- matrix(NA, ncol = length(facultiesHMGU), nrow = length(facultiesHMGU))
colnames(meanPubFac) <- rownames(meanPubFac) <- facultiesHMGU
for (i in 1:nrow(meanPubFac)){
  for (j in 1:nrow(meanPubFac)){
    if(rownames(meanPubFac)[i] %in% names(table(totalPaperPI)) &
       rownames(meanPubFac)[j] %in% names(table(totalPaperPI))){
      meanPubFac[i, j] <- mean(c(table(totalPaperPI)[rownames(meanPubFac)[i]], 
                                 table(totalPaperPI)[rownames(meanPubFac)[j]]), na.rm = TRUE)
    } else if(!(rownames(meanPubFac)[i] %in% names(table(totalPaperPI))) &
              !(rownames(meanPubFac)[j] %in% names(table(totalPaperPI)))){
      meanPubFac[i, j] <- 0
    } else {
      meanPubFac[i, j] <- mean(c(table(totalPaperPI)[rownames(meanPubFac)[i]], 
                                 table(totalPaperPI)[rownames(meanPubFac)[j]]), na.rm = TRUE)/2
    }
  }
}


# distance, institute and meanPub matrix
for (i in 1:a){
  for (j in  1:a){
    dist[i, j] <- FacDistMe[maxInst[colAuthors[i]], maxInst[colAuthors[j]]]
    # matrix which indicates if two authors are from the same institute/category
    matInst[i, j] <- ifelse(as.character(maxInst[colAuthors[i]]) == as.character(maxInst[colAuthors[j]]), 1, 0)
    # matrix which indicates if two authors are from the same category
    matCat[i, j] <- ifelse(allCat[names(topics[which(names(topics) == maxInst[colAuthors[i]])])] == allCat[names(topics[which(names(topics) == maxInst[colAuthors[j]])])], 1, 0)
    # matrix with mean publications of instututes per author pair
    meanPub[i, j] <- meanPubFac[maxInst[colAuthors[i]], maxInst[colAuthors[j]]]
  }
}

GebKnoten$Location[GebKnoten$Abbreviation == "DES"] <- NA
# location per author
locPA <- list()
for (i in 1:a){
  locPA[[i]] <- unique(c(GebKnoten[GebKnoten$Abbreviation == maxInstHelm[[colAuthors[i]]], "Location"])$Location)
  if(length(locPA[[i]]) < 1) {locPA[[i]] <- NA}
}
names(locPA) <- colAuthors[1:a]

# locations for each author pair 
for (i1 in 1:a){    
  for (j1 in 1:a){
    dists <- c()
    if(!is.na(locPA[i1]) & !is.na(locPA[j1])){
      for (d in unlist(locPA[i1])) {
        for (b in unlist(locPA[j1])) {
          db <- data2[d, b]
          names(db) <- paste0(d, "-", b)
          dists <- c(dists, db)
        }
      }    
      locMat[i1,j1] <-  names(which.min(dists))
    }
  }
}

locMat[which(locMat == "Grosshadern2-Neuherberg")] <- "Neuherberg-Grosshadern2"
locMat[which(locMat == "Grosshadern1-Neuherberg")] <- "Neuherberg-Grosshadern1"
locMat[which(locMat == "Muenchen1-Neuherberg")] <- "Neuherberg-Muenchen1"
locMat[which(locMat == "Muenchen2-Neuherberg")] <- "Neuherberg-Muenchen2"
locMat[which(locMat == "Muenchen3-Neuherberg")] <- "Neuherberg-Muenchen3"
locMat[which(locMat == "Muenchen4-Neuherberg")] <- "Neuherberg-Muenchen4"
locMat[which(locMat == "Muenchen1-Grosshadern1")] <- "Grosshadern1-Muenchen1"
locMat[which(locMat == "Muenchen1-Grosshadern2")] <- "Grosshadern2-Muenchen1"
locMat[which(locMat == "Muenchen2-Grosshadern1")] <- "Grosshadern1-Muenchen2"
locMat[which(locMat == "Muenchen2-Grosshadern2")] <- "Grosshadern2-Muenchen2"
locMat[which(locMat == "Muenchen3-Grosshadern1")] <- "Grosshadern1-Muenchen3"
locMat[which(locMat == "Muenchen3-Grosshadern2")] <- "Grosshadern2-Muenchen3"
locMat[which(locMat == "Muenchen4-Grosshadern1")] <- "Grosshadern1-Muenchen4"
locMat[which(locMat == "Muenchen4-Grosshadern2")] <- "Grosshadern2-Muenchen4"
locMat[which(locMat == "Grosshadern2-Grosshadern1")] <- "Grosshadern1-Grosshadern2"

sort(table(locMat))

# symmetric structure
mat[lower.tri(mat, diag = TRUE)] <- t(mat)[lower.tri(mat, diag = TRUE)]
dist[lower.tri(dist, diag = TRUE)] <- t(dist)[lower.tri(dist, diag = TRUE)]
weight[lower.tri(weight, diag = TRUE)] <- t(weight)[lower.tri(weight, diag = TRUE)]
locMat[lower.tri(locMat, diag = TRUE)] <- t(locMat)[lower.tri(locMat, diag = TRUE)]
matCat[lower.tri(matCat, diag = TRUE)] <- t(matCat)[lower.tri(matCat, diag = TRUE)]
matInst[lower.tri(matInst, diag = TRUE)] <- t(matInst)[lower.tri(matInst, diag = TRUE)]
meanPub[lower.tri(meanPub, diag = TRUE)] <- t(meanPub)[lower.tri(meanPub, diag = TRUE)]


### Institute data
# sum of total papers for two connected institutes
for (i in 1:nrow(outInst)){
  outInst$SumTotalPapers[i] <-  sum(table(totalPaperPI)[outInst$Institute[i]],
                                    table(totalPaperPI)[outInst$CoInstitute[i]])
}

# Prepare data and matrices
outInst2 <- outInst
# vector of Bielefeld authors which have at least one collaboration (use only those for analysis)
colInst <- unique(c(outInst$Institute, outInst$CoInstitute))

outInst2$Institute <- unlist(lapply(outInst$Institute,  function(x) which(colInst == x)))
outInst2$CoInstitute <- unlist(lapply(outInst$CoInstitute,  function(x) which(colInst == x)))

# create matrices which contain number of publications, weight and dist for all combinations of two authors
b <- length(colInst)
matI <- sumPapI <- locMatI <- matCatI <- matrix(NA, b, b)
meanPubI <- matrix(0, b, b)

colnames(matI) <- rownames(matI) <- colnames(sumPapI) <- rownames(sumPapI) <- 
  colnames(locMatI) <- rownames(locMatI) <- colnames(meanPubI) <- rownames(meanPubI) <- colnames(matCatI) <- rownames(matCatI) <- colInst[1:b]

for (i in 1:b){
  for (j in  1:b){
    val1 <- outInst2[outInst2$Institute == i & outInst2$CoInstitute == j, "NrCommonPapers"]
    val2 <- outInst2[outInst2$Institute == i & outInst2$CoInstitute == j, "SumTotalPapers"]
    ifelse(length(val1) > 0, matI[i,j] <- val1, matI[i,j] <- 0)
    ifelse(length(val2) > 0, sumPapI[i,j] <- val2, sumPapI[i,j] <- 0)
    # matrix which indicates if two authors are from the same category
    matCatI[i,j] <- ifelse(allCat[colnames(matCatI)[i]] == allCat[colnames(matCatI)[j]], 1, 0)
  }
}

colInst <- sort(colInst)
# location per institute
locPI <- list()
for(i in 1:length(colInst)){
  l <- unique(GebKnoten[GebKnoten$Abbreviation == colInst[i], "Location"])
  locPI[[i]] <- as.matrix(l)[,1]
}
names(locPI) <- colInst
locPI["DES"] <- NA
locPI["HPC"] <- "Neuherberg"
locPI["Mixed"] <- NA

# locations for each institute pair 
for (i1 in 1:b){    
  for (j1 in 1:b){
    meanPubI[i1, j1] <- meanPubFac[colInst[i1], colInst[j1]]
    dists <- c()
    if(!is.na(locPI[i1]) & !is.na(locPI[j1])){
      for (d in unlist(locPI[i1])) {
        for (g in unlist(locPI[j1])) {
          dg <- data2[d, g]
          names(dg) <- paste0(d, "-", g)
          dists <- c(dists, dg)
        }
      }    
      locMatI[i1,j1] <-  names(which.min(dists))
    }
  }
}

# meanPub matrix
meanPubI <- meanPubFac[colInst, colInst]



# symmetric structure
sumPapI[lower.tri(sumPapI, diag = TRUE)] <- t(sumPapI)[lower.tri(sumPapI, diag = TRUE)]
matI[lower.tri(matI, diag = TRUE)] <- t(matI)[lower.tri(matI, diag = TRUE)]
locMatI[lower.tri(locMatI, diag = TRUE)] <- t(locMatI)[lower.tri(locMatI, diag = TRUE)]
matCatI[lower.tri(matCatI, diag = TRUE)] <- t(matCatI)[lower.tri(matCatI, diag = TRUE)]
meanPubI[lower.tri(meanPubI, diag = TRUE)] <- t(meanPubI)[lower.tri(meanPubI, diag = TRUE)]

a <- length(colAuthors)
b <- length(colInst)

save.image("Workspaces/QAPGamGlm_HMGU_zwischen.RData")

rm(list = setdiff(ls(), c("mat", "dist", "locMat", "meanPub", "matInst", "matInstLogic", "matCat", "matCatI",
                          "matI", "locMatI", "meanPubI", "sumPapI", "FacDistMe", "colInst")))
save.image("Workspaces/Permutations/EverythingNeeded_QapGamFunction1_HMGU.RData")


# Total function for all steps (original models, all permutations, data combination) in one function
# alternative for fatser results, use the three R-files Test_Split_QapGamFunction1.R, 
# Test_Split_QapGamFunction2.R and Test_Split_QapGamFunction3.R

'# Function GamHurdleQap

# dat: should be an array with five (symmatric) matrices with following order (publications, distances, locations, meanPub, sameInstitute)
# distance: only distances smaller than dist are considered
# reps: number of permutations
# kbasis: number of basis functions in gam
# location: which location should be considered in the models?
# logicMat: if TRUE, only distances between different institutes are considered
# xAdd: additional x variable is added (here: meanPub)


GamHurdleQap <- function(dat, distance, reps, seed = 123, kbasis, location = NULL, logicMat = FALSE, xAdd = FALSE){
  qaplist <- list()
  zero1 <- count1 <- list()
  if(xAdd == TRUE) {zero2 <-  count2 <- list()}
  # prepare data for modeling
  # set author pairs which are from the same institute to NA
  if(logicMat == TRUE){
    dat[1,,][which(dat[5,,] == TRUE)] <- NA
    dat[2,,][which(dat[5,,] == TRUE)] <- NA
    dat[3,,][which(dat[5,,] == TRUE)] <- NA
    dat[4,,][which(dat[5,,] == TRUE)] <- NA
  }
  # vectorize publication, distance and location matrices
  y <-  as.numeric(gvectorize(dat[1,,], mode = "graph", diag = FALSE, censor.as.na = FALSE))
  x1 <- as.numeric(gvectorize(dat[2,,], mode = "graph", diag = FALSE, censor.as.na = FALSE))
  xLoc <- as.factor(gvectorize(dat[3,,], mode = "graph", diag = FALSE, censor.as.na = FALSE))
  xPub <- as.integer(gvectorize(dat[4,,], mode = "graph", diag = FALSE, censor.as.na = FALSE))
  
  # keep only specific locations
  if (!is.null(location)){
    ind <- c()
    for (i in 1:length(location)){
      ind <- c(ind, which(xLoc == location[i]))
    }
    y <- y[ind]
    x1 <- x1[ind]
    xLoc <- xLoc[ind]
    xPub <- xPub[ind]
  }
  
  # keep only distances smaller than distance
  y <- y[which(x1 <= distance)]
  x1 <- x1[which(x1 <= distance)]
  xPub <- xPub[which(x1 <= distance)]
  
  # remove observations (in y and x) which have missing data in x1 (distance data)
  # in this way we remove all observations which have missing distance values or are from the same institute
  y <- y[which(!is.na(x1))] 
  x1 <- x1[which(!is.na(x1))]
  xPub <- xPub[which(!is.na(x1))]
  
  # dichtomize publication data (for binary model)
  y_bin <- as.factor(ifelse(y == 0, 0, 1))
  # keep only observations which have at least one publication (for poisson model)
  x2 <- x1[which(y != 0)]
  xPub2 <- xPub[which(y != 0)]
  y2 <- y[which(y != 0)]
  
  ### original models
  if (xAdd == TRUE){
    # Two-step model instead of classical hurlde
    # GLM with binomial family: is there a collaboration between two authors or not (independent of number of papers)
    modelBin <- glm(factor(y_bin) ~ x1 + xPub, family = binomial(link = "cloglog"))
    # GLM with ztpoisson family only on data with at least one publication: answer questions "how many publications"
    modelPois <-  glm(y2 ~ x2 + xPub2, family = ztpoisson)
    # GAM with ztpoisson family only on data with at least one publication: answer questions "how many publications"
    modelGamPois <-  gam(y2 ~ s(x2, bs = "ps", k = kbasis) + xPub2, family = ztpoisson)
  } else {
    # Two-step model instead of classical hurlde
    # GLM with binomial family: is there a collaboration between two authors or not (independent of number of papers)
    modelBin <- glm(factor(y_bin) ~ x1, family = binomial(link = "cloglog"))
    # GLM with ztpoisson family only on data with at least one publication: answer questions "how many publications"
    modelPois <-  glm(y2 ~ x2, family = ztpoisson)
    # GAM with ztpoisson family only on data with at least one publication: answer questions "how many publications"
    modelGamPois <-  gam(y2 ~ s(x2, bs = "ps", k = kbasis), family = ztpoisson)
    
  }
  
  # keep model coefficients
  zero1$testval <- modelBin$coefficients[2]
  count1$testval <- modelPois$coefficients[2]
  if(xAdd == TRUE){
    zero2$testval <- modelBin$coefficients[3]
    count2$testval <- modelPois$coefficients[3]
  }
  
  # smoothing parameter from original model
  smPar <- modelGamPois$sp
  # set maximum distance to maximum distance of fit
  distance <- max(x2) + 1
  
  #-----
  # create dataset for prediction (needed for confidence intervals)
  data_orig <- data.frame("permY2" = y2, "permX" = x2, "permXPub" = xPub2)
  data_orig$permY2 <- as.factor(data_orig$permY2)
  pred_data <- data_orig[1:300,]
  
  for(j in 1:ncol(pred_data)){
    if(class(pred_data[[j]]) == "factor") {
      pred_data[[j]] <- as.factor(rep(levels(data_orig[[j]]), length.out = nrow(pred_data)))
    }
    if(class(pred_data[[j]]) %in% c("integer", "numeric")) {
      pred_data[[j]] <- seq(from = min(data_orig[[j]]), to = max(data_orig[[j]]), length = nrow(pred_data))
    }
  }
  
  # Initialize matrix for quantiles of coefficients
  matrix_coefficients    <- matrix(NA, nrow = length(modelGamPois$coefficients), ncol = reps)
  quantiles_coefficients <- matrix(NA, ncol = 2, nrow = nrow(matrix_coefficients))
  rownames(matrix_coefficients) <-  rownames(quantiles_coefficients) <- names(modelGamPois$coefficients)
  colnames(quantiles_coefficients) <- c("2.5", "97.5")
  
  
  # Initialize list of matrices to summarize predictions for each covariate 
  # and each permutation
  if(xAdd == TRUE) {list_pred_terms <- vector("list",  ncol(data_orig)-1)
  } else {list_pred_terms <- vector("list",  ncol(data_orig)-2)}
  
  for(j in 1:length(list_pred_terms)) {
    list_pred_terms[[j]] <- matrix(NA, nrow = nrow(pred_data), ncol = reps)    
  }
  
  # prepare data list for plot
  plotData <- list()
  modPlot <- mgcv::plot.gam(modelGamPois, seWithMean = TRUE, pages = 1, select = 1, 
                            scale = -1, shift = coef(modelGamPois)[1]) #, shift = coef(x$modelGamPois)[1]
  plotData[[1]] <- list(x = modPlot[[1]]$x,
                        fit = modPlot[[1]]$fit,
                        se = modPlot[[1]]$se)
  #----
  
  ### permutated models (with fixed smoothing parameter in gam)
  #permModsGam <- list()
  predModsGam <- list()    
  zero1$dist <- count1$dist <- vector(mode = "numeric", length = reps)
  if(xAdd == TRUE){zero2$dist <-  count2$dist <- vector(mode = "numeric", length = reps)}
  
  set.seed(seed)
  for (i in 1:reps){
    # permute publication matrix; keep x and xLoc matrix as it is
    permY <- as.integer(gvectorize(rmperm(dat[1,,]), mode = "graph", diag = FALSE, censor.as.na = FALSE))
    x1 <- as.numeric(gvectorize(dat[2,,], mode = "graph", diag = FALSE, censor.as.na = FALSE))
    xLoc <- as.factor(gvectorize(dat[3,,], mode = "graph", diag = FALSE, censor.as.na = FALSE))
    xPub <- as.integer(gvectorize(dat[4,,], mode = "graph", diag = FALSE, censor.as.na = FALSE))
    
    # keep only specific locations
    if (!is.null(location)){
      ind <- c()
      for (l in 1:length(location)){
        ind <- c(ind, which(xLoc == location[l]))
      }
      permY <- permY[ind]
      x1 <- x1[ind]
      xLoc <- xLoc[ind]
      xPub <- xPub[ind]
    }
    
    # define max. distance, remove missing distances
    permY <- permY[which(x1 <= distance)]
    x1 <- x1[which(x1 <= distance)]
    xPub <- xPub[which(x1 <= distance)]
    
    permY <- permY[which(!is.na(x1))]
    xPub <- xPub[which(!is.na(x1))]
    x1 <- x1[which(!is.na(x1))]
    
    # binary publication
    y_bin <- ifelse(permY == 0, 0, 1)
    # at least one publication
    permX <- x1[which(permY != 0)]
    permXPub <- xPub[which(permY != 0)]
    permY2 <- permY[which(permY != 0)]
    
    # models
    if(xAdd == TRUE){
      mod1 <- glm(factor(y_bin) ~ x1 + xPub, family = binomial(link = "cloglog"))
      mod2 <- glm(permY2 ~ permX + permXPub, family = ztpoisson)
      mod3 <-  gam(permY2 ~ s(permX, bs = "ps", k = kbasis, sp = smPar) + permXPub, family = ztpoisson)
    } else{
      mod1 <- glm(factor(y_bin) ~ x1, family = binomial(link = "cloglog"))
      mod2 <- glm(permY2 ~ permX, family = ztpoisson)
      mod3 <-  gam(permY2 ~ s(permX, bs = "ps", k = kbasis, sp = smPar), family = ztpoisson)        
    }
    
    # save model coefficients from GLMs
    zero1$dist[i] <- mod1$coefficients[2]
    count1$dist[i] <- mod2$coefficients[2]
    if (xAdd == TRUE){
      zero2$dist[i] <- mod1$coefficients[3]
      count2$dist[i] <- mod2$coefficients[3]
    }
    
    # # save all permutated GAMs
    # permModsGam[[i]] <- mod3
    
    # Save coefficients
    matrix_coefficients[, i] <- mod3$coefficients
    
    # Calculate predictions per covariate
    pred_terms_temp <- predict(mod3, newdata = pred_data, type = "terms")
    
    # Save GAM predictions
    predModsGam[[i]] <- pred_terms_temp 
    
    # Save predictions per covariate
    for(j in 1:length(list_pred_terms)) {
      list_pred_terms[[j]][,i] <- pred_terms_temp[,j]  
    }
    
    # data list for plot
    modPlotPerm <- mgcv::plot.gam(mod3, seWithMean = TRUE, pages = 1, select = 0, scale = -1, shift = coef(mod3)[1])
    plotData[[i+1]] <- list(x = modPlotPerm[[1]]$x,
                            fit = modPlotPerm[[1]]$fit,
                            se = modPlotPerm[[1]]$se)
  }
  
  # name list_pred_terms
  names(list_pred_terms) <- colnames(pred_terms_temp)
  
  # Initialize list of matrices to save quantiles of predictions per covariate
  if(xAdd == TRUE){quantiles_pred_terms <- vector("list", ncol(data_orig)-1)}
  else{quantiles_pred_terms <- vector("list", ncol(data_orig)-2)}
  
  for(j in 1:length(quantiles_pred_terms)) {
    quantiles_pred_terms[[j]] <- matrix(NA, nrow = nrow(pred_data), ncol = 2)
    colnames(quantiles_pred_terms[[j]]) <- c("2.5", "97.5")
  }
  # name list_pred_terms
  names(quantiles_pred_terms) <- names(list_pred_terms)
  
  shifted <- "FALSE" # control if predicted values are shifted by intercept
  
  #---------------------------
  # add intercept
#   shifted <- "TRUE"
# for(j in 1: length(list_pred_terms)){
#     for(i3 in 1:reps){
#     list_pred_terms[[j]][,i3] <- list_pred_terms[[j]][,i3] + permModsGam[[i3]]$coefficients[1] # richtig???
#   }
# }
  #---------------------------
  
  # Calculate quantiles of predictions per covariate
  for(i1 in 1:nrow(pred_data)) {
    for(j in 1:length(quantiles_pred_terms)) {
      quantiles_pred_terms[[j]][i1,] <- quantile(list_pred_terms[[j]][i1,], probs = c(0.025,0.975))
    }
  }
  
  # prediction data with quantiles
  CIdata <- data.frame(x1 = pred_data$permX, 
                       x1_0.025 = quantiles_pred_terms[["s(permX)"]][,1], 
                       x1_0.975 = quantiles_pred_terms[["s(permX)"]][,2])
  if(xAdd == TRUE){
    CIdata$x2 <- pred_data$permXPub
    CIdata$x2_0.025 <- quantiles_pred_terms[["permXPub"]][,1]
    CIdata$x2_0.975 <- quantiles_pred_terms[["permXPub"]][,2]
  }
  
  # qaplist
  zero1$pgreq <- mean(as.numeric(zero1$dist >= zero1$testval))
  zero1$pleeq <- mean(as.numeric(zero1$dist <= zero1$testval))
  count1$pgreq <- mean(as.numeric(count1$dist >= count1$testval))
  count1$pleeq <- mean(as.numeric(count1$dist <= count1$testval))
  class(zero1) <- class(count1) <- c("qaptest", "qap")
  qaplist$zero1 <- zero1
  qaplist$count1 <- count1
  
  if(xAdd == TRUE){
    zero2$pgreq <- mean(as.numeric(zero2$dist >= zero2$testval))
    zero2$pleeq <- mean(as.numeric(zero2$dist <= zero2$testval))
    count2$pgreq <- mean(as.numeric(count2$dist >= count2$testval))
    count2$pleeq <- mean(as.numeric(count2$dist <= count2$testval))
    class(zero2) <- class(count2) <- c("qaptest", "qap")
    qaplist$zero2 <- zero2
    qaplist$count2 <- count2
  }
  
  qaplist$modelBin <- modelBin
  qaplist$modelPois <- modelPois
  
  # results
  results <- list()
  results$data_orig <- data_orig
  #results$pred_data <- pred_data
  results$modelBin <- modelBin
  results$modelPois <- modelPois
  results$modelGamPois <- modelGamPois
  results$smPar <- smPar
  results$plotData <- plotData
  #results$permIntercepts <- matrix_coefficients[1,]
  #results$permModsGam <- permModsGam
  #results$predModsGam <- predModsGam
  #results$list_pred_terms <- list_pred_terms
  results$CIdata <- CIdata
  results$qaplist <- qaplist
  results$distance <- distance
  results$location <- location
  results$kbasis <- kbasis
  results$reps <- reps
  results$SplinesShiftedByIntercept <- shifted
  class(results) <- "GamQap"
  return(results)
}

# with logic mat
matInstLogic <- ifelse(matInst == 0, FALSE, TRUE)  # TRUE means that two authors have the same institute
k5 <- array(dim=c(5, nrow(mat), nrow(mat)))
k5[1,,] <- mat
k5[2,,] <- dist
k5[3,,] <- locMat
k5[4,,] <- meanPub
k5[5,,] <- matInstLogic

# institute
datI <- array(dim=c(5, nrow(matI), nrow(matI)))
datI[1,,] <- matI
datI[2,,] <- FacDistMe[colInst, colInst]
datI[3,,] <- locMatI
datI[4,,] <- meanPubI
datI[5,,] <- sumPapI

save.image("Workspaces/QapGamGlm_Helmholtz_server1.Rdata")
#load("Workspaces/QapGamGlm_Helmholtz_server1.Rdata")


# dist 1000 Neuherberg
# all
# with additional covariate
set.seed(234)
QAP_all1000_xAdd <- GamHurdleQap(dat = k5, distance = 1000, reps = 10, seed = 123, 
                                 kbasis = 10, location = "Neuherberg-Neuherberg", logicMat = FALSE, xAdd = TRUE)
#without additional covariate
set.seed(234)
QAP_all1000 <- GamHurdleQap(dat = k5, distance = 1000, reps = 10, seed = 123, 
                            kbasis = 10, location = "Neuherberg-Neuherberg", logicMat = FALSE, xAdd = FALSE)
#save.image(paste0("Workspaces/QapGamGlm_Helmholtz2.Rdata"))

# interdisciplinary
# with additional covariate
set.seed(234)
QAP_interd1000_xAdd <- GamHurdleQap(dat = k5, distance = 1000, reps = 100, seed = 123, 
                                    kbasis = 10, location = "Neuherberg-Neuherberg", logicMat = TRUE, xAdd = TRUE)
#without additional covariate
set.seed(234)
QAP_interd1000 <- GamHurdleQap(dat = k5, distance = 1000, reps = 10, seed = 123, 
                               kbasis = 10, location = "Neuherberg-Neuherberg", logicMat = TRUE, xAdd = FALSE)

#save.image(paste0("Workspaces/QapGamGlm_Helmholtz3.Rdata"))


#----------------------------------------------------------------------------------------
# institute network
#load(paste0("Workspaces/HMGU_CompleteAnalysis.Rdata"))

# without additional covariable
set.seed(789)
QAP_inst10000 <- GamHurdleQap(dat = datI, dist = 1000, reps = 111, seed = 123,
                              kbasis = 10, location = "Neuherberg-Neuherberg", logicMat = FALSE, xAdd = FALSE)
# # with additional covariable
# set.seed(789)
# QAP_inst10000_xAdd <- GamHurdleQap(dat = datI, dist = 1000, reps = 111, seed = 123, 
#                                        kbasis = 10, location = "Neuherberg-Neuherberg", logicMat = FALSE, xAdd = TRUE)

rm(list=setdiff(ls(), c(
  "Year", "QAP_all1000_xAdd", "QAP_all1000", "QAP_interd1000_xAdd", "QAP_interd1000",
  "QAP_inst10000"
)))

save.image(paste0("Workspaces/QapGamGlm_Helmholtz_server.Rdata"))'



