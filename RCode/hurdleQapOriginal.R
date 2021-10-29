# load required packages ----
library(statnet)
library(mgcv)
library(countreg)

## hurdleQapOriginal -------------------------------------------

#---------------------------------------------------------------------------------------
#### Input Data

# y: matrix with count target variable
# x: list of matrices
#     - first matrix should be distance matrix
#     - following matrices are further covariates
# removeControl: is there data which should not be considered? TRUE/FALSE
# logicMatrix: if removeControl = TRUE: logical matrix [which data should not be considered? observation which should be removed are TRUE]
# maxDist: define a maximum distance (in unit of distance matrix x[[1]]), only distances smaller than dist are considered
# kbasis: number of basis functions in bam (used only for first element of x (distance))

hurdleQapOriginal <- function(y, x, removeControl = FALSE, logicMatrix = NULL, maxDist, kbasis){
  # keep original data
  y_orig <- y
  x_orig <- x

  # prepare lists for results
  nCovs <- length(x)
  zero <- count <- vector(mode = "list", length = nCovs)
  names(zero) <- names(count) <- paste0("cov", 1:nCovs)


  #----- data preparation

  # logicControl: which data should not be considered?
  # e.g. authors from same institute
  if(removeControl == TRUE){
    y[which(logicMatrix  == TRUE)] <- NA
    for(i in 1:length(x)){
      x[[i]][which(logicMatrix  == TRUE)] <- NA
    }
  }
  # Set all observations with distance < given maxDist to NA
  y[which(x[[1]] > maxDist)] <- NA
  for(i in 1:length(x)){
    x[[i]][which(x[[1]] > maxDist)] <- NA
  }

  # vectorize all matrices
  y_vec <-  as.numeric(sna::gvectorize(y, mode = "graph", diag = FALSE, censor.as.na = FALSE))
  x_vec <- list()
  for(i in 1:nCovs){
    x_vec[[i]] <-  as.numeric(sna::gvectorize(x[[i]], mode = "graph", diag = FALSE, censor.as.na = FALSE))
  }

  # remove observations (in y_vec and x_vec) which have missing data in x_vec[[1]] (distance data)
  # in this way we remove all observations which have missing distance values or are from the same institute
  y_vec <- y_vec[which(!is.na(x_vec[[1]]))]
  for(i in 1:length(x_vec)){
    x_vec[[i]] <- x_vec[[i]][which(!is.na(x_vec[[1]]))]
  }

  # dichtomize publication data (for binary model)
  y_bin <- as.factor(ifelse(y_vec == 0, 0, 1))
  x_bin <- x_vec
  # keep only observations which have at least one publication (for poisson model)
  x_pois <- x_vec
  for(i in 1:nCovs){
    x_pois[[i]] <- x_pois[[i]][which(y_vec != 0)]
  }
  y_pois <- y_vec[which(y_vec != 0)]


  #------- original models
  for(i in 1:nCovs){
    assign(paste0("xbin_",i), x_bin[[i]])
    assign(paste0("xpois_",i), x_pois[[i]])
  }
  xnam_bin <- paste0("xbin_", 1:nCovs)
  xnam_pois <- paste0("xpois_", 1:nCovs)

  # Two-step model instead of classical hurlde
  # GLM with binomial family: is there a collaboration between two authors or not (independent of number of papers)
  modelBin <- glm(as.formula(paste("factor(y_bin) ~ ", paste(xnam_bin, collapse= "+"))),
                  family = binomial(link = "cloglog"))
  # GLM with ztpoisson family only on data with at least one publication: answer questions "how many publications"
  modelPois <-  glm(as.formula(paste("y_pois ~ ", paste(xnam_pois, collapse= "+"))),
                    family = countreg::ztpoisson)


  # GAM with binomial family: is there a collaboration between two authors or not (independent of number of papers)
  # GAM with ztpoisson family only on data with at least one publication: answer questions "how many publications"

  if(nCovs > 1) {
    modelGamBin <- mgcv::bam(as.formula(paste("factor(y_bin) ~ ", paste("s(xbin_1, bs = 'ps', k = kbasis) +",
                                                                  paste(xnam_bin[2:nCovs], collapse= "+")))),
                       family = binomial(link = "cloglog"))
    modelGamPois <-  mgcv::bam(as.formula(paste("y_pois ~ ", paste("s(xpois_1, bs = 'ps', k = kbasis) +", paste(xnam_pois[2:nCovs], collapse= "+")))),
                         family = ztpoisson)
  } else{
    modelGamBin <- mgcv::bam(as.formula(paste("factor(y_bin) ~ ", paste("s(xbin_1, bs = 'ps', k = kbasis)"))),
                       family = binomial(link = "cloglog"))
    modelGamPois <-  mgcv::bam(as.formula(paste("y_pois ~ ", paste("s(xpois_1, bs = 'ps', k = kbasis)"))),
                         family = countreg::ztpoisson)
  }


  # keep model coefficients for modelBin and modelPois

  for(i in 1:nCovs){
    zero[[i]]$testval <- modelBin$coefficients[(i+1)]
    count[[i]]$testval <- modelPois$coefficients[(i+1)]
  }

  # smoothing parameter from original model
  smPar <- modelGamPois$sp
  # set maximum distance to maximum distance of fit
  distance <- max(max(xbin_1, na.rm = TRUE), max(xpois_1, na.rm = TRUE)) + 1

  #------- data for plot
  # create dataset for prediction (needed for confidence intervals)
  ## Binomial
  data_orig_Bin <- data.frame(y_bin)
  for(i in 1:nCovs){
    data_orig_Bin[,(i+1)] <- x_bin[[i]]
    colnames(data_orig_Bin)[i+1] <- paste0("xbin_", i)
  }
  data_orig_Bin$y_bin <- as.factor(data_orig_Bin$y_bin)

  # prediction data
  pred_data_Bin <- data_orig_Bin[1:300,]

  for(j in 1:ncol(pred_data_Bin)){
    if(class(pred_data_Bin[[j]]) == "factor") {
      pred_data_Bin[[j]] <- as.factor(rep(levels(data_orig_Bin[[j]]), length.out = nrow(pred_data_Bin)))
    }
    if(class(pred_data_Bin[[j]]) %in% c("integer", "numeric")) {
      pred_data_Bin[[j]] <- seq(from = min(data_orig_Bin[[j]], na.rm = TRUE), to = max(data_orig_Bin[[j]], na.rm = TRUE), length = nrow(pred_data_Bin))
    }
  }

  ## Poisson
  data_orig_Pois <- data.frame(y_pois)
  for(i in 1:nCovs){
    data_orig_Pois[,(i+1)] <- x_pois[[i]]
    colnames(data_orig_Pois)[i+1] <- paste0("xpois_", i)
  }
  data_orig_Pois$y_pois <- as.factor(data_orig_Pois$y_pois)

  # prediction data
  pred_data_Pois <- data_orig_Pois[1:300,]

  for(j in 1:ncol(pred_data_Pois)){
    if(class(pred_data_Pois[[j]]) == "factor") {
      pred_data_Pois[[j]] <- as.factor(rep(levels(data_orig_Pois[[j]]), length.out = nrow(pred_data_Pois)))
    }
    if(class(pred_data_Pois[[j]]) %in% c("integer", "numeric")) {
      pred_data_Pois[[j]] <- seq(from = min(data_orig_Pois[[j]], na.rm = TRUE), to = max(data_orig_Pois[[j]], na.rm = TRUE), length = nrow(pred_data_Pois))
    }
  }
  # prediction with transformation, x1_pred, all other covariates as mean value
  cov_mean <- unlist(lapply(x_orig[2:length(x_orig)], function(x) mean(x, na.rm = TRUE)))

  pred_data_Bin_mean <- pred_data_Bin
  pred_data_Pois_mean <- pred_data_Pois

  if(nCovs > 1){
    for(i in 1:(nCovs-1)){
      pred_data_Bin_mean[,i+2] <- cov_mean[i]
      pred_data_Pois_mean[,i+2] <- cov_mean[i]
    }
  }

  # terms

  #glm
  pred_Orig_Bin_terms_Glm <- predict(modelBin, newdata = pred_data_Bin, type = "terms", se.fit = TRUE)
  pred_Orig_Pois_terms_Glm <- predict(modelPois, newdata = pred_data_Pois, type = "terms", se.fit = TRUE)
  temp_fit_bin_glm <- as.data.frame(pred_Orig_Bin_terms_Glm$fit)
  temp_fit_pois_glm <- as.data.frame(pred_Orig_Pois_terms_Glm$fit)
  temp_se_bin_glm <- as.data.frame(pred_Orig_Bin_terms_Glm$se.fit)
  temp_se_pois_glm <- as.data.frame(pred_Orig_Pois_terms_Glm$se.fit)


  #gam
  pred_Orig_Bin_terms <- predict(modelGamBin, newdata = pred_data_Bin, type = "terms", se.fit = TRUE)
  pred_Orig_Pois_terms <- predict(modelGamPois, newdata = pred_data_Pois, type = "terms", se.fit = TRUE)
  temp_fit_bin <- as.data.frame(pred_Orig_Bin_terms$fit)
  temp_fit_pois <- as.data.frame(pred_Orig_Pois_terms$fit)
  temp_se_bin <- as.data.frame(pred_Orig_Bin_terms$se.fit)
  temp_se_pois <- as.data.frame(pred_Orig_Pois_terms$se.fit)


  # response
  # glm
  pred_Orig_Bin_resp_Glm <- predict(modelBin, newdata = pred_data_Bin_mean, type = "response", se.fit = TRUE)
  pred_Orig_Pois_resp_Glm <- predict(modelPois, newdata = pred_data_Pois_mean, type = "response", se.fit = TRUE)
  #gam
  pred_Orig_Bin_resp_Gam <- predict(modelGamBin, newdata = pred_data_Bin_mean, type = "response", se.fit = TRUE)
  pred_Orig_Pois_resp_Gam <- predict(modelGamPois, newdata = pred_data_Pois_mean, type = "response", se.fit = TRUE)

  # plotData
  # terms

  plotData_origBin_terms_Glm <- list(x = pred_data_Bin$xbin_1,
                                 fit = temp_fit_bin_glm$`xbin_1`,
                                 se = temp_se_bin_glm$`xbin_1`)

  plotData_origPois_terms_Glm <- list(x = pred_data_Pois$xpois_1,
                                  fit = temp_fit_pois_glm$`xpois_1`,
                                  se = temp_se_pois_glm$`xpois_1`)


  plotData_origBin_terms <- list(x = pred_data_Bin$xbin_1,
                                 fit = temp_fit_bin$`s(xbin_1)`,
                                 se = temp_se_bin$`s(xbin_1)`)

  plotData_origPois_terms <- list(x = pred_data_Pois$xpois_1,
                                  fit = temp_fit_pois$`s(xpois_1)`,
                                  se = temp_se_pois$`s(xpois_1)`)

  # response
  plotData_origBin_resp_Glm <- list(x = pred_data_Bin_mean$xbin_1,
                           fit = as.numeric(pred_Orig_Bin_resp_Glm$fit),
                           se = as.numeric(pred_Orig_Bin_resp_Glm$se.fit))

  plotData_origBin_resp_Gam <- list(x = pred_data_Bin_mean$xbin_1,
                               fit = as.numeric(pred_Orig_Bin_resp_Gam$fit),
                               se = as.numeric(pred_Orig_Bin_resp_Gam$se.fit))

  plotData_origPois_resp_Glm <- list(x = pred_data_Pois_mean$xpois_1,
                               fit = as.numeric(pred_Orig_Pois_resp_Glm$fit),
                               se = as.numeric(pred_Orig_Pois_resp_Glm$se.fit))

  plotData_origPois_resp_Gam <- list(x = pred_data_Pois_mean$xpois_1,
                               fit = as.numeric(pred_Orig_Pois_resp_Gam$fit),
                               se = as.numeric(pred_Orig_Pois_resp_Gam$se.fit))

  #----- Results
  resultsOriginal <- list()
  resultsOriginal$data_orig_Pois <- data_orig_Pois
  resultsOriginal$data_orig_Bin <- data_orig_Bin
  resultsOriginal$modelBin <- modelBin
  resultsOriginal$modelPois <- modelPois
  resultsOriginal$modelGamBin <- modelGamBin
  resultsOriginal$modelGamPois <- modelGamPois
  resultsOriginal$smPar <- smPar
  resultsOriginal$maxDist <- distance
  resultsOriginal$kbasis <- kbasis
  resultsOriginal$count <- count
  resultsOriginal$zero <- zero
  resultsOriginal$y_orig <- y_orig
  resultsOriginal$x_orig <- x_orig
  resultsOriginal$y_prep <- y
  resultsOriginal$x_prep <- x
  resultsOriginal$pred_data_Pois <- pred_data_Pois
  resultsOriginal$pred_data_Bin <- pred_data_Bin
  resultsOriginal$pred_data_Pois_mean <- pred_data_Pois_mean
  resultsOriginal$pred_data_Bin_mean <- pred_data_Bin_mean
  resultsOriginal$plotData_origBin_terms <- plotData_origBin_terms
  resultsOriginal$plotData_origPois_terms <- plotData_origPois_terms
  resultsOriginal$plotData_origBin_terms_Glm <- plotData_origBin_terms_Glm
  resultsOriginal$plotData_origPois_terms_Glm <- plotData_origPois_terms_Glm
  resultsOriginal$plotData_origBin_resp_Glm <- plotData_origBin_resp_Glm
  resultsOriginal$plotData_origBin_resp_Gam <- plotData_origBin_resp_Gam
  resultsOriginal$plotData_origPois_resp_Glm <- plotData_origPois_resp_Glm
  resultsOriginal$plotData_origPois_resp_Gam <- plotData_origPois_resp_Gam
  resultsOriginal$nCovs <- nCovs
  return(resultsOriginal)
}


