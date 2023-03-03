# ----  poissonQapOriginal

# poission model without QAP
poissonOriginal <- function(y, x, removeControl = FALSE, logicMatrix = NULL, maxDist, kbasis){
  # keep original data
  y_orig <- y
  x_orig <- x
  
  # prepare lists for results
  nCovs <- length(x)
  coefs <- vector(mode = "list", length = nCovs)
  names(coefs) <- paste0("cov", 1:nCovs)
  
  
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
  
 
  #------- original models
  for(i in 1:nCovs){
    assign(paste0("x_",i), x_vec[[i]])
  }
  xnam <- paste0("x_", 1:nCovs)
  
  # One-step model poisson
  # GLM with poisson family on whole data 
  modelPois <-  glm(as.formula(paste("y_vec ~ ", paste(xnam, collapse= "+"))),
                    family = "poisson")
  
  # GAM with poisson family on whole data 
  
  if(nCovs > 1) {
    modelGamPois <-  mgcv::bam(as.formula(paste("y_vec ~ ", paste("s(x_1, bs = 'ps', k = kbasis) +", paste(xnam[2:nCovs], collapse= "+")))),
                               family = "poisson")
  } else{
    modelGamPois <-  mgcv::bam(as.formula(paste("y_vec ~ ", paste("s(x_1, bs = 'ps', k = kbasis)"))),
                               family = "poisson")
  }
  
  
  # keep model coefficients for modelBin and modelPois
  
  for(i in 1:nCovs){
    coefs[[i]]$testval <- modelPois$coefficients[(i+1)]
  }
  
  # smoothing parameter from original model
  smPar <- modelGamPois$sp
  # set maximum distance to maximum distance of fit
  distance <- max(max(x_1, na.rm = TRUE), max(x_1, na.rm = TRUE)) + 1
  
  #------- data for plot
  # create dataset for prediction (needed for confidence intervals)
  data_orig <- data.frame(y_vec)
  for(i in 1:nCovs){
    data_orig[,(i+1)] <- x_vec[[i]]
    colnames(data_orig)[i+1] <- paste0("x_", i)
  }
  data_orig$y_vec <- as.factor(data_orig$y_vec)
  
  # prediction data
  pred_data <- data_orig[1:300,]
  
  for(j in 1:ncol(pred_data)){
    if(class(pred_data[[j]]) == "factor") {
      pred_data[[j]] <- as.factor(rep(levels(data_orig[[j]]), length.out = nrow(pred_data)))
    }
    if(class(pred_data[[j]]) %in% c("integer", "numeric")) {
      pred_data[[j]] <- seq(from = min(data_orig[[j]], na.rm = TRUE), to = max(data_orig[[j]], na.rm = TRUE), length = nrow(pred_data))
    }
  }
  
  # prediction with transformation, x1_pred, all other covariates as mean value
  cov_mean <- unlist(lapply(x_orig[2:length(x_orig)], function(x) mean(x, na.rm = TRUE)))
  
  pred_data_mean <- pred_data

  if(nCovs > 1){
    for(i in 1:(nCovs-1)){
      pred_data_mean[,i+2] <- cov_mean[i]
    }
  }
  
  # terms
  
  #glm
  pred_Orig_terms_Glm <- predict(modelPois, newdata = pred_data, type = "terms", se.fit = TRUE)
  temp_fit_glm <- as.data.frame(pred_Orig_terms_Glm$fit)
  temp_se_glm <- as.data.frame(pred_Orig_terms_Glm$se.fit)

  
  #gam
  pred_Orig_terms_Gam <- predict(modelGamPois, newdata = pred_data, type = "terms", se.fit = TRUE)
  temp_fit <- as.data.frame(pred_Orig_terms_Gam$fit)
  temp_se <- as.data.frame(pred_Orig_terms_Gam$se.fit)
  
  
  # response
  # glm
  pred_Orig_resp_Glm <- predict(modelPois, newdata = pred_data_mean, type = "response", se.fit = TRUE)
  #gam
  pred_Orig_resp_Gam <- predict(modelGamPois, newdata = pred_data_mean, type = "response", se.fit = TRUE)
  
  # plotData
  # terms
  
  plotData_orig_terms_Glm <- list(x = pred_data$x_1,
                                      fit = temp_fit_glm$`x_1`,
                                      se = temp_se_glm$`x_1`)
  plotData_orig_terms_Gam <- list(x = pred_data$x_1,
                                  fit = temp_fit$`s(x_1)`,
                                  se = temp_se$`s(x_1)`)
  
  # response
  plotData_orig_resp_Glm <- list(x = pred_data_mean$x_1,
                                    fit = as.numeric(pred_Orig_resp_Glm$fit),
                                    se = as.numeric(pred_Orig_resp_Glm$se.fit))
  plotData_orig_resp_Gam <- list(x = pred_data_mean$x_1,
                                    fit = as.numeric(pred_Orig_resp_Gam$fit),
                                    se = as.numeric(pred_Orig_resp_Gam$se.fit))
  
  #----- Results
  resultsOriginal <- list()
  resultsOriginal$data_orig <- data_orig
  resultsOriginal$modelPois <- modelPois
  resultsOriginal$modelGamPois <- modelGamPois
  resultsOriginal$smPar <- smPar
  resultsOriginal$maxDist <- distance
  resultsOriginal$kbasis <- kbasis
  resultsOriginal$coefs <- coefs
  resultsOriginal$y_orig <- y_orig
  resultsOriginal$x_orig <- x_orig
  resultsOriginal$y_prep <- y
  resultsOriginal$x_prep <- x
  resultsOriginal$pred_data <- pred_data
  resultsOriginal$pred_data_mean <- pred_data_mean
  resultsOriginal$plotData_orig_terms_Glm <- plotData_orig_terms_Glm  
  resultsOriginal$plotData_orig_terms_Gam <- plotData_orig_terms_Gam
  resultsOriginal$plotData_orig_resp_Glm <- plotData_orig_resp_Glm
  resultsOriginal$plotData_orig_resp_Gam <- plotData_orig_resp_Gam
  resultsOriginal$nCovs <- nCovs
  return(resultsOriginal)
}


# testPois <- poissonOriginal(y = test$Y, x = list(test$X_dist, test$X_pub),
#   removeControl = FALSE, logicMatrix = NULL,
#   maxDist = 1000, kbasis = 8)



