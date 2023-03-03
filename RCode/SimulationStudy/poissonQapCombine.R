
## poissonQapCombine-----------------------

poissonQapCombine <- function(permutationList, OriginalList){
  reps <- length(permutationList)
  nCovs <- OriginalList$nCovs
  
  # list_pred_terms
  # Initialize list of matrices to summarize predictions for each covariate
  # and each permutation
  list_pred_terms <- vector("list",  ncol(OriginalList$data_orig)-1)
  
  for(j in 1:length(list_pred_terms)) {
    list_pred_terms[[j]] <- matrix(NA, nrow = nrow(OriginalList$pred_data), ncol = reps)
  }
  
  # name list_pred_terms
  names(list_pred_terms) <- colnames(permutationList[[1]]$pred_terms_temp)
  
  # plotData
  plotData_terms <- plotData_resp <- vector(mode = "list", length = reps)
  
  
  coefsGlm <- vector(mode = "list", length = nCovs)
  for(i in 1:nCovs){
    coefsGlm[[i]]$dist <-  vector(mode = "numeric", length = reps)
  }
  names(coefsGlm) <- names(OriginalList$model$coefficients)[-1]
  
  # Initialize matrix for quantiles of coefficients Gam
  matrix_coefficients    <- matrix(NA, nrow = length(OriginalList$modelGamPois$coefficients), ncol = reps)
  quantiles_coefficients <- matrix(NA, ncol = 2, nrow = nrow(matrix_coefficients))
  rownames(matrix_coefficients) <-  rownames(quantiles_coefficients) <- names(OriginalList$modelGamPois$coefficients)
  colnames(quantiles_coefficients) <- c("2.5", "97.5")
  
  
  #--------------
  # combine all permutations
  for (p in 1:length(permutationList)){
    # list_pred_terms
    # Save predictions per covariate
    for(j in 1:length(list_pred_terms)) {
      list_pred_terms[[j]][,p] <- permutationList[[p]]$pred_terms_temp$fit[,j]
    }
    
    
    # plotData
    plotData_terms[[p]] <- permutationList[[p]]$plotData_terms
    plotData_resp[[p]] <- permutationList[[p]]$plotData_resp
    
    
    # save model coefficients from GLMs
    for(i in 1:nCovs){
      coefsGlm[[i]]$dist[p] <- permutationList[[p]]$coefsGlm[[i]]$dist
      coefsGlm[[i]]$testval <- OriginalList$modelPois$coefficients[i+1]
    }
    # matrix coefficients
    matrix_coefficients[, p] <- permutationList[[p]]$matrix_coefficients
  }
  
  # add original data to plot Data
  # response
  plotData_resp$plotData_orig <- OriginalList$plotData_orig_resp_Gam
  plotData_resp <- plotData_resp[c(length(plotData_resp), 1:(length(plotData_resp)-1))]
  
  #terms
  plotData_terms$plotData_orig <- OriginalList$plotData_orig_terms
  plotData_terms <- plotData_terms[c(length(plotData_terms), 1:(length(plotData_terms)-1))]
  
  # Initialize list of matrices to save quantiles of predictions per covariate
  quantiles_pred_terms <- vector("list", ncol(OriginalList$data_orig)-1)
  
  for(j in 1:length(quantiles_pred_terms)) {
    quantiles_pred_terms[[j]] <- matrix(NA, nrow = nrow(OriginalList$pred_data), ncol = 2)
    colnames(quantiles_pred_terms[[j]]) <- c("2.5", "97.5")
  }
  # name list_pred_terms
  names(quantiles_pred_terms) <- names(list_pred_terms)
  #---
  
  quantiles_pred_termsPois_exp <- quantiles_pred_terms
  
  # Calculate quantiles of predictions per covariate
  for(i1 in 1:nrow(OriginalList$pred_data)) {
    for(j in 1:length(quantiles_pred_terms)) {
      quantiles_pred_terms[[j]][i1,] <- quantile(list_pred_terms[[j]][i1,], probs = c(0.025,0.975))
      quantiles_pred_termsPois_exp[[j]][i1,] <- quantile(exp(list_pred_terms[[j]][i1,]), probs = c(0.025,0.975))
    }
  }
  
  # terms
  temp <- matrix(NA, nrow = length(plotData_terms)-1, ncol = 300)
  for(i in 1:nrow(temp)){
    for(j in 1:ncol(temp))
      temp[i,j] <- plotData_terms[[i+1]]$fit[j]
  }
  
  perm_low <- apply(temp, 2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
  perm_up <- apply(temp, 2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
  dataQuant_terms <- data.frame(x = plotData_terms[[1]]$x, perm_low, perm_up)
  
  # response
  temp <- matrix(NA, nrow = length(plotData_resp)-1, ncol = 300)
  for(i in 1:nrow(temp)){
    for(j in 1:ncol(temp))
      temp[i,j] <- plotData_resp[[i+1]]$fit[j]
  }
  
  perm_low <- apply(temp, 2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
  perm_up <- apply(temp, 2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
  dataQuant_resp <- data.frame(x = plotData_resp[[1]]$x, perm_low, perm_up)
  
  
  # qaplist
  qaplist <- list()
  for(i in 1:nCovs){
    coefsGlm[[i]]$pgreq <- mean(as.numeric(coefsGlm[[i]]$dist >= coefsGlm[[i]]$testval))
    coefsGlm[[i]]$pleeq <- mean(as.numeric(coefsGlm[[i]]$dist <= coefsGlm[[i]]$testval))
    class(coefsGlm[[i]]) <- c("qaptest", "qap")
  }
  
  # Gam linear coefficients
  if(nCovs > 1){
    coefsGam <- vector("list", nCovs-1)
    for(i in 2:nCovs){
      coefsGam[[i-1]]$dist <- matrix_coefficients[i,]
      coefsGam[[i-1]]$testval <- OriginalList$modelGamPois$coefficients[i]
      
      coefsGam[[i-1]]$pgreq <- mean(as.numeric(coefsGam[[i-1]]$dist >= coefsGam[[i-1]]$testval))
      coefsGam[[i-1]]$pleeq <- mean(as.numeric(coefsGam[[i-1]]$dist <= coefsGam[[i-1]]$testval))
      class(coefsGam[[i-1]]) <- c("qaptest", "qap")
    }
  } else {
    coefsGam <- NULL
  }
  
  qaplist$coefsGlm <- coefsGlm
  qaplist$coefsGam <- coefsGam
  
  # results
  resultsPerms <- list()
  resultsPerms$modelGlmPois <- OriginalList$modelPois
  resultsPerms$modelGamPois <- OriginalList$modelGamPois
  resultsPerms$qaplist <- qaplist
  resultsPerms$data_orig <- OriginalList$data_orig
  resultsPerms$pred_data <- OriginalList$pred_data
  resultsPerms$pred_datas_mean <- OriginalList$pred_data_mean
  resultsPerms$plotData_resp <- plotData_resp
  resultsPerms$plotData_terms <- plotData_terms
  resultsPerms$dataQuantile_resp <- dataQuant_resp
  resultsPerms$dataQuantile_terms <- dataQuant_terms
  resultsPerms$plotData_orig_resp_Glm <- OriginalList$plotData_orig_resp_Glm
  resultsPerms$plotData_orig_terms_Glm <- OriginalList$plotData_orig_terms_Glm
  resultsPerms$matrix_coefficients <- matrix_coefficients
  resultsPerms$reps <- reps
  class(resultsPerms) <- "HurdleQap"
  return(resultsPerms)
}


#testCombine <- poissonQapCombine(permutationList = testPerms, OriginalList = testPois)
