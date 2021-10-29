
## hurdleQapCombine-----------------------

hurdleQapCombine <- function(permutationList, OriginalList){
  reps <- length(permutationList)
  nCovs <- OriginalList$nCovs

  # list_pred_terms
  # Initialize list of matrices to summarize predictions for each covariate
  # and each permutation
  list_pred_termsBin <- vector("list",  ncol(OriginalList$data_orig_Bin)-1)

  for(j in 1:length(list_pred_termsBin)) {
    list_pred_termsBin[[j]] <- matrix(NA, nrow = nrow(OriginalList$pred_data_Bin), ncol = reps)
  }

  list_pred_termsPois <- vector("list",  ncol(OriginalList$data_orig_Pois)-1)

  for(j in 1:length(list_pred_termsPois)) {
    list_pred_termsPois[[j]] <- matrix(NA, nrow = nrow(OriginalList$pred_data_Pois), ncol = reps)
  }

  # name list_pred_terms
  names(list_pred_termsBin) <- colnames(permutationList[[1]]$pred_terms_tempBin)
  names(list_pred_termsPois) <- colnames(permutationList[[1]]$pred_terms_tempPois)


  # plotData
  plotDataBin_terms <- plotDataPois_terms <-  plotDataBin_resp <- plotDataPois_resp <- vector(mode = "list", length = reps)


  zeroGlm <- countGlm <- vector(mode = "list", length = nCovs)
  for(i in 1:nCovs){
    zeroGlm[[i]]$dist <-  vector(mode = "numeric", length = reps)
    countGlm[[i]]$dist <-  vector(mode = "numeric", length = reps)
  }
  names(zeroGlm) <- names(OriginalList$modelBin$coefficients)[-1]
  names(countGlm) <- names(OriginalList$modelPois$coefficients)[-1]

  # Initialize matrix for quantiles of coefficients Gam
  matrix_coefficientsBin    <- matrix(NA, nrow = length(OriginalList$modelGamBin$coefficients), ncol = reps)
  quantiles_coefficientsBin <- matrix(NA, ncol = 2, nrow = nrow(matrix_coefficientsBin))
  rownames(matrix_coefficientsBin) <-  rownames(quantiles_coefficientsBin) <- names(OriginalList$modelGamBin$coefficients)
  colnames(quantiles_coefficientsBin) <- c("2.5", "97.5")

  matrix_coefficientsPois    <- matrix(NA, nrow = length(OriginalList$modelGamPois$coefficients), ncol = reps)
  quantiles_coefficientsPois <- matrix(NA, ncol = 2, nrow = nrow(matrix_coefficientsPois))
  rownames(matrix_coefficientsPois) <-  rownames(quantiles_coefficientsPois) <- names(OriginalList$modelGamPois$coefficients)
  colnames(quantiles_coefficientsPois) <- c("2.5", "97.5")

  #--------------
  # combine all permutations
  for (p in 1:length(permutationList)){
    # list_pred_terms
    # Save predictions per covariate
    for(j in 1:length(list_pred_termsBin)) {
      list_pred_termsBin[[j]][,p] <- permutationList[[p]]$pred_terms_tempBin$fit[,j]
    }

    for(j in 1:length(list_pred_termsPois)) {
      list_pred_termsPois[[j]][,p] <- permutationList[[p]]$pred_terms_tempPois$fit[,j]
    }

    # plotData
    plotDataBin_terms[[p]] <- permutationList[[p]]$plotDataBin_terms
    plotDataPois_terms[[p]] <- permutationList[[p]]$plotDataPois_terms
    plotDataBin_resp[[p]] <- permutationList[[p]]$plotDataBin_resp
    plotDataPois_resp[[p]] <- permutationList[[p]]$plotDataPois_resp


    # save model coefficients from GLMs
    for(i in 1:nCovs){
      zeroGlm[[i]]$dist[p] <- permutationList[[p]]$zeroGlm[[i]]$dist
      countGlm[[i]]$dist[p] <- permutationList[[p]]$countGlm[[i]]$dist
      zeroGlm[[i]]$testval <- OriginalList$modelBin$coefficients[i+1]
      countGlm[[i]]$testval <- OriginalList$modelPois$coefficients[i+1]
    }
    # matrix coefficients
    matrix_coefficientsBin[, p] <- permutationList[[p]]$matrix_coefficientsBin
    matrix_coefficientsPois[, p] <- permutationList[[p]]$matrix_coefficientsPois
  }

  # add original data to plot Data
  # response
  plotDataBin_resp$plotData_origBin <- OriginalList$plotData_origBin_resp_Gam
  plotDataBin_resp <- plotDataBin_resp[c(length(plotDataBin_resp), 1:(length(plotDataBin_resp)-1))]
  plotDataPois_resp$plotData_origPois <- OriginalList$plotData_origPois_resp_Gam
  plotDataPois_resp <- plotDataPois_resp[c(length(plotDataPois_resp), 1:(length(plotDataPois_resp)-1))]

  #terms
  plotDataBin_terms$plotData_origBin <- OriginalList$plotData_origBin_terms
  plotDataBin_terms <- plotDataBin_terms[c(length(plotDataBin_terms), 1:(length(plotDataBin_terms)-1))]
  plotDataPois_terms$plotData_origPois <- OriginalList$plotData_origPois_terms
  plotDataPois_terms <- plotDataPois_terms[c(length(plotDataPois_terms), 1:(length(plotDataPois_terms)-1))]

  # Initialize list of matrices to save quantiles of predictions per covariate
  quantiles_pred_termsBin <- vector("list", ncol(OriginalList$data_orig_Bin)-1)

  for(j in 1:length(quantiles_pred_termsBin)) {
    quantiles_pred_termsBin[[j]] <- matrix(NA, nrow = nrow(OriginalList$pred_data_Bin), ncol = 2)
    colnames(quantiles_pred_termsBin[[j]]) <- c("2.5", "97.5")
  }
  # name list_pred_terms
  names(quantiles_pred_termsBin) <- names(list_pred_termsBin)

  quantiles_pred_termsPois <- vector("list", ncol(OriginalList$data_orig_Pois)-1)

  for(j in 1:length(quantiles_pred_termsPois)) {
    quantiles_pred_termsPois[[j]] <- matrix(NA, nrow = nrow(OriginalList$pred_data_Pois), ncol = 2)
    colnames(quantiles_pred_termsPois[[j]]) <- c("2.5", "97.5")
  }
  # name list_pred_terms
  names(quantiles_pred_termsPois) <- names(list_pred_termsPois)

  #---

  quantiles_pred_termsBin_plog <- quantiles_pred_termsBin
  quantiles_pred_termsPois_exp <- quantiles_pred_termsPois

  # Calculate quantiles of predictions per covariate
  for(i1 in 1:nrow(OriginalList$pred_data_Bin)) {
    for(j in 1:length(quantiles_pred_termsBin)) {
      quantiles_pred_termsBin[[j]][i1,] <- quantile(list_pred_termsBin[[j]][i1,], probs = c(0.025,0.975))
      quantiles_pred_termsBin_plog[[j]][i1,] <- quantile(plogis(list_pred_termsBin[[j]][i1,]), probs = c(0.025,0.975))
    }
  }
  for(i1 in 1:nrow(OriginalList$pred_data_Pois)) {
    for(j in 1:length(quantiles_pred_termsPois)) {
      quantiles_pred_termsPois[[j]][i1,] <- quantile(list_pred_termsPois[[j]][i1,], probs = c(0.025,0.975))
      quantiles_pred_termsPois_exp[[j]][i1,] <- quantile(exp(list_pred_termsPois[[j]][i1,]), probs = c(0.025,0.975))
    }
  }

  temp <- matrix(NA, nrow = length(plotDataBin_terms)-1, ncol = 300)
  for(i in 1:nrow(temp)){
    for(j in 1:ncol(temp))
      temp[i,j] <- plotDataBin_terms[[i+1]]$fit[j]
  }

  perm_low <- apply(temp,2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
  perm_up <- apply(temp,2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
  dataQuant_Bin_terms <- data.frame(x = plotDataBin_terms[[1]]$x, perm_low, perm_up)


  # response
  temp <- matrix(NA, nrow = length(plotDataBin_resp)-1, ncol = 300)
  for(i in 1:nrow(temp)){
     for(j in 1:ncol(temp))
     temp[i,j] <- plotDataBin_resp[[i+1]]$fit[j]
   }

   perm_low <- apply(temp,2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
   perm_up <- apply(temp,2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
   dataQuant_Bin_resp <- data.frame(x = plotDataBin_resp[[1]]$x, perm_low, perm_up)

  temp <- matrix(NA, nrow = length(plotDataPois_terms)-1, ncol = 300)
  for(i in 1:nrow(temp)){
    for(j in 1:ncol(temp))
      temp[i,j] <- plotDataPois_terms[[i+1]]$fit[j]
  }

  perm_low <- apply(temp, 2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
  perm_up <- apply(temp, 2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
  dataQuant_Pois_terms <- data.frame(x = plotDataPois_terms[[1]]$x, perm_low, perm_up)

  # response
  temp <- matrix(NA, nrow = length(plotDataPois_resp)-1, ncol = 300)
  for(i in 1:nrow(temp)){
    for(j in 1:ncol(temp))
      temp[i,j] <- plotDataPois_resp[[i+1]]$fit[j]
  }

  perm_low <- apply(temp, 2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
  perm_up <- apply(temp, 2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
  dataQuant_Pois_resp <- data.frame(x = plotDataPois_resp[[1]]$x, perm_low, perm_up)

  # qaplist
  qaplist <- list()
  for(i in 1:nCovs){
    zeroGlm[[i]]$pgreq <- mean(as.numeric(zeroGlm[[i]]$dist >= zeroGlm[[i]]$testval))
    zeroGlm[[i]]$pleeq <- mean(as.numeric(zeroGlm[[i]]$dist <= zeroGlm[[i]]$testval))
    countGlm[[i]]$pgreq <- mean(as.numeric(countGlm[[i]]$dist >= countGlm[[i]]$testval))
    countGlm[[i]]$pleeq <- mean(as.numeric(countGlm[[i]]$dist <= countGlm[[i]]$testval))
    class(zeroGlm[[i]]) <- class(countGlm[[i]]) <- c("qaptest", "qap")
  }

  # Gam linear coefficients
  if(nCovs > 1){
    zeroGam <- countGam <- vector("list", nCovs-1)
    for(i in 2:nCovs){
      zeroGam[[i-1]]$dist <- matrix_coefficientsBin[i,]
      zeroGam[[i-1]]$testval <- OriginalList$modelGamBin$coefficients[i]
      countGam[[i-1]]$dist <- matrix_coefficientsPois[i,]
      countGam[[i-1]]$testval <- OriginalList$modelGamPois$coefficients[i]

      zeroGam[[i-1]]$pgreq <- mean(as.numeric(zeroGam[[i-1]]$dist >= zeroGam[[i-1]]$testval))
      zeroGam[[i-1]]$pleeq <- mean(as.numeric(zeroGam[[i-1]]$dist <= zeroGam[[i-1]]$testval))
      countGam[[i-1]]$pgreq <- mean(as.numeric(countGam[[i-1]]$dist >= countGam[[i-1]]$testval))
      countGam[[i-1]]$pleeq <- mean(as.numeric(countGam[[i-1]]$dist <= countGam[[i-1]]$testval))
      class(zeroGam[[i-1]]) <- class(countGam[[i-1]]) <- c("qaptest", "qap")
    }
  } else {
    zeroGam <- countGam <- NULL
  }

  qaplist$zeroGlm <- zeroGlm
  qaplist$countGlm <- countGlm
  qaplist$zeroGam <- zeroGam
  qaplist$countGam <- countGam

  # results
  resultsPerms <- list()
  resultsPerms$modelGlmBin <- OriginalList$modelBin
  resultsPerms$modelGamBin <- OriginalList$modelGamBin
  resultsPerms$modelGlmPois <- OriginalList$modelPois
  resultsPerms$modelGamPois <- OriginalList$modelGamPois
  resultsPerms$data_orig_Bin <- OriginalList$data_orig_Bin
  resultsPerms$data_orig_Pois <- OriginalList$data_orig_Pois
  resultsPerms$pred_data_Bin <- OriginalList$pred_data_Bin
  resultsPerms$pred_data_Bin_mean <- OriginalList$pred_data_Bin_mean
  resultsPerms$pred_data_Pois <- OriginalList$pred_data_Pois
  resultsPerms$pred_data_Pois_mean <- OriginalList$pred_data_Pois_mean
  resultsPerms$plotDataBin_resp <- plotDataBin_resp
  resultsPerms$plotDataBin_terms <- plotDataBin_terms
  resultsPerms$dataQuantile_Bin_resp <- dataQuant_Bin_resp
  resultsPerms$dataQuantile_Bin_terms <- dataQuant_Bin_terms
  resultsPerms$plotDataPois_resp <- plotDataPois_resp
  resultsPerms$plotDataPois_terms <- plotDataPois_terms
  resultsPerms$dataQuantile_Pois_resp <- dataQuant_Pois_resp
  resultsPerms$dataQuantile_Pois_terms <- dataQuant_Pois_terms
  resultsPerms$plotData_origBin_resp_Glm <- OriginalList$plotData_origBin_resp_Glm
  resultsPerms$plotData_origPois_resp_Glm <- OriginalList$plotData_origPois_resp_Glm
  resultsPerms$plotData_origBin_terms_Glm <- OriginalList$plotData_origBin_terms_Glm
  resultsPerms$plotData_origPois_terms_Glm <- OriginalList$plotData_origPois_terms_Glm
  resultsPerms$qaplist <- qaplist
  resultsPerms$matrix_coefficientsBin <- matrix_coefficientsBin
  resultsPerms$matrix_coefficientsPois <- matrix_coefficientsPois
  resultsPerms$reps <- reps
  class(resultsPerms) <- "HurdleQap"
  return(resultsPerms)
}
