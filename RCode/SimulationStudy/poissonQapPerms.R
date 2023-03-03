poissonQapPerms <- function(OriginalResults, seed, reps = NULL){
  
  calculation <- function(OriginalResults, seed){
    set.seed(seed)
    # permute publication matrix; keep x matrices as they are
    permY <- as.integer(sna::gvectorize(sna::rmperm(OriginalResults$y_prep), mode = "graph", diag = FALSE, censor.as.na = FALSE))
    x_vec <- list()
    for(i in 1:OriginalResults$nCovs){
      x_vec[[i]] <- as.numeric(sna::gvectorize(OriginalResults$x_prep[[i]], mode = "graph", diag = FALSE, censor.as.na = FALSE))
    }
    
    # remove missing distances
    permY <- permY[which(!is.na(x_vec[[1]]))]
    if(OriginalResults$nCovs > 1){
      for(i in 2:OriginalResults$nCovs){
        x_vec[[i]] <- x_vec[[i]][which(!is.na(x_vec[[1]]))]
      }
    }
    x_vec[[1]] <- x_vec[[1]][which(!is.na(x_vec[[1]]))]
    
    #------- permuted models
    for(i in 1:OriginalResults$nCovs){
      assign(paste0("x_",i), x_vec[[i]])
      #assign(paste0("xpois_",i), x_pois[[i]])
    }
    xnam <- paste0("x_", 1:OriginalResults$nCovs)

    # One-step poisson model instead of classical hurlde
 
    # GLM with poisson family on whole data 
    mod2 <-  glm(as.formula(paste("permY ~ ", paste(xnam, collapse= "+"))),
                 family = "poisson")
    
    
    # GAM with poisson family on whole data
    if(OriginalResults$nCovs > 1){
      mod4 <-  mgcv::bam(as.formula(paste("permY ~ ", paste("s(x_1, bs = 'ps', k = OriginalResults$kbasis, sp = OriginalResults$smPar) +", paste(xnam[2:OriginalResults$nCovs], collapse= "+")))),
                         family = "poisson")
    } else {
      mod4 <-  mgcv::bam(as.formula(paste("permY ~ ", paste("s(x_1, bs = 'ps', k = OriginalResults$kbasis, sp = OriginalResults$smPar)"))),
                         family = "poisson")
    }
    
    coefsGlm <- vector("list", OriginalResults$nCovs)
    # save model coefficients from GLMs
    for(i in 1:OriginalResults$nCovs){
      coefsGlm[[i]]$dist <- mod2$coefficients[(i+1)]
    }
    
    # Save coefficients from mod3, mod4 (GAMs)
    matrix_coefficients4 <- mod4$coefficients
    
    # Calculate predictions per covariate
    pred_terms_temp4 <- predict(mod4, newdata = OriginalResults$pred_data, type = "terms", se.fit = TRUE)
    temp_fit <- as.data.frame(pred_terms_temp4$fit)
    temp_se <- as.data.frame(pred_terms_temp4$se.fit)
    pred_resp_temp4 <- predict(mod4, newdata = OriginalResults$pred_data_mean, type = "response", se.fit = TRUE)
    
    # terms prediction
    plotData_terms <- list(x = OriginalResults$pred_data$x_1,
                               fit = temp_fit$`s(x_1)`,
                               se = temp_se$`s(x_1)`)
    plotData_terms$fit[which(plotData_terms$x > max(x_vec[[1]], na.rm = TRUE))] <- NA
    
    # response prediction
    plotData_resp <- list(x = OriginalResults$pred_data_mean$x_1,
                              fit = as.numeric(pred_resp_temp4$fit),
                              se = as.numeric(pred_resp_temp4$se.fit))
    plotData_resp$fit[which(plotData_resp$x > max(x_vec[[1]], na.rm = TRUE))] <- NA
    
    # results
    resultsQap <- list()
    resultsQap$matrix_coefficients <- matrix_coefficients4
    resultsQap$pred_terms_temp <- pred_terms_temp4
    resultsQap$plotData_terms <- plotData_terms
    resultsQap$plotData_resp <- plotData_resp
    resultsQap$coefsGlm <- coefsGlm
    return(resultsQap)
  }
  
  if(is.null(reps)){
    SingleResult <- calculation(OriginalResults, seed)
    return(SingleResult)
  }  else {
    allPermutations <- vector("list",  reps)
    for(perm in 1:reps){
      allPermutations[[perm]] <- calculation(OriginalResults = OriginalResults, seed = perm)
      names(allPermutations)[perm] <- paste0("Permutation", perm)
      if( perm %% 5 == 0 ) cat(paste("iteration", perm, "complete\n"))
    }
    return(allPermutations)
  }
}

#testPerms <- possionQapPerms(testPois, seed = 123, reps = 3)
  
