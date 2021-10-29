
## hurdleQapPerms ----------------------------------------

# OriginalResults should be the output from the function hurdleQapOriginal
      # reps: number of permutations
# seed: choose a seed for random permutation
# reps: Null or number of permutations if all permutations should be calculated at once (only recommended with small datasets)

hurdleQapPerms <- function(OriginalResults, seed, reps = NULL){

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

    # dichtomize publication data (for binary model)
    y_bin <- as.factor(ifelse(permY == 0, 0, 1))
    x_bin <- x_vec
    # keep only observations which have at least one publication (for poisson model)
    x_pois <- x_vec
    for(i in 1:OriginalResults$nCovs){
      x_pois[[i]] <- x_pois[[i]][which(permY != 0)]
    }
    y_pois <- permY[which(permY != 0)]

    #------- permuted models
    for(i in 1:OriginalResults$nCovs){
      assign(paste0("xbin_",i), x_bin[[i]])
      assign(paste0("xpois_",i), x_pois[[i]])
    }
    xnam_bin <- paste0("xbin_", 1:OriginalResults$nCovs)
    xnam_pois <- paste0("xpois_", 1:OriginalResults$nCovs)

    # Two-step model instead of classical hurlde
    # GLM with binomial family: is there a collaboration between two authors or not (independent of number of papers)
    mod1 <- glm(as.formula(paste("factor(y_bin) ~ ", paste(xnam_bin, collapse= "+"))),
                    family = binomial(link = "cloglog"))
    # GLM with ztpoisson family only on data with at least one publication: answer questions "how many publications"
    mod2 <-  glm(as.formula(paste("y_pois ~ ", paste(xnam_pois, collapse= "+"))),
                      family = countreg::ztpoisson)


    # GAM with binomial family: is there a collaboration between two authors or not (independent of number of papers)
    # GAM with ztpoisson family only on data with at least one publication: answer questions "how many publications"
    if(OriginalResults$nCovs > 1){
      mod3 <- mgcv::bam(as.formula(paste("factor(y_bin) ~ ", paste("s(xbin_1, bs = 'ps', k = OriginalResults$kbasis, sp = OriginalResults$smPar) +",
                                                             paste(xnam_bin[2:OriginalResults$nCovs], collapse= "+")))),
                         family = binomial(link = "cloglog"))
      mod4 <-  mgcv::bam(as.formula(paste("y_pois ~ ", paste("s(xpois_1, bs = 'ps', k = OriginalResults$kbasis, sp = OriginalResults$smPar) +", paste(xnam_pois[2:OriginalResults$nCovs], collapse= "+")))),
                           family = countreg::ztpoisson)
    } else {
      mod3 <- mgcv::bam(as.formula(paste("factor(y_bin) ~ ", paste("s(xbin_1, bs = 'ps', k = OriginalResults$kbasis, sp = OriginalResults$smPar)"))),
                  family = binomial(link = "cloglog"))
      mod4 <-  mgcv::bam(as.formula(paste("y_pois ~ ", paste("s(xpois_1, bs = 'ps', k = OriginalResults$kbasis, sp = OriginalResults$smPar)"))),
                   family = countreg::ztpoisson)
    }

    zeroGlm <- countGlm <- vector("list", OriginalResults$nCovs)
    # save model coefficients from GLMs
    for(i in 1:OriginalResults$nCovs){
      zeroGlm[[i]]$dist <- mod1$coefficients[(i+1)]
      countGlm[[i]]$dist <- mod2$coefficients[(i+1)]
    }

    # Save coefficients from mod3, mod4 (GAMs)
    matrix_coefficients3 <- mod3$coefficients
    matrix_coefficients4 <- mod4$coefficients


    # Calculate predictions per covariate
    pred_terms_temp3 <- predict(mod3, newdata = OriginalResults$pred_data_Bin, type = "terms", se.fit = TRUE)
    pred_terms_temp4 <- predict(mod4, newdata = OriginalResults$pred_data_Pois, type = "terms", se.fit = TRUE)
    temp_fit_bin <- as.data.frame(pred_terms_temp3$fit)
    temp_fit_pois <- as.data.frame(pred_terms_temp4$fit)
    temp_se_bin <- as.data.frame(pred_terms_temp3$se.fit)
    temp_se_pois <- as.data.frame(pred_terms_temp4$se.fit)

    pred_resp_temp3 <- predict(mod3, newdata = OriginalResults$pred_data_Bin_mean, type = "response", se.fit = TRUE)
    pred_resp_temp4 <- predict(mod4, newdata = OriginalResults$pred_data_Pois_mean, type = "response", se.fit = TRUE)

    # terms prediction
    plotDataBin_terms <- list(x = OriginalResults$pred_data_Bin$xbin_1,
                                   fit = temp_fit_bin$`s(xbin_1)`,
                                   se = temp_se_bin$`s(xbin_1)`)
    plotDataBin_terms$fit[which(plotDataBin_terms$x > max(x_bin[[1]], na.rm = TRUE))] <- NA

    plotDataPois_terms <- list(x = OriginalResults$pred_data_Pois$xpois_1,
                                    fit = temp_fit_pois$`s(xpois_1)`,
                                    se = temp_se_pois$`s(xpois_1)`)
    plotDataPois_terms$fit[which(plotDataPois_terms$x > max(x_pois[[1]], na.rm = TRUE))] <- NA

    # response prediction
    plotDataBin_resp <- list(x = OriginalResults$pred_data_Bin_mean$xbin_1,
                              fit = as.numeric(pred_resp_temp3$fit),
                              se = as.numeric(pred_resp_temp3$se.fit))
    plotDataBin_resp$fit[which(plotDataBin_resp$x > max(x_bin[[1]], na.rm = TRUE))] <- NA

    plotDataPois_resp <- list(x = OriginalResults$pred_data_Pois_mean$xpois_1,
                             fit = as.numeric(pred_resp_temp4$fit),
                             se = as.numeric(pred_resp_temp4$se.fit))
    plotDataPois_resp$fit[which(plotDataPois_resp$x > max(x_pois[[1]], na.rm = TRUE))] <- NA


    # results
    resultsQap <- list()
    resultsQap$matrix_coefficientsBin <- matrix_coefficients3
    resultsQap$matrix_coefficientsPois <- matrix_coefficients4
    resultsQap$pred_terms_tempBin <- pred_terms_temp3
    resultsQap$pred_terms_tempPois <- pred_terms_temp4
    resultsQap$plotDataBin_terms <- plotDataBin_terms
    resultsQap$plotDataPois_terms <- plotDataPois_terms
    resultsQap$plotDataBin_resp <- plotDataBin_resp
    resultsQap$plotDataPois_resp <- plotDataPois_resp
    resultsQap$countGlm <- countGlm
    resultsQap$zeroGlm <- zeroGlm
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
