# simulation with two covariates

taskid <- Sys.getenv('SLURM_ARRAY_TASK_ID')

library(extraDistr)
library(truncnorm)

source("hurdleQap.R", echo = TRUE)

load("distance_HMGU.RData")

# remove distance NAs
meanPub_HMGU <- meanPub_HMGU[-which(is.na(distance_HMGU))]
distance_HMGU <- na.omit(distance_HMGU)
# remove distance < 1000
meanPub_HMGU <- meanPub_HMGU[-which(distance_HMGU > 1000)]
distance_HMGU <- distance_HMGU[-which(distance_HMGU > 1000)]


# Function to create data
createData <- function(n = 50,
                       beta_bin0, 
                       beta_binDist_min, beta_binDist_max,
                       beta_binPub_min, beta_binPub_max,
                       beta_pois0, 
                       beta_poisDist_min, beta_poisDist_max,
                       beta_poisPub_min, beta_poisPub_max, 
                       seed){
  authors <- paste0("ID", 1:n)
  set.seed(seed)
  
  # sample beta_binX and beta_poisX
  beta_binDist <- 0
  beta_poisDist <- 0
  beta_binPub <- 0
  beta_poisPub <- 0
  
  ## Distance matrix X_dist
  X_dist <- matrix(nrow = n, ncol = n)
  colnames(X_dist) <- rownames(X_dist) <- authors
  temp <- sample(1:length(distance_HMGU), size = length(X_dist[lower.tri(X_dist, diag = FALSE)]), replace = TRUE)
  #summary(temp)  
  #hist(temp)
  X_dist[lower.tri(X_dist, diag = FALSE)] <- distance_HMGU[temp]
  X_dist[upper.tri(X_dist, diag = FALSE)] <- t(X_dist)[upper.tri(X_dist)]
  diag(X_dist) <- 0
  # create vector x
  x_dist <- X_dist[upper.tri(X_dist, diag = FALSE)]
  #-------
  ## publication matrix X_pub
  X_pub <- matrix(nrow = n, ncol = n)
  colnames(X_pub) <- rownames(X_pub) <- authors
  X_pub[lower.tri(X_pub, diag = FALSE)] <- meanPub_HMGU[temp]
  X_pub[upper.tri(X_pub, diag = FALSE)] <- t(X_pub)[upper.tri(X_pub)]
  diag(X_pub) <- 0
  # create vector x
  x_pub <- X_pub[upper.tri(X_pub, diag = FALSE)]
  
  
  
  ## Create vector y
  # binomial part
  eta <- beta_bin0 + beta_binDist*x_dist + beta_binPub*x_pub 
  pi <- 1-exp(-exp(eta))
  z <- rbern(length(pi), prob = pi)
  
  # ZTP part
  y <- ifelse(z == 0, 0, NA)
  
  theta <- beta_pois0 + beta_poisDist*x_dist[z != 0] + beta_poisPub*x_pub[z != 0]
  lambda <- exp(theta)/(1-exp(-exp(theta)))
  
  library(extraDistr)
  y[z != 0] <- rtpois(n = length(lambda), lambda = lambda, a = 0)
  
  ## Put y into matrix
  Y <- matrix(NA, nrow = n, ncol = n)
  Y[upper.tri(Y, diag = FALSE)] <- y
  Y[lower.tri(Y, diag = FALSE)] <-  t(Y)[lower.tri(Y)]
  diag(Y) <- 0
  colnames(Y) <- rownames(Y) <- rownames(X_dist)
  
  results <- list(y = y, x_dist = x_dist, x_pub = x_pub, X_dist = X_dist, X_pub = X_pub, Y = Y, 
                  mean_lambda_pois = mean(lambda, na.rm = TRUE),
                  beta_bin0 = beta_bin0, beta_pois0 = beta_pois0,
                  beta_binDist = beta_binDist, beta_poisDist = beta_poisDist,
                  beta_binPub = beta_binPub, beta_poisPub = beta_poisPub)
  return(results)
}


# HurdleQap on simulated data
QAPreps <- 1000 #number of QAP repetitions

## Randomly drawn effect
pvalues_hurdleQap <- pvalues_hurdle <-  pvalues_poissonQap <- pvalues_poisson <- list()
coefs <- coefsHurdleQap <- coefsPoissonQap <- list()
meanLambda <- list()

test <- createData(n = 50,
                   beta_bin0 = -3.7,
                   beta_binDist_min = -0.003, beta_binDist_max = 0.003,
                   beta_binPub_min = -0.003, beta_binPub_max = 0.003,
                   beta_pois0 = 1,
                   beta_poisDist_min = -0.003, beta_poisDist_max = 0.003,
                   beta_poisPub_min = -0.003, beta_poisPub_max = 0.003,
                   seed = taskid)

coefs[[taskid]] <- list(beta_binDist = test$beta_binDist, beta_poisDist = test$beta_poisDist,
                        beta_binPub = test$beta_binPub, beta_poisPub = test$beta_poisPub) 

# HurdleQAP
set.seed(taskid)
myQap <- hurdleQap(y = test$Y, x = list(test$X_dist, test$X_pub),
                   removeControl = FALSE, logicMatrix = NULL,
                   maxDist = 1000, kbasis = 8, reps = QAPreps)


pvalues_hurdleQap[[taskid]] <- list(xbin_dist = myQap$qaplist$zeroGlm$xbin_1[2:4],
                                    xpois_dist = myQap$qaplist$countGlm$xpois_1[2:4],
                                    xbin_pub = myQap$qaplist$zeroGlm$xbin_2[2:4],
                                    xpois_pub = myQap$qaplist$countGlm$xpois_2[2:4])

coefsHurdleQap[[taskid]] <- list(xbin_dist = myQap$modelGlmBin$coefficients[2],
                                 xpois_dist = myQap$modelGlmPois$coefficients[2],
                                 xbin_pub = myQap$modelGlmBin$coefficients[3],
                                 xpois_pub = myQap$modelGlmPois$coefficients[3])

meanLambda[[taskid]] <- test$mean_lambda_pois

# Hurdle (without QAP)
pvalues_hurdle[[taskid]] <- list(bin_dist = summary(myQap$modelGlmBin)$coefficients[,4][2],
                                 pois_dist = summary(myQap$modelGlmPois)$coefficients[,4][2],
                                 bin_pub = summary(myQap$modelGlmBin)$coefficients[,4][3],
                                 pois_pub = summary(myQap$modelGlmPois)$coefficients[,4][3])

# Possion with QAP
source("poissonQap.R", echo = TRUE)
set.seed(taskid)
myPoissonQap <- poissonQap(y = test$Y, x = list(test$X_dist, test$X_pub),
                           removeControl = FALSE, logicMatrix = NULL,
                           maxDist = 1000, kbasis = 8, reps = QAPreps)

pvalues_poissonQap[[taskid]] <- list(x_dist = myPoissonQap$qaplist$coefsGlm[[1]][2:4],
                                     x_pub = myPoissonQap$qaplist$coefsGlm[[2]][2:4])

coefsPoissonQap[[taskid]] <- list(xpois_dist = myPoissonQap$modelGlmPois$coefficients[2],
                                  xpois_pub = myPoissonQap$modelGlmPois$coefficients[3])
# Possion (without QAP)
pvalues_poisson[[taskid]] <- list(pois_dist = summary(myPoissonQap$modelGlmPois)$coefficients[,4][2],
                                  pois_pub = summary(myPoissonQap$modelGlmPois)$coefficients[,4][3])
print(taskid)


rm(list = c("myQap", "myPoissonQap", "test", "createData", "hurdleQap", "hurdleQapCombine", "hurdleQapOriginal", "hurdleQapPerms",
            "poissonQap", "poissonOriginal", "poissonQapPerms", "poissonQapCombine", "distance_HMGU", "meanPub_HMGU"))
session <- sessionInfo()
save.image(file = paste0("Workspaces/2C_NV_n50/simulation_twoCov_NV_n50_", QAPreps, "_", taskid, ".Rdata"))




