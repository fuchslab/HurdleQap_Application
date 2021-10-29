# Application of functions from package; Helmholtz data

# Application example hurdleQap
source("hurdleQap.R")

# Data preparation
load("Workspaces/EverythingNeeded_QapGamFunction1_HMGU.RData")
# with logic mat
matInstLogic <- ifelse(matInst == 0, FALSE, TRUE)  # TRUE means that two authors have the same institute

### author network: dist 1000, Neuherberg, all
my_y <- mat
my_x <- list()
my_x[[1]] <- dist # distance matrix
my_x[[2]] <- meanPub # mean number of publications from two institutes
my_x[[3]] <- matCat # same category?
# restrict to Neuherberg only
location <- "Neuherberg-Neuherberg"
my_y[which(locMat != location)] <- NA
for(i in 1:length(my_x)){
  my_x[[i]][which(locMat != location)] <- NA
}

# three covariates
Original_all_x3 <- hurdleQap(y = my_y, x = my_x[1:3], removeControl = FALSE, logicMatrix = NULL, maxDist = 1000, kbasis = 8, reps = 1000)
save(Original_all_x3, file = "Workspaces/Original_all_x3_wrapper.RData")

