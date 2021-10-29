# Application of functions from package; Helmholtz data

# Application example hurdleQap
source("hurdleQap.R")

# Data preparation
load("Workspaces/EverythingNeeded_QapGamFunction1_HMGU.RData")
# with logic mat
matInstLogic <- ifelse(matInst == 0, FALSE, TRUE)  # TRUE means that two authors have the same institute

### institute network
my_yI <- matI
my_xI <- list()
my_xI[[1]] <- FacDistMe[colInst, colInst]
my_xI[[2]] <- meanPubI
my_xI[[3]] <- matCatI
# restrict to Neuherberg only
location <- "Neuherberg-Neuherberg"
my_yI[which(locMatI != location)] <- NA
for(i in 1:length(my_xI)){
  my_xI[[i]][which(locMatI != location)] <- NA
}

# three covariates
Original_inst_x3 <- hurdleQap(y = my_yI, x = my_xI[1:3], removeControl = FALSE, logicMatrix = NULL, maxDist = 1000, kbasis = 8, reps = 1000)
save(Original_inst_x3, file = "Workspaces/Permutations/Original_inst_x3.RData")

