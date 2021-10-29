
# Application example hurdleQap
source("hurdleQap.R")

# Data preparation
load("Workspaces/EverythingNeeded_QapGamFunction1_Bielefeld.RData")
# with logic mat
matInstLogic <- ifelse(matInst == 0, FALSE, TRUE)  # TRUE means that two authors have the same institute

# institute network, all buildings
my_yI <- matI
my_xI <- list()
my_xI[[1]] <- FacDistMe[colInst, colInst]
my_xI[[2]] <- meanPubI
my_xI[[3]] <- matCatI

# three covariates
Original_inst_all_x3 <- hurdleQap(y = my_yI, x = my_xI[1:3], removeControl = FALSE, logicMatrix = NULL, maxDist = 100000, kbasis = 8, reps = 1000)
save(Original_inst_all_x3, file = "Workspaces/Original_inst_all_x3_wrapper_Bielefeld.RData")
