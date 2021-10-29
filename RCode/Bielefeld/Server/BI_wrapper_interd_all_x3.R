
# Application example hurdleQap
source("hurdleQap.R")

# Data preparation
load("Workspaces/EverythingNeeded_QapGamFunction1_Bielefeld.RData")
# with logic mat
matInstLogic <- ifelse(matInst == 0, FALSE, TRUE)  # TRUE means that two authors have the same institute

##### different options #####

### all buildings
# author network: dist 100000, all, all
my_y <- mat
my_x <- list()
my_x[[1]] <- dist # distance matrix
my_x[[2]] <- meanPub # mean number of publications from two institutes
my_x[[3]] <- matCat

# author network: dist 100000, all, interdisciplinary
# three covariates
Original_interd_all_x3 <- hurdleQap(y = my_y, x = my_x[1:3], removeControl = TRUE, logicMatrix = matInstLogic, maxDist = 100000, kbasis = 8, reps = 1000)
save(Original_interd_all_x3, file = "Workspaces/Original_interd_all_x3_wrapper_Bielefeld.RData")

