
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

# two covariates
Original_all_all_x2 <- hurdleQap(y = my_y, x = my_x[1:2], removeControl = FALSE, logicMatrix = NULL, maxDist = 100000, kbasis = 8, reps = 1000)
save(Original_all_all_x2, file = "Workspaces/Original_all_all_x2_wrapper_Bielefeld.RData")
