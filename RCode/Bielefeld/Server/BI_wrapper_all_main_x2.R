
# Application example hurdleQap
source("hurdleQap.R")

# Data preparation
load("Workspaces/EverythingNeeded_QapGamFunction1_Bielefeld.RData")
# with logic mat
matInstLogic <- ifelse(matInst == 0, FALSE, TRUE)  # TRUE means that two authors have the same institute

##### different options #####

### only main building
# author network: dist 100000, main-main, all
my_y <- mat
my_x <- list()
my_x[[1]] <- dist # distance matrix
my_x[[2]] <- meanPub # mean number of publications from two institutes
my_x[[3]] <- matCat # same category?
# restrict to main building only
location <- "main-main"
my_y[which(locMat != location)] <- NA
for(i in 1:length(my_x)){
  my_x[[i]][which(locMat != location)] <- NA
}

# two covariates
Original_all_main_x2 <- hurdleQap(y = my_y, x = my_x[1:2], removeControl = FALSE, logicMatrix = NULL, maxDist = 100000, kbasis = 8, reps = 1000)
save(Original_all_main_x2, file = "Workspaces/Original_all_main_x2_wrapper_Bielefeld.RData")