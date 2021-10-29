
# Application example hurdleQap
source("hurdleQap.R")

# Data preparation
load("Workspaces/EverythingNeeded_QapGamFunction1_Bielefeld.RData")
# with logic mat
matInstLogic <- ifelse(matInst == 0, FALSE, TRUE)  # TRUE means that two authors have the same institute

# institute network, main building

my_yI <- matI
my_xI <- list()
my_xI[[1]] <- FacDistMe[colInst, colInst]
my_xI[[2]] <- meanPubI
my_xI[[3]] <- matCatI
# restrict to main only
location <- "main-main"
my_yI[which(locMatI != location)] <- NA
for(i in 1:length(my_xI)){
  my_xI[[i]][which(locMatI != location)] <- NA
}

# two covariates
Original_inst_main_x2 <- hurdleQap(y = my_yI, x = my_xI[1:2], removeControl = FALSE, logicMatrix = NULL, maxDist = 100000, kbasis = 8, reps = 1000)
#save(Original_inst_main_x2, file = "Workspaces/Original_inst_main_x2_wrapper_Bielefeld.RData")

rversion <- R.Version()
rm(list=setdiff(ls(), c("rversion", "Original_inst_main_x2")))
save.image("Workspaces/Original_inst_main_x2_wrapper_Bielefeld_test.RData")
