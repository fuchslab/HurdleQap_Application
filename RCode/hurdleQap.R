library(pscl)
library(statnet)
library(mgcv)
library(countreg)
library(rlist)
library(ggplot2)
library(gridExtra)
library(grid)

# load single functions ----
source("hurdleQapOriginal.R")
source("hurdleQapPerms.R", echo = TRUE)
source("hurdleQapCombine.R", echo = TRUE)
source("plotHurdleQap.R", echo = TRUE)

hurdleQap <- function(y, x, removeControl = FALSE, logicMatrix = NULL, maxDist, kbasis, reps){
  # original model
  modelOriginal <- hurdleQapOriginal(y = y, x = x, removeControl = removeControl,
                                        logicMatrix = logicMatrix , maxDist = maxDist, kbasis = kbasis)
  # permutation
  allperms <- hurdleQapPerms(OriginalResults = modelOriginal, reps = reps, seed = NULL)
  # combined results (original and permutations)
  combined <- hurdleQapCombine(permutationList = allperms, OriginalList = modelOriginal)
}
