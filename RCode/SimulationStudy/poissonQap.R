source("poissonQapOriginal.R")
source("poissonQapPerms.R")
source("poissonQapCombine.R")


poissonQap <- function(y, x, removeControl = FALSE, logicMatrix = NULL, maxDist, kbasis, reps){
  # original model
  modelOriginal <- poissonOriginal(y = y, x = x, removeControl = removeControl,
                                     logicMatrix = logicMatrix , maxDist = maxDist, kbasis = kbasis)
  # permutation
  allperms <- poissonQapPerms(OriginalResults = modelOriginal, reps = reps, seed = NULL)
  # combined results (original and permutations)
  combined <- poissonQapCombine(permutationList = allperms, OriginalList = modelOriginal)
}
