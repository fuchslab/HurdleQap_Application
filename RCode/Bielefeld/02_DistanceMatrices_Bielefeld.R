load("Workspaces/workspaceBiData_DatPrep_Bibliometrix.Rdata")


# load required packages
library(readxl)
library(tidyverse)
library(igraph)
library(stringr)
library(DescTools)

# load the distance matrices   
path <- ("Daten/Bielefeld/Distanzmatrix.xlsx")

Distdata <- path %>% 
  excel_sheets() %>% 
  set_names(excel_sheets(path = path)) %>% 
  map(read_excel, path = path) 

# first column as rownames
Distdata <- map(Distdata, as.data.frame)
Distdata <- map(Distdata, function(x) { rownames(x) <- x[,1]; x[,2:ncol(x)]})

# set diagonal to 0 and fill upper triangular matrix in lower
Distdata <- map(Distdata, function(x) { diag(x) <- 0; x })
Distdata <- map(Distdata, function(x) { x[lower.tri(x, diag = TRUE)] <- t(x)[lower.tri(x, diag = TRUE)]; x })

# give numbers to node names according to floor 
floors <- c("", "02", "01", 0:10, "X0", "X1", "X2", "X3", "X4")
for (i in 1:length(Distdata)){
  rownames(Distdata[[i]]) <- colnames(Distdata[[i]]) <- paste0(rownames(Distdata[[i]]), floors[i]) 
}

# binomial matrix to define IF there is a connection between two points
dataBin <- map(Distdata, function(x) ifelse(is.na(x), 0, 1))


# calculate distances between all points
allDist <- list()
for (i in 1:length(Distdata)){
  M <- as.matrix(Distdata[[i]])
  Mbin <- as.matrix(dataBin[[i]])
  g <- graph.adjacency(Mbin, weighted = TRUE, mode = "directed")
  E(g)$weight <-  M[which(!is.na(M))]
  # distances calculates the length of all the shortest paths from or to the vertices in the network.
  allDist[[i]] <- as.data.frame(distances(g))
}
names(allDist) <- names(Distdata)


# big distance matrix (first without Gebaudeuebergaenge)
rnames <- c()
for (i in 2:length(allDist)){
  rnames <- c(rnames, colnames(allDist[[i]]))
}

bigDist <- matrix(NA, nrow = length(rnames), ncol = length(rnames))
colnames(bigDist) <- rownames(bigDist) <- rnames

# fill single matrices in big matrix
for (k in 2:length(allDist)){
  for (i in 1:nrow(allDist[[k]])){
    for (j in 1:ncol(allDist[[k]])){
      bigDist[rownames(allDist[[k]])[i], colnames(allDist[[k]])[j]] <- Distdata[[k]][rownames(allDist[[k]])[i], colnames(allDist[[k]])[j]]
    }
  }
}

# add distances between different buildings
newNames <- c(colnames(bigDist), colnames((Distdata)[["Gebaeudeuebergaenge"]])[-which(colnames((Distdata)[["Gebaeudeuebergaenge"]]) %in% colnames(bigDist))])
bigDist <- cbind(bigDist, NA, NA, NA, NA, NA)
bigDist <- rbind(bigDist, NA, NA, NA, NA, NA)
colnames(bigDist) <- rownames(bigDist) <- newNames

for (i in 1:nrow((Distdata)[["Gebaeudeuebergaenge"]])){
  for (j in 1:ncol((Distdata)[["Gebaeudeuebergaenge"]])){
    bigDist[colnames(Distdata[["Gebaeudeuebergaenge"]])[i], colnames(Distdata[["Gebaeudeuebergaenge"]])[j]] <- (Distdata)[["Gebaeudeuebergaenge"]][i,j]
  }
}


#-------------------------------------------
# fill in steps and elevators between floors

# floor and stair distances are set so that making two floors with stairs is faster than two floors of elevator,
# but for three or more floors the elevator is faster

oneFloorElev <- 15
oneFloorStairs <- 24

# adjust distances for stairs and elevators
waitTime <- 25 # waiting time for elevator
floorsUHG <- c("02", "01", 0:10)
floorsX <- c("X0", "X1", "X2", "X3", "X4")

# nodes with elevator/stairs
elevUHG <- unique(gsub("[[:digit:]]", "", colnames(bigDist)[str_detect(colnames(bigDist), "a[^X]")]))
stairUHG <- unique(gsub("[[:digit:]]", "", colnames(bigDist)[str_detect(colnames(bigDist), "t[^X]")]))

elevX <-  unique(gsub("X[[:digit:]]", "", colnames(bigDist)[str_detect(colnames(bigDist), "aX")]))
# no stairs (without elevator) in building X 



# calculate for each a/t node the distance to the other floors according to elevator/stairs formular
# UHG
for (i in 1:length(floorsUHG)) {
  # elevator
  for (j in 1:length(elevUHG)) {
    # check if node exists
    if (paste0(elevUHG[j], floorsUHG[i]) %in% colnames(bigDist)) {
      for (k in floorsUHG[-i]) {
        # check if node exists also in an other floor
        if (paste0(elevUHG[j], k) %in% colnames(bigDist)){
          # separate between stairs and elevator for floor distances 
          # for floor distances of max. two floors stairs are faster
          if (abs(i - which(floorsUHG == k)) <= 2){
            bigDist[paste0(elevUHG[j], floorsUHG[i]), paste0(elevUHG[j], k)] <-
            bigDist[paste0(elevUHG[j], k), paste0(elevUHG[j], floorsUHG[i])] <-
            # formula for stairs
            abs(which(floorsUHG == k) - which(floorsUHG == floorsUHG[i])) * oneFloorStairs 
          }
          # for floor distances of three or more floors the elevator is faster
          else {
            bigDist[paste0(elevUHG[j], floorsUHG[i]), paste0(elevUHG[j], k)] <-
            bigDist[paste0(elevUHG[j], k), paste0(elevUHG[j], floorsUHG[i])] <-
            # formula for elevator
            waitTime + abs(which(floorsUHG == k) - which(floorsUHG == floorsUHG[i])) * oneFloorElev
          }
        }
      }
    }
  }
  # stairs
  for (j in 1:length(stairUHG)) {
    # check if node exists
    if (paste0(stairUHG[j], floorsUHG[i]) %in% colnames(bigDist)) {
      for (k in floorsUHG[-i]) {
        # check if node exists also in an other floor
        if (paste0(stairUHG[j], k) %in% colnames(bigDist))
          bigDist[paste0(stairUHG[j], floorsUHG[i]), paste0(stairUHG[j], k)] <-
            bigDist[paste0(stairUHG[j], k), paste0(stairUHG[j], floorsUHG[i])] <-
            # formula for stairs
            abs(which(floorsUHG == k) - which(floorsUHG == floorsUHG[i])) * oneFloorStairs 
      }
    }
  }
}

# Building X (only elevators with stairs)
for (i in 1:length(floorsX)) {
  for (j in 1:length(elevX)) {
    # check if node exists
    if (paste0(elevX[j], floorsX[i]) %in% colnames(bigDist)) {
      for (k in floorsX[-i]) {
        # check if node exists also in an other floor
        if (paste0(elevX[j], k) %in% colnames(bigDist)){
          # separate between stairs and elevator for floor distances 
          # for floor distances of max. two floors stairs are faster
          if (abs(i - which(floorsX == k)) <= 2){
            bigDist[paste0(elevX[j], floorsX[i]), paste0(elevX[j], k)] <-
            bigDist[paste0(elevX[j], k), paste0(elevX[j], floorsX[i])] <-
            # formula for stairs
            abs(which(floorsX == k) - which(floorsX == floorsX[i])) * oneFloorStairs  
          }
          else {
            bigDist[paste0(elevX[j], floorsX[i]), paste0(elevX[j], k)] <-
            bigDist[paste0(elevX[j], k), paste0(elevX[j], floorsX[i])] <-
            # formula for elevator
            waitTime + abs(which(floorsX == k) - which(floorsX == floorsX[i])) * oneFloorElev
          }
        }
      }
    }
  }
}


# assign dead-end street for every floor by hand

# 02
bigDist["Da02", "D02"] <- NA
bigDist["Ea02", "E02"] <- NA

# 01
bigDist["Wa01", "W01"] <- NA
bigDist["Da01", "D01"] <- NA
bigDist["Ea01", "E01"] <- NA
bigDist["Fa01", "F01"] <- NA

# 0
bigDist["Da0", "D0"] <- NA
bigDist["Ea0", "E0"] <- NA
bigDist["Fa0", "F0"] <- NA

# 1
bigDist["Wa1", "W1"] <- NA
bigDist["Da1", "D1"] <- NA
bigDist["Ea1", "E1"] <- NA
bigDist["Fa1", "F1"] <- NA

# 2
bigDist["Wa2", "W2"] <- NA
bigDist["Da2", "D2"] <- NA
bigDist["Ea2", "E2"] <- NA
bigDist["Fa2", "F2"] <- NA

# 3
bigDist["Wa3", "W3"] <- NA
bigDist["Da3", "D3"] <- NA
bigDist["Ea3", "E3"] <- NA
bigDist["Fa3", "F3"] <- NA

# 4
bigDist["Wa4", "W4"] <- NA
bigDist["Da4", "D4"] <- NA
bigDist["Ea4", "E4"] <- NA
bigDist["Fa4", "F4"] <- NA
bigDist["UMVa4", "M4"] <- NA
bigDist["CMDa4", "M4"] <- NA

# 5
bigDist["Wa4", "W4"] <- NA
bigDist["Da4", "D4"] <- NA

# 6
bigDist["VW6", "Wg6"] <- NA

# 7
bigDist["Wa7", "W7"] <- NA

# X1
bigDist["XCaX1", "XDaX1"] <- NA



# control elevator Da1, which is "cutted" fronm the rest
bigDist["Da1", ] [ which(is.na(bigDist["Da1", ])==FALSE)]

bigDist["CMDa1", ] [ which(is.na(bigDist["CMDa1", ])==FALSE)]
bigDist["BLCa2", ] [ which(is.na(bigDist["BLCa2", ])==FALSE)]
bigDist["XDaX1", ] [ which(is.na(bigDist["XDaX1", ])==FALSE)]
bigDist["XDaX0", ] [ which(is.na(bigDist["XDaX0", ])==FALSE)]

#-----------------------------------------
# binary matrix
bigDistBin <- bigDist
bigDistBin <- ifelse(is.na(bigDist), 0, 1)


# create network and calculate distances
M <- as.matrix(bigDist)
Mbin <- as.matrix(bigDistBin)
g <- graph.adjacency(Mbin, weighted = TRUE, mode = "directed")
E(g)$weight <-  M[which(!is.na(M))]

# distances calculates the length of all the shortest paths from or to the vertices in the network.
allDistBig <- as.data.frame(distances(g, mode = "out"))
set.seed(123)
plot(g, vertex.label.cex = 0.7, vertex.color = SetAlpha("tomato", 0.7),
     vertex.size = 10)




#-----------------------------------------------------------------
# load data for building-faculty structure
FacKnoten <- read_excel("Daten/Bielefeld/Fakultat_Knotenpunkte.xlsx")

# change nodes of CITEC by hand: 
FacKnoten[FacKnoten$Fakultat == "CITEC", "Knotenpunkte"] <- "CITEC"
FacKnoten[FacKnoten$Fakultat == "CITEC", "Location"] <- "out"

# calculate minimum of all distances between all nodes of one faculty with another
FacDist <- matrix(NA, nrow = nrow(FacKnoten), ncol = nrow(FacKnoten))
rownames(FacDist) <- colnames(FacDist) <- FacKnoten$Fakultat

for (i in 1:ncol(FacDist)) {
  for (j in 1:ncol(FacDist)) {
    nodesA <- trimws(unlist(str_split(FacKnoten$Knotenpunkte[i], ",")))
    nodesB <- trimws(unlist(str_split(FacKnoten$Knotenpunkte[j], ",")))
    dists <- c()
    for (a in nodesA) {
      for (b in nodesB) {
        dists <- c(dists, allDistBig[a, b])
      }
    }
    # minimum
    FacDist[i, j] <- min(dists[is.finite(dists)], na.rm = TRUE)
    
  }
}

# make matrix symmetric: calculate mean of distance AB and BA
FacDist2 <- FacDist
for (i in 1:nrow(FacDist2)){
  for (j in 1:nrow(FacDist2)){
    FacDist[i,j] <- FacDist[j,i] <- mean(FacDist2[i,j], FacDist2[j,i], na.rm = TRUE)
  }
}

# multiply steps by step length
stepl <- 0.75
# FacDist in meters
FacDistMe <- FacDist*stepl


# test how much the distance between [i,j] and [j,i] differ in general (if not symmetric) 
distDiff <- c()
for (i in 1:(ncol(FacDist2)-1)){
  for (j in (i+1):ncol(FacDist2)){
    distDiff <-c(distDiff, abs(FacDist2[i,j]-FacDist2[j,i]))
  }
}
summary(distDiff)



# distances between all institutes
M <- FacDistMe
Mbin <- ifelse(is.na(M), 0, 1)
colnames(Mbin) <- rownames(Mbin) <- colnames(M)
g <- graph.adjacency(Mbin, weighted = TRUE, mode = "directed")
E(g)$weight <-  M[which(!is.na(M))]
#plot(g)
set.seed(123)
plot(g, vertex.label.cex = 0.7, vertex.color = SetAlpha("tomato", 0.7),
     vertex.size = 10)


# set all distances within a faculty/institute to 0
diag(FacDistMe) <- 0

rm(a, b, dists, i, j, nodesA, nodesB, g, M, Mbin, k, path, FacDist2)

save.image("Workspaces/Distances_Bielefeld.Rdata")


#-----------------------------------------------------------------
#-----------------------------------------------------------------

