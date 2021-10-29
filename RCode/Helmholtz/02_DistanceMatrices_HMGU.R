load("Workspaces/workspaceHMGU_DatPrep_Bibliometrix.Rdata")

# load required packages
library(readxl)
library(tidyverse)
library(igraph)
library(stringr)
library(DescTools)

# load the distance matrices   
data1 <- as.data.frame(read_excel("Daten/Helmholtz/HGMU_Distanzmatrix.xlsx"))
data1 <- data1[-1]
rownames(data1) <- colnames(data1)

data2 <- as.data.frame(read_excel("Daten/Helmholtz/Distanzmatrix_ausserhalb_neuherberg.xlsx"))
data2 <- data2[-1]
rownames(data2) <- colnames(data2)
data2 <- data2/0.72 # (step length, because here we have already meters)

# add distances between different buildings
newNames <- c(colnames(data1), colnames(data2)[-which(colnames(data2) %in% colnames(data1))])
data1 <- cbind(data1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
data1 <- rbind(data1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
colnames(data1) <- rownames(data1) <- newNames

for (i in 1:nrow(data2)){
  for (j in 1:ncol(data2)){
    data1[colnames(data2)[i], colnames(data2)[j]] <- data2[i,j]
  }
}

# fill upper triangular matrix in lower
data1[lower.tri(data1, diag = TRUE)] <- t(data1)[lower.tri(data1, diag = TRUE)]
data2[lower.tri(data2, diag = TRUE)] <- t(data2)[lower.tri(data2, diag = TRUE)]

# binomial matrix to define IF there is a connection between two points
dataBin <- ifelse(is.na(data1), 0, 1)

# calculate distances between all points
M <- as.matrix(data1)
Mbin <- as.matrix(dataBin)
g <- graph.adjacency(Mbin, weighted = TRUE, mode = "directed")
E(g)$weight <-  M[which(!is.na(M))]
#plot(g)

# distances calculates the length of all the shortest paths from or to the vertices in the network.
allDist <- as.data.frame(distances(g))
set.seed(123)
plot(g, vertex.label.cex = 0.7, vertex.color = SetAlpha("tomato", 0.7),
     vertex.size = 10)


#-----------------------------------------------------------------
# load data for building-faculty structure

GebKnoten <- read_excel("Daten/Helmholtz/Zuordnung_Institute_Gebaeude_reduziert.xlsx", na = "NA")

GebKnoten$ID <- paste0("ID", 1:nrow(GebKnoten))

length(unique(GebKnoten$Building)) # 54 unique buildings

# delete DES and HPC (not clear which builing/node)
GebKnoten <- GebKnoten[-c(106,107), ]

# Calculate minimum of all possible distances between two buildings
BuildMat <- matrix(NA, ncol = length(unique(GebKnoten$Building)), nrow = length(unique(GebKnoten$Building)))
colnames(BuildMat) <- rownames(BuildMat) <- sort(unique(GebKnoten$Building))

buildNodes <- GebKnoten[order(GebKnoten$Building),c("Building", "Knoten")]
buildNodes <- buildNodes[!duplicated(buildNodes),]

for (i in 1:ncol(BuildMat)) {
  for (j in 1:ncol(BuildMat)) {
    nodesA <- trimws(unlist(str_split(buildNodes$Knoten[i], ",")))
    nodesB <- trimws(unlist(str_split(buildNodes$Knoten[j], ",")))
    dists <- c()
    for (a in nodesA){
      for(b in nodesB){
        dists <- c(dists, allDist[a,b])
      }
    }
    BuildMat[i, j] <- min(dists, na.rm = TRUE) # mean(dists, na.rm = TRUE)
  }
}
# set diagonal to 0
diag(BuildMat) <- 0

# which institute is in which building?
InstBuild <- GebKnoten[,c("Abbreviation", "Building")] 

# set several nodes to one building
BuildInst <- as.data.frame(InstBuild) %>% group_by(Abbreviation) %>% summarize(Buildings = paste(sort(Building), collapse = ","))


InstDist <- matrix(NA, ncol = nrow(BuildInst), nrow = nrow(BuildInst))
colnames(InstDist) <- rownames(InstDist) <- BuildInst$Abbreviation

# minimum between all possible distances
for (i in 1:ncol(InstDist)) {
  for (j in 1:ncol(InstDist)) {
    nodesA <- trimws(unlist(str_split(BuildInst$Buildings[i], ",")))
    nodesB <- trimws(unlist(str_split(BuildInst$Buildings[j], ",")))
    dists <- c()
    for (a in nodesA) {
      for (b in nodesB) {
        dists <- c(dists, BuildMat[a, b])
      }
    }
    InstDist[i, j] <- min(dists[is.finite(dists)], na.rm = TRUE)
  }
}

# multiply steps by step length in meter
stepl <- 0.72

# FacDist in meters
InstDistMe <- InstDist*stepl
FacDistMe <- InstDistMe

# set all distances within a faculty/institute to 0
diag(FacDistMe) <- 0

rm(a, b, dists, i, j, nodesA, nodesB, g, M, Mbin)
save.image("Workspaces/Distances_Helmholtz.Rdata")

