load("Workspaces/DataPreparation2_Bielefeld.Rdata")

#----------------
#------ Building network
# Network with iGraph (following http://kateto.net/networks-r-igraph)
library(igraph)
library(RColorBrewer)
library(devEMF)
library(DescTools)
library(ggsci)
library(scales)

#.....................................................
# # colors
colfunc <- colorRampPalette(c("black", "lightgray"), alpha = 0.7)


instColors <- c("Biology" = "#92B168", 
                "Chemistry" = "#A08CAA", 
                "Educational Science" = "#F08C50", 
                "History and Philosophy and Theology" = "#7DB4BE", 
                "Public Health" = "#DC5A5A", 
                "Linguistics and Literary Studies" = "#8FA3B9",
                "Mathematics" = "#DCAA41", 
                "Physics" = "#649696", 
                "Psychology and Sports Science" = "#8C3250", 
                "Law" = "#8C8C96",
                "Sociology" = "#BFD02F",
                "Technology" = "#558CA0",
                "Business Administration and Economics" = "#74799B",
                "Applied Sciences" = "yellow",
                "CeBiTec" = "mediumvioletred", 
                "CITEC" = "dodgerblue4",
                "Institute for Interdisciplinary Research on Conflict and Violence" = "darkseagreen1",
                "Research Institute for Cognition and Robotics" = "lightpink1"
                )

instColors <- SetAlpha(instColors, alpha = 0.8)
sort(topics[as.integer(unique(maxInstBi))])
length(instColors) == length(unique(maxInstBi))



# Paper per Author in Network (in out)
outAuthors <- unique(c(out$Author, out$Collaborator))
# change names from authors_orig to authors
for(i in 1:length(PapersPerAu)){
  names(PapersPerAu)[i] <- authors[which(authors_orig == names(PapersPerAu)[i])]
}


PPA_out <- c()
for (i in 1:length(outAuthors)){
  pap <- PapersPerAu[[outAuthors[i]]]
  countpap <- 0
  for (j in 1: length(pap)){
    if(length(AllPapers[[pap[j]]]$Authors %in% outAuthors) > 1){
      countpap <- countpap + 1
    }
  }
  PPA_out[i] <- countpap
}
names(PPA_out) <- outAuthors

### network with weights = distance
g2 <- graph(edges=c(t(out[,1:2])), directed=FALSE)
E(g2)$weight <- out[,"dist"] 
V(g2)$numberPapers <- PPA_out[V(g2)$name]
V(g2)$institute <- topicsBi[V(g2)$name]
deg <- igraph::degree(g2, mode="all")
V(g2)$degree <- deg


pdf("Grafiken/Bielefeld/Network_Bielefeld_Authors_weightDist.pdf", width = 10, height = 10)
set.seed(123)
plot(g2, vertex.label = NA, 
     vertex.size = log(deg)*2, 
     vertex.color = instColors[V(g2)$institute])
dev.off()

# different node size (according to nr of papaers per Author in out)
pdf("Grafiken/Bielefeld/Network_Bielefeld_Authors_weightDist_NodesNrPap.pdf", width = 10, height = 10)
set.seed(123)
plot(g2, vertex.label = NA, 
     vertex.size = V(g2)$numberPapers/10, 
     vertex.color = instColors[V(g2)$institute])
dev.off()


#----------------------
### network with weights = co-authorship-index
g2_2 <- graph(edges=c(t(out[,1:2])), directed=FALSE)
E(g2_2)$weight <- out[,"weight"] 
E(g2_2)$dist <- out[,"dist"] 
V(g2_2)$institute <- topicsBi[V(g2_2)$name]
deg_2 <- igraph::degree(g2_2, mode="all")
V(g2_2)$degree <- deg_2

pdf("Grafiken/Bielefeld/Network_Bielefeld_Authors_weightIndex.pdf", width = 10, height = 10)
set.seed(123)
plot(g2_2, vertex.label = NA, 
     edge.color = setNames(colfunc(length(table(round(E(g2_2)$dist, digit = 2)))), names(table(round(E(g2_2)$dist, digit = 2))))[paste0(round(E(g2_2)$dist, digit = 2))],
     vertex.size = log(deg_2)*2, 
     vertex.color = instColors[V(g2_2)$institute])
dev.off()

#----------------------
### network with weights = inverse number of commonPapers
g2_3 <- graph(edges=c(t(out[,1:2])), directed=FALSE)
E(g2_3)$weight <- max(out[ ,"NrCommonPapers"])/out[ ,"NrCommonPapers"]
E(g2_3)$dist <- out[,"dist"] 
V(g2_3)$institute <- topicsBi[V(g2_3)$name]
deg_3 <- igraph::degree(g2_3, mode="all")
V(g2_3)$degree <- deg_3

pdf("Grafiken/Bielefeld/Network_Bielefeld_Authors_weightNumberPapers.pdf", width = 10, height = 10)
set.seed(123)
plot(g2_3, vertex.label = NA, 
     edge.color = setNames(colfunc(length(table(round(E(g2_3)$dist, digit = 2)))), names(table(round(E(g2_3)$dist, digit = 2))))[paste0(round(E(g2_3)$dist, digit = 2))],
     vertex.size = log(deg_3)*2, 
     vertex.color = instColors[V(g2_3)$institute])
dev.off()

#-----------------------------------------------------------------------------------------------------------------------------
# Network for institutes
g3 <- graph(edges=c(t(outInst[,1:2])), directed=FALSE)
E(g3)$weight <- max(outInst[ ,"NrCommonPapers"])/outInst[ ,"NrCommonPapers"]
E(g3)$dist <- outInst[,"dist"]
deg3 <- igraph::degree(g3, mode="all")
V(g3)$degree <- deg3


# vertex size according to function regarding sum of number of publications with other institutes (also double counted)
pdf("Grafiken/Bielefeld/Network_Bielefeld_Institutes_white.pdf", width = 10, height = 10)
set.seed(123)
plot.igraph(g3, vertex.label = V(g3)$name,  vertex.color = instColors[V(g3)$name], 
            edge.color = setNames(colfunc(length(table(round(E(g3)$dist, digit = 2)))),
                                  names(table(round(E(g3)$dist, digit = 2))))[paste0(round(E(g3)$dist, digit = 2))],
            edge.width = 2,
            vertex.size = unlist(lapply(V(g3)$name, function(x) 
              sum(outInst$NrCommonPapers[outInst$Institute == x | outInst$CoInstitute == x])))/10,
            vertex.label.cex = 1.2, vertex.frame.color = "white"
)
dev.off()

# vertex size according to function regarding number of publications (total of one institute) divided by 10
pdf("Grafiken/Bielefeld/Network_Bielefeld_Institutes_VertexNrPaperDiv10.pdf", width = 10, height = 10)
set.seed(123)
plot.igraph(g3, vertex.label = V(g3)$name, vertex.color = instColors[V(g3)$name],
            edge.color = setNames(colfunc(length(table(round(E(g3)$dist, digit = 2)))),
                                  names(table(round(E(g3)$dist, digit = 2))))[paste0(round(E(g3)$dist, digit = 2))],
            edge.width = 2,
            vertex.size = sapply(PPI, length)[V(g3)$name]/10,
            vertex.label.cex = 1.2, vertex.frame.color = "white"
)
dev.off()

# vertex size according to igraph::degree/2
pdf("Grafiken/Bielefeld/Network_Bielefeld_Institutes_VertexHalfdegree.pdf", width = 10, height = 10)
set.seed(123)
plot.igraph(g3, vertex.label = V(g3)$name, vertex.color = instColors[V(g3)$name],
            edge.color = setNames(colfunc(length(table(round(E(g3)$dist, digit = 2)))),
                                  names(table(round(E(g3)$dist, digit = 2))))[paste0(round(E(g3)$dist, digit = 2))],
            edge.width = 2,
            vertex.size = deg3, 
            vertex.label.cex = 1.2, vertex.frame.color = "white"
)
dev.off()


# General Function for network Properties
NetworkProps <- function(network, direct){
  propList <- list("NumberNodes" = vcount(network) ,
                   "NumberEdges" = ecount(network), 
                   "Density" = edge_density(network, loops = FALSE),
                   "Transitivity" = transitivity(network, type="global"),
                   "Diameter" = diameter(network, directed=direct),
                   "ShortestPaths" = length(shortest.paths(network)),
                   "CharactPathLength" = average.path.length(network, directed=direct),
                   "MeanDegree" = mean(igraph::degree(network, mode="all")),
                   "AvBetwCentrality" = centr_betw(network, directed=direct, normalized=T)$centralization,
                   "ConnectedComponents"= count_components(network))
  return(propList)
}

propListg2 <- NetworkProps(network = g2, direct=FALSE)
propListg2_2 <- NetworkProps(network = g2_2, direct=FALSE)
propListg2_3 <- NetworkProps(network = g2_3, direct=FALSE)
propListg3 <- NetworkProps(network = g3, direct=FALSE)

save.image("Workspaces/NetworkAnalysis_Bielefeld.Rdata")


