load("Workspaces/DataPreparation2_HMGU.Rdata")

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
# colors
n <- length(uniInst)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
instColors <- col_vector[1:length(uniInst)]
names(instColors) <- uniInst
instColors <- SetAlpha(instColors, alpha = 0.7)

# cat Colors
length(categories)
show_col(pal_d3("category20")(20))
catColors <- pal_d3("category20", 0.7)(20)[c(1, 2, 3, 4, 5, 6, 8)]
names(catColors) <- categories 

# col function
colfunc <- colorRampPalette(c("black", "lightgray"), alpha = 0.7)

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
V(g2)$institute <- topicsHMGU[V(g2)$name]
V(g2)$category <- allCat[V(g2)$institute]
deg <- igraph::degree(g2, mode="all")
V(g2)$degree <- deg

pdf("Helmholtz/Network_Authors_weightDist.pdf", width = 10, height = 10)
set.seed(123)
plot(g2, vertex.label = NA, 
     vertex.size = log(deg)*2, 
     vertex.color = catColors[V(g2)$category])
dev.off()

# different node size (according to nr of papers per Author in out)
pdf("Helmholtz/Network_Authors_weightDist_NodesNrPap.pdf", width = 10, height = 10)
set.seed(123)
plot(g2, vertex.label = NA, 
     vertex.size = V(g2)$numberPapers/10, 
     vertex.color = catColors[V(g2)$category])
dev.off()

#----------------------
### network with weights = co-authorship-index
g2_2 <- graph(edges=c(t(out[,1:2])), directed=FALSE)
E(g2_2)$weight <- out[,"weight"] 
E(g2_2)$dist <- out[,"dist"] 
V(g2_2)$institute <- topicsHMGU[V(g2_2)$name]
V(g2_2)$category <- allCat[V(g2_2)$institute]
deg_2 <- igraph::degree(g2_2, mode="all")
V(g2_2)$degree <- deg_2

pdf("Helmholtz/Network_Authors_weightIndex.pdf", width = 10, height = 10)
set.seed(123)
plot(g2_2, vertex.label = NA, 
     edge.color = setNames(colfunc(length(table(round(E(g2_2)$dist, digit = 2)))), names(table(round(E(g2_2)$dist, digit = 2))))[paste0(round(E(g2_2)$dist, digit = 2))],
     vertex.size = log(deg_2)*2, 
     vertex.color = catColors[V(g2_2)$category])
dev.off()

#----------------------
### network with weights = inverse number of commonPapers
g2_3 <- graph(edges=c(t(out[,1:2])), directed=FALSE)
E(g2_3)$weight <- max(out[ ,"NrCommonPapers"])/out[ ,"NrCommonPapers"]
E(g2_3)$dist <- out[,"dist"] 
V(g2_3)$institute <- topicsHMGU[V(g2_3)$name]
V(g2_3)$category <- allCat[V(g2_3)$institute]
deg_3 <- igraph::degree(g2_3, mode="all")
V(g2_3)$degree <- deg_3

pdf("Helmholtz/Network_Authors_weightNumberPapers.pdf", width = 10, height = 10)
set.seed(123)
plot(g2_3, vertex.label = NA, 
     edge.color = setNames(colfunc(length(table(round(E(g2_3)$dist, digit = 2)))), names(table(round(E(g2_3)$dist, digit = 2))))[paste0(round(E(g2_3)$dist, digit = 2))],
     vertex.size = log(deg_3)*2, 
     vertex.color = catColors[V(g2_3)$category])
dev.off()

#--------------------------------------------------------------
# Network for institutes
g3 <- graph(edges=c(t(outInst[,1:2])), directed=FALSE)
E(g3)$weight <- max(outInst[ ,"NrCommonPapers"])/outInst[ ,"NrCommonPapers"]
E(g3)$dist <- outInst[,"dist"]
V(g3)$category <- allCat[V(g3)$name]
deg3 <- igraph::degree(g3, mode="all")
V(g3)$degree <- deg3

# vertex size according to function regarding sum of number of publications with other institutes (also double counted)
pdf("Helmholtz/Network_Institutes_white.pdf", width = 10, height = 10)
set.seed(123)
plot.igraph(g3, vertex.label = V(g3)$name, vertex.color = catColors[V(g3)$category],
            edge.color = setNames(colfunc(length(table(round(E(g3)$dist, digit = 2)))),
                                  names(table(round(E(g3)$dist, digit = 2))))[paste0(round(E(g3)$dist, digit = 2))],
            edge.width = 2,
            vertex.size = unlist(lapply(V(g3)$name, function(x) 
              sum(outInst$NrCommonPapers[outInst$Institute == x | outInst$CoInstitute == x])))/100,
            vertex.label.cex = 1.2, vertex.frame.color = "white"
)
dev.off()

# vertex size according to function regarding number of publications (total of one institute) divided by 20
pdf("Helmholtz/Network_Bielefeld_Institutes_VertexNrPaperDiv20.pdf", width = 10, height = 10)
set.seed(123)
plot.igraph(g3, vertex.label = V(g3)$name, vertex.color = catColors[V(g3)$category],
            edge.color = setNames(colfunc(length(table(round(E(g3)$dist, digit = 2)))),
                                  names(table(round(E(g3)$dist, digit = 2))))[paste0(round(E(g3)$dist, digit = 2))],
            edge.width = 2,
            vertex.size = sapply(PPI, length)[V(g3)$name]/20,
            vertex.label.cex = 1.2, vertex.frame.color = "white"
)
dev.off()

# vertex size according to degree/3
pdf("Helmholtz/Network_Bielefeld_Institutes_VertexThirdDegree.pdf", width = 10, height = 10)
set.seed(123)
plot.igraph(g3, vertex.label = V(g3)$name, vertex.color = catColors[V(g3)$category],
            edge.color = setNames(colfunc(length(table(round(E(g3)$dist, digit = 2)))),
                                  names(table(round(E(g3)$dist, digit = 2))))[paste0(round(E(g3)$dist, digit = 2))],
            edge.width = 2,
            vertex.size = deg3/3,
            vertex.label.cex = 1.2, vertex.frame.color = "white"
)
dev.off()


# General Function for network Properties
NetworkProps <- function(network, direct){
  propList <- list("NumberNodes" = vcount(network) ,
                   "NumberEdges" = ecount(network), 
                   "Density" = edge_density(network, loops = FALSE),
                   "Transitivity" = transitivity(network, type = "global"),
                   "Diameter" = diameter(network, directed = direct),
                   "ShortestPaths" = length(shortest.paths(network)),
                   "CharactPathLength" = average.path.length(network, directed = direct),
                   "MeanDegree" = mean(igraph::degree(network, mode="all")),
                   "AvBetwCentrality" = centr_betw(network, directed = direct, normalized = TRUE)$centralization,
                   "ConnectedComponents"= count_components(network))
  return(propList)
}

propListg2 <- NetworkProps(network = g2, direct = FALSE)
propListg2_2 <- NetworkProps(network = g2_2, direct = FALSE)
propListg2_3 <- NetworkProps(network = g2_3, direct = FALSE)
propListg3 <- NetworkProps(network = g3, direct = FALSE)

save.image("Workspaces/NetworkAnalysis_HMGU.Rdata")
