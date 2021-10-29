# load packages
library(rlist)
library(ggplot2)
library(igraph)
library(RColorBrewer)
library(devEMF)
library(DescTools)
library(ggsci)
library(scales)
library(grid)
library(gridExtra)
library(pscl)
library(statnet)
library(voxel)
library(mgcv)
library(tidyverse)

#_________________________________________________________________________________
#_________________________________________________________________________________
# Helmholtz

load("Workspaces/CompleteAnalysis_HMGU.Rdata")
path <- "./Graphics/Helmholtz/"

## Number of publications per institute
numPub <- long %>%
  group_by(dep, category) %>%
  summarise(count_dep = n()) %>%
  ggplot(aes(x = reorder(dep, -count_dep), y = count_dep)) +
  geom_bar(stat = 'identity', aes(fill = category))+
  labs(title = "Helmholtz Zentrum München", subtitle = "Year 2015 - 2019",
       x = "", y = "Number of publications") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1.3)),
        title = element_text(size = rel(1.5)), 
        legend.position = "bottom",
        legend.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) +
  scale_fill_manual(name = "Research category", values = catColors[names(table(long$category))])

sort(table(long$dep), decreasing = TRUE)

pdf(paste0(path, "NumberPublicationsPerCategory_querAxis_ggplot.pdf"), width = 12, height = 7)
numPub
dev.off()

## Number of different institutes per publication
nDep <- map(AllPapers, "Department") %>%
  map_int(., ~ unique(length(.x)))

df <- data.frame(n = as.factor(nDep), names = names(nDep))

pdf(paste0(path, "NumberInstitutesPerPublication.pdf"), width = 9, height = 10)
ggplot(data = df, aes(n)) +
  labs(title = "Number of different institutes per publication", 
       y = "Number of publications", x = "Number of different institutes") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 17)) +
  geom_bar(fill = "#6572B8") + 
  scale_x_discrete(labels = as.character(levels(df$n)))
dev.off()

## Number of authors per publication
nAut <- map(AllPapers, "Authors") %>%
  map_int(., ~ unique(length(.x)))
df2 <- data.frame(n = as.factor(nAut[which(nAut > 0)]), names = names(nAut[which(nAut > 0)]))
summary(nAut[which(nAut > 0)])

pdf(paste0(path, "NumberAuthorsPerPublication.pdf"), width = 9, height = 10)
ggplot(data = df2, aes(n)) + 
  labs(title = "Number of authors per publication", 
       y = "Number of publications", x = "Number of authors") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 17)) +
  geom_bar(fill = "#6572B8") + 
  scale_x_discrete(labels = levels(df2$n))
dev.off()

#----------------------------------------------------------------
### Networks
#Network properties
propListg2

##### Author network: network weights according to distances
# Different node size: according to number of papers per author in out

pdf(paste0(path, "HelmholtzAuthors_NodeSizeNumberPaper_WeightsDistances.pdf"), width = 10, height = 10)
par(mar=c(0,0,0,0))
set.seed(123)
plot(g2, vertex.label = NA, 
     vertex.size = log(degree(g2))*2,
     vertex.color = catColors[V(g2)$category])
dev.off()

##### Author network: network weights according to co-authorship index
pdf(paste0(path, "HelmholtzAuthors_WeightsCoauthorIndex.pdf"), width = 10, height = 10)
par(mar=c(0,0,0,0))
set.seed(123)
plot(g2_2, vertex.label = NA, 
     vertex.size = log(degree(g2_2))*2, 
     vertex.color = catColors[V(g2_2)$category])
dev.off()

##### Author network: network weights according to number of papers
pdf(paste0(path, "HelmholtzAuthors_WeightsNumberPapers.pdf"), width = 10, height = 10)
par(mar=c(0,0,0,0))
set.seed(123)
plot(g2_3, vertex.label = NA, 
     vertex.size = log(degree(g2_3))*2, 
     vertex.color = catColors[V(g2_3)$category])
dev.off()


#### Institute network: network weights according to distances
# Network properties
propListg3

set.seed(123)
plot.igraph(g3, vertex.label = V(g3)$name, vertex.color = catColors[V(g3)$category],
            vertex.size = unlist(lapply(V(g3)$name, function(x) sum(outInst$NrCommonPapers[outInst$Institute == x | outInst$CoInstitute == x])))/100,#log(deg3)*5
            vertex.label.cex = 1.2, vertex.label.color = "black"
)


pdf(paste0(path, "HelmholtzInstitutes_WeightsDistances_black.pdf"), width = 10, height = 10)
par(mar=c(0,0,0,0))
set.seed(999)
plot.igraph(g3, vertex.label = V(g3)$name, vertex.color = catColors[V(g3)$category],
            vertex.size = unlist(lapply(V(g3)$name, function(x) sum(outInst$NrCommonPapers[outInst$Institute == x | outInst$CoInstitute == x])))/50,#log(deg3)*5
            vertex.label.cex = 1.2, vertex.label.color = "black", margin = -0.08
)
dev.off()

pdf(paste0(path, "HelmholtzInstitutes_WeightsDistances_wihte.pdf"), width = 10, height = 10)
par(mar=c(0,0,0,0))
set.seed(999)
plot.igraph(g3, vertex.label = V(g3)$name, vertex.color = catColors[V(g3)$category],
            vertex.size = unlist(lapply(V(g3)$name, function(x) sum(outInst$NrCommonPapers[outInst$Institute == x | outInst$CoInstitute == x])))/50,#log(deg3)*5
            vertex.label.cex = 1.2, vertex.label.color = "black",  vertex.frame.color = "white", margin = -0.08
)
dev.off()
#-----------------------------
# Model plots
# see extra file "PublicationPlot_Gam.R" and "PublicationPlot_Glm.R" 


#----------------------------------------------------------------
# distances
topicsAuthors <- maxInst[namesHMGU]
names(topicsAuthors) <- namesHMGU
table(topicsAuthors)
# distance vector. number of people in one institute can theoretically have connections to ALL other people in all other institutes
possibleDists <- c()

# only take each couple once (AB = BA)
for (i in 1:length(names(table(topicsAuthors)))){ 
  for (j in i: length(names(table(topicsAuthors)))){
    a <- names(table(topicsAuthors))[i]
    b <- names(table(topicsAuthors))[j]
    if(i == j){
      possibleDists <- c(possibleDists, rep(FacDistMe[a,b], times = (as.integer(table(topicsAuthors)[a])*(as.integer(table(topicsAuthors)[b])-1))))
    }
    else {
      possibleDists <- c(possibleDists, rep(FacDistMe[a,b], times = (as.integer(table(topicsAuthors)[a])*as.integer(table(topicsAuthors)[b]))))
    }
  }
}


# only interdisciplinary ones (no within Faculty distances)
possibleDistsInterDis <- c()
for (i in 1:(length(names(table(topicsAuthors)))-1)){ # only until length -1
  for (j in (i+1): length(names(table(topicsAuthors)))){ # from i+1 to avoid distances from same institute
    a <- names(table(topicsAuthors))[i]
    b <- names(table(topicsAuthors))[j]
    possibleDistsInterDis <- c(possibleDistsInterDis, 
                               rep(FacDistMe[a,b], 
                                   times = (as.integer(table(topicsAuthors)[a])*as.integer(table(topicsAuthors)[b]))))
  }
}


### real distances
# all: out$dist

# interdisciplinary
outDistInterDis <- c()
for(i in 1:nrow(out)){
  if(out$instAut[i] != out$instCol[i]){
    outDistInterDis <- c(outDistInterDis, out$dist[i])
  }
}

# proportion of collaborating author pairs compared to all author pairs
## all
length(out$dist)/length(possibleDists)
## interdis
length(outDistInterDis)/length(possibleDistsInterDis)

length(possibleDists)
length(out$dist)

dat <- data.frame(xx = c(possibleDists, out$dist),
                  distances = rep(c("all possible distances", "all co-author distances"),
                                  times = c(length(possibleDists), length(out$dist))))
dat$distances <- factor(dat$distances, levels = c("all possible distances", "all co-author distances"))

dat1000 <- dat[dat$xx < 1000, ]
dat1000$distances <- factor(dat1000$distances, levels = c("all possible distances", "all co-author distances"))



length(possibleDistsInterDis)
length(outDistInterDis)

dat2 <- data.frame(xx = c(possibleDistsInterDis, outDistInterDis), 
                   `Interdisciplinary distances` = rep(c("all possible interd. distances", "all interd. co-author distances"),
                                                       times = c(length(possibleDistsInterDis), length(outDistInterDis))))
dat2$Interdisciplinary.distances <- factor(dat2$Interdisciplinary.distances, 
                                           levels = c("all possible interd. distances", "all interd. co-author distances"))

dat2_1000 <- dat2[dat2$xx < 1000, ]
dat2_1000$Interdisciplinary. <- factor(dat2_1000$Interdisciplinary., levels = c("all possible distances", "all co-author distances"))

# KS-Tests
test3 <- ks.test(possibleDists[possibleDists <= 1000], out$dist[out$dist <= 1000], alternative = "two.sided")
test6 <- ks.test(possibleDistsInterDis[possibleDistsInterDis <= possibleDistsInterDis], outDistInterDis[outDistInterDis <= 1000], alternative = "two.sided")

# save plots
CDF6_HMGU <- ggplot(data = dat2_1000, aes(xx))
save(CDF6_HMGU, file = paste0(path,"CDF6_HMGU.Rdata"))

CDF3_HMGU <- ggplot(data = dat1000, aes(xx))
save(CDF3_HMGU, file = paste0(path,"CDF3_HMGU.Rdata"))


rm(list = setdiff(ls(), c("CDF3_HMGU", "CDF6_HMGU")))

#_________________________________________________________________________________
#_________________________________________________________________________________
# Bielefeld

load("Workspaces/CompleteAnalysis_Bielefeld.Rdata")
path <- "./Graphics/Bielefeld/"

## Number of publications per institute
numPub <- long %>%
  group_by(dep) %>%
  summarise(count_dep = n()) %>%
  ggplot(aes(x = reorder(dep, -count_dep), y = count_dep)) +
  geom_bar(stat = 'identity', aes(fill = dep))+
  labs(title = "Bielefeld University", subtitle = "Year 2015 - 2019",
       x = "", y = "Number of publications") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.3)),
        title = element_text(size = rel(1.5)), 
        legend.position = "None") +
  scale_fill_manual(values = instColors[names(table(long$dep))])

sort(table(long$dep), decreasing = TRUE)

pdf(paste0(path, "NumberPublicationsPerCategory_querAxis_ggplot.pdf"), width = 7, height = 10)
numPub
dev.off()


## Number of different institutes per publication
nDep <- map(AllPapers, "Department") %>%
  map_int(., ~ unique(length(.x)))

df <- data.frame(n = as.factor(nDep), names = names(nDep))
table(df$n)

pdf(paste0(path, "NumberInstitutesPerPublication.pdf"), width = 9, height = 10)
ggplot(data = df, aes(n)) +
  labs(title = "Number of different institutes per publication", 
       y = "Number of publications", x = "Number of different institutes") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 17)) +
  geom_bar(fill = "#6572B8") + 
  scale_x_discrete(labels = as.character(levels(df$n)))
dev.off()


## Number of authors per publication
nAut <- map(AllPapers, "Authors") %>%
  map_int(., ~ unique(length(.x)))
df2 <- data.frame(n = as.factor(nAut[which(nAut > 0)]), names = names(nAut[which(nAut > 0)]))
table(df2$n)
summary(df2$n)
summary(nAut[which(nAut > 0)])

pdf(paste0(path, "NumberAuthorsPerPublication.pdf"), width = 9, height = 10)
ggplot(data = df2, aes(n)) + 
  labs(title = "Number of authors per publication", 
       y = "Number of publications", x = "Number of authors") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 17)) +
  geom_bar(fill = "#6572B8") + 
  scale_x_discrete(labels = levels(df2$n))
dev.off()

#----------------------------------------------
# Networks

##### Author network: network weights according to distances
# Different node size: according to number of papers per author in out

pdf(paste0(path, "BielefeldAuthors_NodeSizeNumberPaper_WeightsDistances.pdf"), width = 10, height = 10)
par(mar=c(0,0,0,0))
set.seed(123)
plot(g2, vertex.label = NA, 
     vertex.size = log(degree(g2))*2, 
     vertex.color = instColors[V(g2)$institute])
dev.off()

##### Author network: network weights according to co-authorship index
pdf(paste0(path, "BielefeldAuthors_WeightsCoauthorIndex.pdf"), width = 10, height = 10)
par(mar=c(0,0,0,0))
set.seed(123)
plot(g2_2, vertex.label = NA, 
     vertex.size = log(degree(g2_2))*2, 
     vertex.color = instColors[V(g2_2)$institute])
dev.off()

##### Author network: network weights according to number of papers
pdf(paste0(path, "BielefeldAuthors_WeightsNumberPapers.pdf"), width = 10, height = 10)
par(mar=c(0,0,0,0))
set.seed(123)
plot(g2_3, vertex.label = NA, 
     vertex.size = log(degree(g2_3))*2, 
     vertex.color = instColors[V(g2_3)$institute])
dev.off()



#### Institute network: network weights according to distances

# Network properties
propListg3


shortNames <- c("Applied Sciences","Biology",
                "CeBiTec","Chemistry",
                "CITEC", "Educational Science", 
                "HistPhilTheo","Public Health",
                "Ling and Lit", "Physics" ,
                "PsychSports", "Sociology", 
                "Technology","ConflictViolence", 
                "Maths", "BusinessAdminEcon",
                "Law" ,"CognitionRobotics")

pdf(paste0(path, "BielefeldInstitutes_WeightsDistances.pdf"), width = 10, height = 10)
par(mar=c(0,1,0,0)+.1)
set.seed(999)
plot.igraph(g3, vertex.label = shortNames,  vertex.color = instColors[V(g3)$name], 
            vertex.size = unlist(lapply(V(g3)$name, function(x) 
              sum(outInst$NrCommonPapers[outInst$Institute == x | outInst$CoInstitute == x])))/10,
            vertex.label.cex = 1.8, vertex.frame.color = "white", vertex.label.color = "black",
            margin = -0.1
)
dev.off()

pdf(paste0(path, "BielefeldInstitutes_WeightsDistances_black.pdf"), width = 10, height = 10)
par(mar=c(0,1,0,0)+.1)
set.seed(999)
plot.igraph(g3, vertex.label = shortNames,  vertex.color = instColors[V(g3)$name], 
            vertex.size = unlist(lapply(V(g3)$name, function(x) 
              sum(outInst$NrCommonPapers[outInst$Institute == x | outInst$CoInstitute == x])))/10,
            vertex.label.cex = 1.8, vertex.frame.color = "black", vertex.label.color = "black",
            margin = -0.09
)
dev.off()

#-----------------------------
# Model plots
# see extra file "PublicationPlot_Gam.R" and "PublicationPlot_Glm.R" 

#_________________________________________________________________________________
#_________________________________________________________________________________
#### Distances
# distances
topicsAuthors <-  names(topics[as.integer(maxInst[namesBi])])
names(topicsAuthors) <- namesBi
table(topicsAuthors)
# distance vector. number of people in one institute can theoretically have connections to ALL other people in all other institutes
possibleDists <- c()

# only take each couple once (AB = BA)
for (i in 1:length(names(table(topicsAuthors)))){ 
  for (j in i: length(names(table(topicsAuthors)))){
    a <- names(table(topicsAuthors))[i]
    b <- names(table(topicsAuthors))[j]
    if(i == j){
      possibleDists <- c(possibleDists, rep(FacDistMe[a,b], times = (as.integer(table(topicsAuthors)[a])*(as.integer(table(topicsAuthors)[b])-1))))
    }
    else {possibleDists <- c(possibleDists, rep(FacDistMe[a,b], times = (as.integer(table(topicsAuthors)[a])*as.integer(table(topicsAuthors)[b]))))
    }
  }
}

# only interdisciplinary ones (no within Faculty distances)
possibleDistsInterDis <- c()
for (i in 1:(length(names(table(topicsAuthors)))-1)){ # only until length -1
  for (j in (i+1): length(names(table(topicsAuthors)))){ # from i+1 to avoid distances from same institute
    a <- names(table(topicsAuthors))[i]
    b <- names(table(topicsAuthors))[j]
    possibleDistsInterDis <- c(possibleDistsInterDis, 
                               rep(FacDistMe[a,b], 
                                   times = (as.integer(table(topicsAuthors)[a])*as.integer(table(topicsAuthors)[b]))))
  }
}

### real distances
# all: out$dist

# interdisciplinary
outDistInterDis <- c()
for(i in 1:nrow(out)){
  if(out$instAut[i] != out$instCol[i]){
    outDistInterDis <- c(outDistInterDis, out$dist[i])
  }
}

# proportion of collaborating author pairs compared to all author pairs
## all
length(out$dist)/length(possibleDists)
## interdis
length(outDistInterDis)/length(possibleDistsInterDis)

length(possibleDists)
length(out$dist)


dat <- data.frame(xx = c(possibleDists, out$dist),
                  distances = rep(c("all possible distances", "all co-author distances"),
                                  times = c(length(possibleDists), length(out$dist))))
dat$distances <- factor(dat$distances, levels = c("all possible distances", "all co-author distances"))

dat1000 <- dat[dat$xx < 1000, ]
dat1000$distances <- factor(dat1000$distances, levels = c("all possible distances", "all co-author distances"))

length(possibleDistsInterDis)
length(outDistInterDis)

dat2 <- data.frame(xx = c(possibleDistsInterDis, outDistInterDis), 
                   `Interdisciplinary distances` = rep(c("all possible interd. distances", "all interd. co-author distances"), 
                                                       times = c(length(possibleDistsInterDis), length(outDistInterDis))))
dat2$Interdisciplinary.distances <- factor(dat2$Interdisciplinary.distances, 
                                           levels = c("all possible interd. distances", "all interd. co-author distances"))
dat2_1000 <- dat2[dat2$xx < 1000, ]
dat2_1000$Interdisciplinary. <- factor(dat2_1000$Interdisciplinary., levels = c("all possible distances", "all co-author distances"))


# KS-Tests
ks.test(possibleDists[possibleDists <= 1000], out$dist[out$dist <= 1000], alternative = "two.sided")
ks.test(possibleDistsInterDis, outDistInterDis, alternative = "two.sided")


# CDFs
# Helmholtz all distances

load("D:/Users/hannah.busen/Documents/HannahICB/4 Promotion/MIT paper/NetworkProject/Publication/nws-readme/Figures/PublicationPlots/Helmholtz/CDF3_HMGU.Rdata")

CDF3_HMGU <- CDF3_HMGU + 
  labs(x = "Distance (in m)", y = "Probability") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 22),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "None") +
  stat_ecdf(aes(color = distances), size = 2, geom = "step", pad = FALSE) +
  scale_color_manual(values = c("darkgrey", "#6572B8"), name = "Distances") +
  annotate(geom = "text", y = 0.95, x = 0, label = "A", size = 10)

# Helmholtz interdisc. distances
load("D:/Users/hannah.busen/Documents/HannahICB/4 Promotion/MIT paper/NetworkProject/Publication/nws-readme/Figures/PublicationPlots/Helmholtz/CDF6_HMGU.Rdata")

CDF6_HMGU <- CDF6_HMGU + 
  labs(x = "Distance (in m)", y = "Probability") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 22),
        legend.position = "None") +
  stat_ecdf(aes(color = Interdisciplinary.distances), size = 2, geom = "step", pad = FALSE) +
  scale_color_manual(values = c("darkgrey", "#6572B8"), name = "Interdisciplinary \ndistances") +
  annotate(geom = "text", y = 0.95, x = 0, label = "B", size = 10)

# Bielefeld all distances
CDF1_Bielefeld <- ggplot(data = dat, aes(xx)) +
  labs(x = "Distance (in m)", y = "Probability") +
  ylim(0,1)  +
  theme_bw() +
  theme(plot.title = element_text(size = 22),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "None") + 
  stat_ecdf(aes(color = distances), size = 2, geom = "step", pad = FALSE) +
  scale_color_manual(values = c("darkgrey", "#6572B8"), name = "Distances") +
  annotate(geom = "text", y = 0.95, x = 0, label = "C", size = 10)

# Bielefeld interdisc. distances
CDF4_Bielefeld <- ggplot(data = dat2, aes(xx)) + 
  labs(x = "Distance (in m)", y = "Probability") +
  ylim(0,1) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 17),
        axis.title.x = element_text(size = 17),
        plot.title = element_text(size = 22),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  stat_ecdf(aes(color = Interdisciplinary.distances), size = 2, geom = "step", pad = FALSE) +
  scale_color_manual(values = c("darkgrey", "#6572B8"), name = "Interdisciplinary \ndistances")  +
  annotate(geom = "text", y = 0.95, x = 0, label = "D", size = 10)


# Legend
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legPlot <- ggplot(data = dat, aes(xx)) +
  labs(x = "Distance (in m)", y = "Probability") +
  ylim(0,1) +
  theme_bw() +
  theme(plot.title = element_text(size = 22),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.key.width = unit(1.5, "cm"), 
        legend.justification = c(1,0), 
        legend.position = "bottom",  
        legend.background = element_blank(),
        legend.box.margin = margin(0.5,5, 0.5,-1, unit = "cm"),
        legend.key = element_blank()) + 
  stat_ecdf(aes(color = distances), size = 2, geom = "step", pad = FALSE) +
  scale_color_manual(values = c("darkgrey", "#6572B8"), name = "") 

leg <- get_legend(legPlot)


P11 <- ggplotGrob(CDF3_HMGU)
P22 <- ggplotGrob(CDF4_Bielefeld)
P12 <- ggplotGrob(CDF1_Bielefeld)
P21 <- ggplotGrob(CDF6_HMGU)

lay_m <- rbind(c(1, 2), c(3, 4), c(5, 5))
combineCDF_bottom <-  grid.arrange(arrangeGrob(P11, top = textGrob("        Helmholtz Zentrum München", gp = gpar(fontsize = 22, fontface = 'bold')), left = textGrob("Overall collaboration", gp = gpar(fontsize = 22, fontface = 'bold'), rot = 90)), 
                                   arrangeGrob(P12, top = textGrob("Bielefeld University", gp = gpar(fontsize = 22, fontface = 'bold'))), 
                                   arrangeGrob(P21, left = textGrob("Interdisciplinary collaboration", gp = gpar(fontsize = 22, fontface = 'bold'), rot = 90)), 
                                   P22, 
                                   leg,
                                   layout_matrix = lay_m, widths = c(2, 1.7), heights = c(2, 2, 0.4))

pdf("combineCDF_bottomLegend_bw.pdf", width = 11, height = 9)
grid.draw(combineCDF_bottom) 
dev.off()

