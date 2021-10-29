library(RISmed)
library(tidyverse)
library(stringdist)
library(stringr)
library(bibliometrix)

load("Workspaces/ClusterBielefeld.Rdata")

# dataset preparation for network analysis

# keep only Bielefeld Authors
namesBi <- names(maxInstBi)

# all possible institutes
uniInst <- unique(maxInstBi)


# PPA: Paper Per Author (only Bielefeld authors)
PPA_position <- PPA_ID <- list()
for (i in 1:length(namesBi)){ 
  if(length(which(authors == namesBi[i])) > 0){      
    vers <- authors_orig[which(authors == namesBi[i])] # use original name for searching in dataset     
    pos <- c()
    for(j in 1:length(vers)){
      pos <- c(pos, which(grepl(vers[j], data$AU, fixed = TRUE)))
    }
      PPA_position[[i]] <- pos
      PPA_ID[[i]] <- data$TI[PPA_position[[i]]]  
      names(PPA_ID)[i] <- names(PPA_position)[i] <- namesBi[i]
  }
  if( i %% 1000 == 0 ) cat(paste("iteration", i, "complete\n"))
}
PPA_ID <- PPA_ID %>% keep( ~ !is.null(.) )
PPA_position <- PPA_position %>% keep( ~ !is.null(.) )
PPA_ID <- PPA_ID[sort(names(PPA_ID))]

maxInst <- maxInst[names(maxInst) %in% namesBi]

save.image("Workspaces/DataPreparation2_Bielefeld_zwischen.Rdata")

#--------------------------------

# APP: Author per Paper
allTitles <- sort(data$TI)
allTitlesBi <- unique(unlist(PPA_ID))
APP <- list()
for (i in 1:length(allTitlesBi)){
  APP[[i]] <- names(PPA_ID[str_detect(PPA_ID, allTitlesBi[i])])
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}
names(APP) <- allTitlesBi[1:length(APP)]

# keep only Bielefeld data
dataBi <- data[data$TI %in% allTitlesBi,]
resultsBi <- biblioAnalysis(dataBi)


# IPP: Institute Per Paper
IPP <- APP
for (i in 1:length(APP)){
  IPP[[i]] <- names(topics[as.integer(maxInst[unlist(strsplit(APP[[i]], "\""))])])
}
names(IPP) <- allTitlesBi[1:length(IPP)]

# Paper per Institute
allInst <- unique(names(topics[sort(unique(as.integer(maxInst[namesBi])))]))

PPI <- list()
for (i in 1:length(allInst)){
  allPap <- c()
  for (j in 1:length(IPP)){
    if(allInst[i] %in% IPP[[j]] == TRUE){
      allPap <- c(allPap, j)
    }
    PPI[[i]] <- names(IPP)[allPap]
  }
}
names(PPI) <- allInst

# long format for ID, institute, year
long <- c()
for (i in 1:length(PPI)){
  id <- rep(NA, length(PPI[[i]]))
  year <- rep(NA, length(PPI[[i]]))
  for (j in 1:length(PPI[[i]])){
    id[j] <- PPI[[i]][j]
    year[j] <- na.omit(unique(data$PY[data$TI == id[j]]))
  } 
  long <- as.data.frame(rbind(long,cbind(id = id, dep = rep(names(PPI)[i], length(PPI[[i]])), year = year)))
}
long$id <- as.character(long$id)
long$dep <- as.character(long$dep)
long$year <- as.character(long$year)

# ID-List with information about list of authors, number of authors, institute, year
AllPapers <- list()
for(i in 1:length(allTitles)){
  vec <- c()
  for(j in 1:length(PPA_ID)){
    if (allTitles[i] %in% PPA_ID[[j]])
      vec <- c(vec, names(PPA_ID)[j])
  }
  AllPapers[[i]] <-  list("Authors" = vec, 
                          "NrAuthors" = length(vec), 
                          "Department" = long$dep[long$id == allTitles[i]], 
                          "Year" = unique(long$year[long$id == allTitles[i]]))
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}

names(AllPapers) <- allTitles
for (i in 1:length(AllPapers)){
  if (length(AllPapers[[i]]$Department)==0) 
    AllPapers[[i]]$Department <- NA
  if (length(AllPapers[[i]]$Year)==0) 
    AllPapers[[i]]$Year <- NA
}


#----------------------------------------------------
library(rlist)

# Creating an Edge List (relationship between two authors)
# authors without collaborators are not considered
out <- c(NA, NA, NA, NA)
for (i in 1:(length(PPA_ID)-1)){ 
  for (j in (i+1):length(PPA_ID)){
    if(sum((PPA_ID[[i]] %in% PPA_ID[[j]])) > 0)
    {out <- rbind(out, c(names(PPA_ID)[i], 
                         names(PPA_ID)[j], 
                         sum((PPA_ID[[i]] %in% PPA_ID[[j]])), 
                         paste(PPA_ID[[i]][PPA_ID[[i]] %in% PPA_ID[[j]]], collapse = ";")))}
  }
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}

# Adjust data frame
out <- out[-1,]
out <- as.data.frame(out)
colnames(out) <- c("Author", "Collaborator", "NrCommonPapers", "PaperTitle")
str(out)
out$NrCommonPapers <- as.numeric(as.character(out$NrCommonPapers))

save.image("Workspaces/DataPreparation2_Bielefeld.Rdata")


# Creating an Edge List (relationship between two institutes)
outInst <- c(NA, NA, NA)
for (i in 1:(length(PPI)-1)){ 
  for (j in (i+1):length(PPI)){
    if(sum((PPI[[i]] %in% PPI[[j]]))>0)
    {outInst <- rbind(outInst, c(names(PPI)[i], names(PPI)[j], sum((PPI[[i]] %in% PPI[[j]]))))}
  }
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}
outInst <- outInst[-1,]
outInst <- as.data.frame(outInst)
colnames(outInst) <- c("Institute", "CoInstitute", "NrCommonPapers")
str(outInst)
outInst$NrCommonPapers <- as.numeric(as.character(outInst$NrCommonPapers))


# Andere Idee: Liste mit allen Infos, aber jede Autoren-Kombi ist ein eigenes Element
out2 <- list()
for (i in 1:(length(PPA_ID)-1)){
  for (j in (i+1):length(PPA_ID)){
    if(sum((PPA_ID[[i]] %in% PPA_ID[[j]]))>0)
    {out2[[(length(out2)+1)]] <- list("Colab1" = names(PPA_ID)[i], "Colab2" = names(PPA_ID)[j], 
                                      "NrPapers" = sum((PPA_ID[[i]] %in% PPA_ID[[j]])),
                                      "WhichID" = PPA_ID[[i]][PPA_ID[[i]] %in% PPA_ID[[j]]], 
                                      "NrColab" = unlist(list.select(AllPapers[PPA_ID[[i]][PPA_ID[[i]] %in% PPA_ID[[j]]]], NrAuthors)))}
  }
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}

# for Institutes
outInst2 <- list()
for (i in 1:(length(PPI)-1)){
  for (j in (i+1):length(PPI)){
    if(sum((PPI[[i]] %in% PPI[[j]]))>0)
    {outInst2[[(length(outInst2)+1)]] <- list("Inst1" =names(PPI)[i], "Inst2"= names(PPI)[j], 
                                              "NrPapers"=sum((PPI[[i]] %in% PPI[[j]])),
                                              "WhichID"=PPI[[i]][PPI[[i]] %in% PPI[[j]]], 
                                              "NrColab"=unlist(list.select(AllPapers[PPI[[i]][PPI[[i]] %in% PPI[[j]]]], NrAuthors)))}
  }
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}

### calculating co-authorship index

w <- rep(NA, length(out2))
for (i in 1:length(out2)){
  w[i] <- out2[[i]]$NrPapers/sum(out2[[i]]$NrColab-1)
}

wInst <- rep(NA, length(outInst2))
for (i in 1:length(outInst2)){
  wInst[i] <- outInst2[[i]]$NrPapers/sum(outInst2[[i]]$NrColab-1)
}

# add to out dataset
out <- cbind(out, weight=w)
out$Author <- as.character(out$Author)
out$Collaborator <- as.character(out$Collaborator)
out$NrCommonPapers <-  as.integer(out$NrCommonPapers)
str(out)

outInst <- cbind(outInst, weight=wInst)
outInst$Institute <- as.character(outInst$Institute)
outInst$CoInstitute <- as.character(outInst$CoInstitute)
outInst$NrCommonPapers <-  as.integer(outInst$NrCommonPapers)
str(outInst)

# Bielefeld authors who actually have a publication
namesBi <- names(PPA_ID)

# topics for all Bielefeld authors
topicsBi <- names(topics[as.integer(maxInst[namesBi])])
names(topicsBi) <- names(maxInst[namesBi])
unique(topicsBi)
table(topicsBi)

# authors
for (i in 1:nrow(out)){
  if(is.na(topicsBi[out$Author[i]])| is.na(topicsBi[out$Collaborator[i]])){
    out$dist[i] <- NA
  } else {
    out$dist[i] <- FacDistMe[topicsBi[out$Author[i]], topicsBi[out$Collaborator[i]]]
  }
}

# institutes
for (i in 1:nrow(outInst)){
  a <- outInst$Institute[i]
  b <- outInst$CoInstitute[i]
  outInst$dist[i] <- FacDistMe[a, b]
}

# how many authors per institute?
table(names(topics[as.integer(maxInstBi)]))


# Locations

for (i in 1:nrow(out)){
  locAut <- unique(c(FacKnoten[FacKnoten$Fakultat == names(topics[as.integer(maxInstBi[[out$Author[i]]])]), "Location"])$Location)
  locAut <- unlist(strsplit(locAut, ", "))
  locCol <- unique(c(FacKnoten[FacKnoten$Fakultat == names(topics[as.integer(maxInstBi[[out$Collaborator[i]]])]), "Location"])$Location)
  locCol <- unlist(strsplit(locCol, ", "))
  
  if (length(locAut) < 1 || length(locCol) < 1 || is.na(locAut) || is.na(locCol)){
    out$location[i] <-  NA
  }
  # if one institute has more than one location, stay in the same location(out/ma)
  else if (length(locAut) > 1 | length(locCol) > 1){
    dists <- c()
    for (a in locAut) {
      for (b in locCol) {
        if (a == b)
        out$location[i] <-  paste0(a, "-", b)
      }
    }
    
  }
  else {
    out$location[i] <-
      paste(paste0(unique(c(FacKnoten[FacKnoten$Fakultat == names(topics[as.integer(maxInstBi[[out$Author[i]]])]), "Location"])$Location), collapse = ""),
            paste0(unique(c(FacKnoten[FacKnoten$Fakultat == names(topics[as.integer(maxInstBi[[out$Collaborator[i]]])]), "Location"])$Location), collapse = ""), 
            sep = "-")
  }
}

locations <- unique(out$location)
sort(table(out$location))

# add institutes for author pairs
for (i in 1:nrow(out)){
  out$instAut[i] <- maxInstBi[[out$Author[i]]]
  out$instCol[i] <- maxInstBi[[out$Collaborator[i]]]
}



# assign a category to each institute
categories <- c("Applied Sciences" = "Natural Sciences", 
               "Biology" = "Natural Sciences", 
               "CeBiTec" = "Natural Sciences",
               "Chemistry" = "Natural Sciences",                                                       
               "CITEC" = "Natural Sciences",                                                          
               "Educational Science" = "Social Sciences",                                              
               "Research Centre Mathematical Modelling" = "Natural Sciences",                           
               "History and Philosophy and Theology" = "Humanities",                              
               "Public Health"  = "Social Sciences",                                                   
               "Institute for Interdisciplinary Research on Conflict and Violence"  = "Social Sciences", 
               "Center for Mathematical Economics" = "Natural Sciences",                                  
               "Interdisciplinary Center for Gender Research"  = "Social Sciences",                     
               "Linguistics and Literary Studies" = "Humanities",                                 
               "Mathematics" = "Natural Sciences",                                                        
               "Mixed" = "Mixed",                                                            
               "Physics" = "Natural Sciences",                                                            
               "Psychology and Sports Science" = "Humanities",                                   
               "Law" = "Social Sciences",                                                              
               "Research Institute for Cognition and Robotics" = "Natural Sciences",                     
               "SFB 8822" = "Social Sciences",                                              
               "Sociology"  = "Social Sciences",                                                       
               "Technology" = "Natural Sciences",                                                         
               "Business Administration and Economics"  = "Social Sciences",                           
               "Center for Interdisciplinary Research"  = "Social Sciences" 
               )
topics[names(categories)]
out$catAut <- categories[names(topics[as.integer(out$instAut)])]
out$catCol <- categories[names(topics[as.integer(out$instCol)])]

#--- Data Summary
DataSummary <- list(
  "nrPapersALL" = length(AllPapers),
  "nrPapersBi" = length(allTitlesBi),
  "nrAuthors_BIpub" = length(resultsBi$Authors),
  "nrAuthors_BIpub_affil" = length(namesBi), 
  "nrInstitutes" = length(uniInst)) 


save.image("Workspaces/DataPreparation2_Bielefeld.Rdata")
