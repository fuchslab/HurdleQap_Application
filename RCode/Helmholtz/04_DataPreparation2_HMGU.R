library(RISmed)
library(tidyverse)
library(stringdist)
library(stringr)


load("Workspaces/ClusterHMGU.Rdata")

# dataset preparation for network analysis

# keep only HMGU Authors
namesHMGU <- names(maxInst[which(maxInst != "notHMGU")])
table(unlist(map(PapersPerAu[namesHMGU],~length(.x))))

# all possible institutes
uniInst <- unique(maxInstHelm)

# PPA: Paper Per Author (only Helmholtz authors)
PPA_position <- PPA_ID <- list()
for (i in 1:length(namesHMGU)){ 
  if(length(which(authors == namesHMGU[i])) > 0){      
    vers <- authors_orig[which(authors == namesHMGU[i])] # use original name for searching in dataset     
    pos <- c()
    for(j in 1:length(vers)){
      pos <- c(pos, which(grepl(vers[j], data$AU, fixed = TRUE)))
    }
    PPA_position[[i]] <- pos
    PPA_ID[[i]] <- data$TI[PPA_position[[i]]]  
    names(PPA_ID)[i] <- names(PPA_position)[i] <- namesHMGU[i]
  }
  if( i %% 1000 == 0 ) cat(paste("iteration", i, "complete\n"))
}
#names(PPA_ID) <- names(PPA_position) <- namesHMGU
PPA_ID <- PPA_ID %>% keep( ~ !is.null(.) )
PPA_position <- PPA_position %>% keep( ~ !is.null(.) )
PPA_ID <- PPA_ID[sort(names(PPA_ID))]

maxInst <- maxInst[names(maxInst) %in% namesHMGU]

save.image("Workspaces/DataPreparation2_HMGU_zwischen.Rdata")
#------------------------------------------------------------------------------------------

# APP: Author per Paper
allTitles <- sort(data$TI)
allTitlesHMGU <- unique(unlist(PPA_ID))
APP <- list()
for (i in 1:length(allTitlesHMGU)){
  APP[[i]] <- names(PPA_ID[str_detect(PPA_ID, allTitlesHMGU[i])])
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}
names(APP) <- allTitlesHMGU[1:length(APP)]

# keep only HMGU data
dataHMGU <- data[data$TI %in% allTitlesHMGU,]
resultsHMGU <- biblioAnalysis(dataHMGU)
  
save.image("Workspaces/DataPreparation2_HMGU_zwischen.Rdata")

# IPP: Institute Per Paper
IPP <- APP
for (i in 1:length(APP)){
  IPP[[i]] <- maxInst[unlist(strsplit(APP[[i]], "\""))]
}
names(IPP) <- allTitlesHMGU[1:length(IPP)]


# assign a category to each institute
allCat <- GebKnoten$Category
names(allCat) <- GebKnoten$Abbreviation
allCat <- allCat[duplicated(names(allCat)) == FALSE]
allCat <- c(allCat, Mixed = "Other")


# CPP: Category per paper
CPP <- APP
for (i in 1:length(APP)){
  CPP[[i]] <- unname(allCat[as.character(IPP[[i]])])
}
names(CPP) <- allTitlesHMGU[1:length(CPP)]


# Paper per Institute
allInst <- uniInst # only institutes which we kept in our data

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

# paper per category
PPC <- list()
for (i in 1:length(unique(allCat))){
  allPap <- c()
  for (j in 1:length(CPP)){
    if(allCat[i] %in% CPP[[j]] == TRUE){
      allPap <- c(allPap, j)
    }
    PPC[[i]] <- names(CPP)[allPap]
  }
}
names(PPC) <- unique(allCat)



#-------------------------------------------------------------------------
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
long$category <- as.character(allCat[long$dep])
long$year <- as.character(long$year)

# ID-List with information about list of authors, number of authors, institute, category, year
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
                          "Category" = long$category[long$id == allTitles[i]],
                          "Year" = unique(long$year[long$id == allTitles[i]]))
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}

names(AllPapers) <- allTitles
for (i in 1:length(AllPapers)){
  if (length(AllPapers[[i]]$Department)==0) 
    AllPapers[[i]]$Department <- NA
  if (length(AllPapers[[i]]$Category)==0) 
    AllPapers[[i]]$Category <- NA
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

save.image("Workspaces/DataPreparation2_Helmholtz.Rdata")

# Creating an Edge List (relationship between two institutes)
outInst <- c(NA, NA, NA, NA)
for (i in 1:(length(PPI)-1)){ 
  for (j in (i+1):length(PPI)){
    if(sum((PPI[[i]] %in% PPI[[j]]))>0)
    {outInst <- rbind(outInst, c(names(PPI)[i], 
                                 names(PPI)[j], 
                                 sum((PPI[[i]] %in% PPI[[j]])),
                                 paste(PPI[[i]][PPI[[i]] %in% PPI[[j]]], collapse = ";")))}
  }
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}
outInst <- outInst[-1,]
outInst <- as.data.frame(outInst)
colnames(outInst) <- c("Institute", "CoInstitute", "NrCommonPapers", "PaperTitle")
str(outInst)
outInst$NrCommonPapers <- as.numeric(as.character(outInst$NrCommonPapers))

# alternative approach: list with all info but each author pair is one observation
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
    {outInst2[[(length(outInst2)+1)]] <- list("Inst1" = names(PPI)[i], "Inst2"= names(PPI)[j], 
                                              "NrPapers"= sum((PPI[[i]] %in% PPI[[j]])),
                                              "WhichID"= PPI[[i]][PPI[[i]] %in% PPI[[j]]], 
                                              "NrColab"= unlist(list.select(AllPapers[PPI[[i]][PPI[[i]] %in% PPI[[j]]]], NrAuthors)))}
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

# Helmholtz authors who actually have a publication
namesHMGU <- names(PPA_ID)

# topics for all Helmholtz authors
topicsHMGU <- names(topics[maxInst[namesHMGU]])
names(topicsHMGU) <- names(maxInst[namesHMGU])
unique(topicsHMGU)
table(topicsHMGU)


# add distance to out data

# authors
for (i in 1:nrow(out)){
  out$dist[i] <- FacDistMe[topicsHMGU[out$Author[i]],topicsHMGU[out$Collaborator[i]]]
}

# institutes
for (i in 1:nrow(outInst)){
  a <- outInst$Institute[i]
  b <- outInst$CoInstitute[i]
  outInst$dist[i] <- FacDistMe[a, b]
}

# how many authors per institute?
table(names(topics[maxInstHelm]))

# categories
categories <- unique(allCat)



# Locations
GebKnoten$Location[GebKnoten$Abbreviation == "DES"] <- "Neuherberg"


# These institutes have different locations: CMA, IAF, IDO, IDR, VIRO

colnames(data2)[1] <- rownames(data2)[1] <- "Neuherberg"

for (i in 1:nrow(out)){
  locAut <- unique(c(GebKnoten[GebKnoten$Abbreviation == maxInstHelm[[out$Author[i]]], "Location"])$Location)
  locCol <- unique(c(GebKnoten[GebKnoten$Abbreviation == maxInstHelm[[out$Collaborator[i]]], "Location"])$Location)
  if (length(locAut) < 1 | length(locCol) < 1){
    out$location[i] <-  NA
  }
  # if one institute has more than one location, take the minimum distance location
  else if (length(locAut) > 1 | length(locCol) > 1){
    dists <- c()
    for (a in locAut) {
      for (b in locCol) {
        ab <- data2[a, b]
        names(ab) <- paste0(a, "-", b)
        dists <- c(dists, ab)
      }
    }
    out$location[i] <-  names(which.min(dists[is.finite(dists)]))
  }
  else {
  out$location[i] <-
    paste(paste0(unique(c(GebKnoten[GebKnoten$Abbreviation == maxInstHelm[[out$Author[i]]], "Location"])$Location), collapse = ""),
          paste0(unique(c(GebKnoten[GebKnoten$Abbreviation == maxInstHelm[[out$Collaborator[i]]], "Location"])$Location), collapse = ""), 
          sep = "-")
  }
}

locations <- unique(out$location)
sort(table(out$location))

# add institutes for author pairs
for (i in 1:nrow(out)){
  out$instAut[i] <- maxInstHelm[[out$Author[i]]]
  out$instCol[i] <- maxInstHelm[[out$Collaborator[i]]]
}

#--- Data Summary
DataSummary <- list(
  "nrPapersALL" = length(AllPapers),
  "nrPapersHMGU" = length(allTitlesHMGU),
  "nrAuthors_HMGUpub" = length(resultsHMGU$Authors),
  "nrAuthors_HMGUpub_affil" = length(namesHMGU),
  "nrInstitutes" = length(uniInst), 
  "nrCategories" = length(categories)) 


save.image("Workspaces/DataPreparation2_HMGU.Rdata")
