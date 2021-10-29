# prepare dataset for institute affiliations

library(RISmed)
library(tidyverse)
library(stringdist)
library(stringr)
library(readxl)

# Years 2015 - 2019
load("Workspaces/Distances_Helmholtz.Rdata")

# all affiliations for one author in one vector
AffilPerAu <- list()
for(i in 1:length(Affil)){
  AffilPerAu[[i]] <- unname(unlist(Affil[[i]]))
}
names(AffilPerAu) <- names(Affil)
AffilPerAu <- AffilPerAu[sort(names(AffilPerAu))]

# synonyms for HMGU
synoms <-
  c("DEUTSCH FORSCH ZENTRUM GESUNDHEIT & UMWELT GMBH, HELMHOLTZ ZENTRUM MUNCHEN",
    "DEUTSCH FORSCHUNGSZENTRUM GESUNDHEIT & UMWELT GMB, HELMHOLTZ ZENTRUM MUNCHEN",
    "DEUTSCH FORSCHUNGSZENTRUM GESUNDHEIT & UMWELT, HELMHOLTZ ZENTRUM MUNCHEN",
    "DEUTSCH FORSCHUNGSZENTRUM GESUNDHEIT & UMWELT",
    "DEUTSCHES FORSCHUNGSZENTRUM GESUNDHEIT UMWEL",
    "DEUTSCH FORSCHUNGSZENTRUM GESUNDHEIT UND UMWELT",
    "DEUTSCHES FORSCHUNGSZENTRUM FR GESUNDHEIT UND UMWELT GMBH",
    "DEUTSCH FORSCH ZENTRUM GESUNDHEIT UMWELTS GMBH", 
    "FORSCHUNGSZENTRUM GESUNDHEIT UMWELT GMBH",
    "DEUTSCH FORSCH ZENTRUM GESUNDHEIT UMWELTS",
    "DEUTSCHES FORSCHUNGSZENTRUM GESUNDHEIT UND UMWELT",
    "DEUTSCHES FORSCHUNGSZENTRUM GESUNDHEIT UMWELT",
    "DEUTSCH FORSCH ZENTRUM GESUNDHEIT UMWELT",
    "DEUTSCHES FORSCHUNGSZENTRUM UMWELT UND GESUNDHEIT",
    "DEUTSCHES FORSCHUNGSZENTRUM GESUNDHEIT UND UMWELT",
    "DEUTSCH FORSCH ZENTRUM GESUNDHEIT UMWELT GMB",
    "DEUTSCH FORSCHUNGS ZENTRUM GESUNDHEIT UMWELT",
    "DEUTSCH FORSCHUNGSZENTRUM GESUNDHEIT UMWEL",
    "FORSCHUNGSZENTRUM GESUNDHEIT UMWELT  DEUTSCH",
    "DEUTSCHES FORSCHUNGSZENTRUM GESUNDHEIT UND UMWELT",

    "GERMAN CTR DIABET RES DZD, GERMAN RES CTR ENVIRONM HLTH, HELMHOLTZ ZENTRUM MUNCHEN",
    "GERMAN CTR DIABET RES DZD, PARTNER SITE HELMHOLTZ ZENTRUM",
    "GERMAN RES CTR ENVIRONM HLTH, HELMHOLTZ ZENTRUM MUNCHEN GMBH",
    "GERMAN CTR ENVIRONM HLTH, HELMHOLTZ ZENTRUM MUNCHEN",
    "GERMAN RES CTR ENVIRONM HLTH GMBH, HELMHOLTZ ZENTRUM MUNCHEN",
    "GERMAN RES CTR ENVIRONM HLTH, HELMHOLTZ ZENTRUM MUNCHEN", 
    "GERMAN RES CTR ENVIRONM HLTH, HELMHOLTZ ZENTRUM MUENCHEN",
    "GERMAN RES CTR ENVIRONM HLTH, HELMHOLTZ CTR MUNICH",
    "GERMAN CTR DIABET RES, HELMHOLTZ CTR MUNICH",
    "GERMAN RESEARCH CENTER FOR ENVIRONMENTAL HEALTH",
    "GERMAN RESEARCH CENTRE FOR ENVIRONMENTAL HEALTH",
    "GERMAN RESEARCH CTR ENVIRONM HLTH",
    "GERMAN CTR HLTH ENVIRONM",
    "GERMAN RES CTR ENVIROMENTAL HLTH",
    "RES CTR ENVIRONM HLTH",
    
    "HELMHOLTZ ZENTRUM MUNCHEN, HMGU, GERMAN CTR ENVIRONM HLTH",
    "HELMHOLTZ ZENTRUM MUNCHEN, GERMAN RES CTR ENVIRONM HLTH GMBH",
    "HELMHOLTZ ZENTRUM MUNCHEN, GERMAN RES CTR ENVIRONM HLTH",    
    "HELMHOLTZ ZENTRUM MUNCHEN, GERMAN RES CTR ENVIRONM",       
    "HELMHOLTZ ZENTRUM MUNCHEN, GERMAN CTR ENVIRONM HLTH",
    "HELMHOLTZ ZENTRUM MUNCHEN GMBH, GERMAN RES CTR ENVIRONM HLTH",
    "HELMHOLTZ ZENTRUM MUNCHEN GERMAN RES CTR ENVIRONM",
    "HELMHOLTZ ZENTRUM MUNCHEN GERMAN RES CTR ENVIRON",
    "HELMHOLTZ ZENTRUM MUNCHEN ERMAN RES CTR ENVIRONM",
    "HELMHOLTZ ZENTRUM MUNCHEN DEUTSCH FORSCHUNGSZENTR",
    "HELMHOLTZ ZENTRUM MUNCHEN, DEUTSCH FORSCHUNGSZENTRUM GESUNDHEIT & UMWELT GMB",
    "HELMHOLTZ ZENTRUM MUNCHEN, DEUTSCH FORSCH ZENTRUM GESUNDHEIT & UMWELT",    
    "HELMHOLTZ ZENTRUM MUNCHEN, DEUTSCH FORSCHUNGSZENTRUM UMWELT & GESUNDHEIT",
    "HELMHOLTZ ZENTRUM MUNCHEN, DEUTSCH FORSCHUNGSZENTRUM GESUNDHEIT",
    "HELMHOLTZ CTR MUNICH, GERMAN RES CTR ENVIRONM & HLTH",
    "HELMHOLTZ CTR MUNICH, GERMAN RES CTR ENVIRONM HLTH",
    
    "GERMAN RES CTR ENVIRONM HLTH GMBH",
    "GERMAN RES CTR ENVIRIONM HLTH GMBH",
    "GERMAN RES CTR ENVIRONM HLTH",
    "GERMAN RES CTR ENVIRON",
    "GERMAN CTR DIABET RES DZD EV",
    "GERMAN CTR DIABET RES DZD",
    "GERMAN CTR DIABET RES",
    "HMGU HELMHOLTZ ZENTRUM MUNCHEN",
    "HELMHOLTZ ZENTRUM MUNCHEN GMBH",
    "HELMHOLTZ ZENTRUM MUNCHEN",
    "HELMHOLTZ ZENTRUM MUENCHEN",
    "HELMHOLTZ ZENTRUM MUNICH",
    "HELMHOLTZ CTR MUNICH",
    "HELHMOLHTZ ZENTRUM MUNCHEN",
    "HELMHOLEZ ZENRRUM MUNCHEN",
    "HELMHOLZ ZENTRUM MUNCHEN",
    "HELMHOLTZ ZENTRUM M NCHEN",
    "HELMHOLTZ ZENTRUM MUCHEN",
    "HELMHOLTZ ZENTRUM MNCHEN",
    "HELMHOLTZ CTR MUENCHEN",
    "HELMHOLTZ ZENTRUM MUENCHEN",
    "HELMHOLTZ ZENTRUM MUENCHEN",
    "HELMHOLTZ ZENTRUM  MUNICH",
    "HELMHOLTZ ZENTRUMMUENCHEN",
    "HELMHOLTZ ZENTRUM MUUNCHEN",
    "HELMHOLTZ ZENTRUM MUNCEN",
    "HELMHOLTZ ZENTRUM MUNCHYEN",
    "HELMHOLTZ ZENTRUM MNCHEN",
    "HELMHOLTZ ZENT MUENCHEN", 
    "HELMHOLTZ CENTRE MUNICH",
    "HELMHOLTZ CENTER MUNICH",
    "HELMHOLTZ ZENTRUMS MUENCHEN",
    "HELMHOLTZ ZENTRURN MUENCHEN"
  )

# remove or change some terms
for(i in 1:length(AffilPerAu)){ 
  x <- AffilPerAu[[i]]
  
  x <- str_to_upper(x)
  
  for (j in 1:length(synoms)){
    x <- str_replace(x, synoms[j], "HMGU")
  }
  
  x <- gsub("[[:punct:]]" , " ", x)
  x <- gsub("[[:space:]]+" , " ", x)  
  x <- gsub("Ü", "UE", x)  
  x <- gsub("Ä", "AE", x)  
  x <- gsub("Ö", "OE", x)
  x <- gsub("ß", "SS", x)
  
  x <- str_replace(x, "HELMHOLTZ ZENNTRUM", "HELMHOLTZ ZENTRUM")
  x <- str_replace(x, "HELMHOLTZZENTRUM", "HELMHOLTZ ZENTRUM")
  x <- str_replace(x, "HELMHOTZ ZENTRUM", "HELMHOLTZ ZENTRUM")
  x <- str_replace(x, "HELMHOLTZ ZENTRTUN", "HELMHOLTZ ZENTRUM")

  x <- str_replace(x, "NEUHERBERG", "")
  x <- str_replace(x, "NEUHERBER", "")
  x <- str_replace(x, "NEUNERBER", "")
  x <- str_replace(x, "NEUHEIBER", "")
  x <- str_replace(x, "NEUHERERG", "")
  x <- str_replace(x, "NAUHERBER", "")
  
  
  x <- str_replace(x, "INGOLSTADTER", "")
  x <- str_replace(x, "INGOLDSTADTER", "")
  x <- str_replace(x, "INGOLSTAEDTER", "")
  x <- str_replace(x, "INGOLSTADTER", "")
  x <- str_replace(x, "INGOSTADTER", "")
  x <- str_replace(x, "INGOLSTODTER", "")
  x <- str_replace(x, "INGOLSTDTER", "")
  x <- str_replace(x, "INGOLSTAEDTLER", "")
  x <- str_replace(x, "INGOLDSTAOTER", "")
  x <- str_replace(x, "INGOLSTAEDTER", "")
  x <- str_replace(x, "INGOISTADTER", "")

  x <- str_replace(x, "LANDSTR 1", "")
  x <- str_replace(x, "LANDTRASSE 1", "")
  x <- str_replace(x, "LANDSTRAE 1", "")
  x <- str_replace(x, "LANDSTRASSE 1", "")
  x <- str_replace(x, "LANSTR 1", "")
  x <- str_replace(x, "LANDSTRAE1", "")
  
  x <- str_replace(x, "INGOLSTADTERLANDSTR 1", "")
  
  x <- gsub("GERMANY",  "",x)
  x <- str_replace(x, "BAVARIA", "")
  x <- str_replace(x, "BAYERN", "")
  x <- str_replace(x, "85764", "")
  x <- str_replace(x, "85746", "")
  x <- str_replace(x, "85674", "")
  x <- str_replace(x, "87564", "")
  x <- str_replace(x, "81377", "")
  x <- str_replace(x, "80802", "")
  x <- str_replace(x, "PARTNER", "")
  x <- str_replace(x, "OBERSCHLEISSHEIM", "")
  x <- str_replace(x, "OBERSCHLEIHEIM", "")
  x <- str_replace(x, "OBERSCHLEISSHEI", "")
  x <- str_replace(x, "MUNCHEN", "MUENCHEN")
  x <- str_replace(x, "HELMHOLTZ ZENTRURN", "HELMHOLTZ ZENTRUM")
  x <- gsub("MUNICH", " ", x)
  x <- gsub("MUENCHEN", " ", x)
  x <- gsub("GERMAN", " ", x)
  x <- gsub("GMBH", " ", x)
  
  x <- gsub("[[:punct:]]" , " ",x)
  x <- gsub("[[:space:]]+" , " ",x)
  x <- trimws(x)  
  x <- str_replace(x, "HMGU[:space:]HMGU", "HMGU")

  x <- trimws(x)  

  x <- gsub("[[:space:]]+" , " ", x)
  x <- trimws(x) 
  
  AffilPerAu[[i]] <- x
  if( i %% 1000 == 0 ) cat(paste("iteration", i, "complete\n"))
}

# All possible unique institutes/Faculties
allAff <- unique(unname(unlist(AffilPerAu)))
allAff[-grep("HMGU", allAff)] <- "notHMGU"
length(unique(allAff))
# keep just HMGU affiliations
allAffHelm <- allAff[which(allAff != "notHMGU")]

# adjust affiliations
allAffHelm2 <- gsub("HMGU", "", allAffHelm) 
allAffHelm2 <- trimws(allAffHelm2) 

names(allAffHelm) <- names(allAffHelm2) <- paste0("Aff", 1:length(allAffHelm))

# remove terms like department, faculty etc
x <- allAffHelm2
x <- str_replace(x, "DEPARTMENT", "")
x <- str_replace(x, "DEPT", "")
x <- str_replace(x, "OF ", "")
x <- str_replace(x, "FUER ", "")
x <- str_replace(x, "FUR ", "")
x <- str_replace(x, "FOR ", "")
x <- str_replace(x, "FR ", "")

x <- str_replace(x, "INSTITUTE", "")
x <- str_replace(x, "INSTITUT", "")
x <- str_replace(x, "INST", "")
x <- str_replace(x, "FACULTY", "")
x <- str_replace(x, "FAKULTAET", "")
x <- str_replace(x, "FAK", "")
x <- str_replace(x, "FAC", "")
x <- str_replace(x, "MNCHEN", "")
x <- str_replace(x, "UNIT", "")

x <- str_replace(x, "RESEARCH", "")
x <- str_replace(x, "RES ", "")
x <- str_replace(x, "GROUP", "")
x <- str_replace(x, "GRP", "")
x <- str_replace(x, "HELMHOLTZ ZENTRUM MNCHEN", "")
x <- str_replace(x, "HELMHOLTZ ZENTRUM", "")
x <- str_replace(x, "DEUTSCHES FORSCHUNGSZENTRUM GESUNDHEIT UND UMWELT", "")

x <- str_replace(x, "[[:space:]]+" , "")
x <- trimws(x) 
names(x) <- paste0("Aff", 1:length(allAffHelm))
allAffHelm2 <- x 


#-------------------------------------------------------------------------------------------------------------

# create affiliation data frame
InstData <- data.frame("id" = names(allAffHelm), "review" = allAffHelm)
InstDataHelm <- data.frame("id" = names(allAffHelm2), "review" = allAffHelm2)
InstDataHelm$review <- as.character(InstDataHelm$review)

# remove affiliations which are too short to have input
InstDataHelm[which(nchar(InstDataHelm$review) < 3),]
InstDataHelm <- InstDataHelm[which(nchar(InstDataHelm$review) >= 3),]

InstData$id <- as.character(InstData$id)
InstDataHelm$id <- as.character(InstDataHelm$id)

# which ids were deleted in InstDataHelm?
InstData$id[-which(InstData$id %in% InstDataHelm$id)]

save.image("Workspaces/ClusterHMGU_zwischen.Rdata")

#------------------------------------------------------------------------------------------------------------
# get institute key words
GebKnoten <- read_excel("Daten/Helmholtz/Zuordnung_Institute_Gebaeude_reduziert.xlsx", na = "NA")
GebKnoten$ID <- paste0("ID", 1:nrow(GebKnoten))
GebKnoten$keywords <- trimws(GebKnoten$keywords) 

InstDataHelm$Inst <- NA

# reorder institutes order to avoid overwriting (e.g. Diabetes)
smallGebKnoten <- GebKnoten[which(!is.na(GebKnoten$keywords)), ]
smallGebKnoten$Abbreviation
diabetes <- c("HDC", "IDF", "IDC", "IDO", "IDO-NBD", "IDR", "IDM", "DZD")
lung <- c("CPC", "ILBD")
epi <- c("EPI", "EPI-AME", "KEPI")
order <- c(which(smallGebKnoten$Abbreviation %in% diabetes), 
           which(smallGebKnoten$Abbreviation %in% lung),
           which(smallGebKnoten$Abbreviation %in% epi),
           which(!smallGebKnoten$Abbreviation %in% c(diabetes, lung, epi, "IRT")),
           which(smallGebKnoten$Abbreviation == "IRT"))

smallGebKnoten <- smallGebKnoten[order,]

# set institutes according to keywords
for (k in 1:nrow(smallGebKnoten)){
    keys <- unlist(strsplit(smallGebKnoten$keywords[k], ", "))
    Pattern <- paste(keys, collapse="|")
    for (i in 1:nrow(InstDataHelm)){
      if (grepl(Pattern, InstDataHelm$review[i]))
        InstDataHelm$Inst[i] <- smallGebKnoten$Abbreviation[k]
    }
}

InstDataHelm$Inst[is.na(InstDataHelm$Inst)] <- "Mixed"

Mixed <- InstDataHelm[InstDataHelm$Inst == "Mixed", ]
Mixed <- cbind(Mixed, InstData[InstData$id %in% Mixed$id, "review"])

#----------

# data which was removed at beginning
InstData$id[-which(InstData$id %in% InstDataHelm$id)]
InstDataHelm <- rbind(InstDataHelm,  cbind(InstData[-which(InstData$id %in% InstDataHelm$id),], Inst = "Mixed"))

# change cluster number so that we have only one number per Affiliation
topics <- setNames(c(1:length(sort(unique(InstDataHelm$Inst)))), sort(unique(InstDataHelm$Inst)))

#-----------------------------------------
# rematch for each author  
ClusterPerAu <- AffilPerAu

for (i in 1:length(ClusterPerAu)){
  aut <- ClusterPerAu[[i]]
  if (length(aut) > 0){
    for(j in 1:length(aut)){
      if (aut[j] %in% InstData$review){
        ClusterPerAu[[i]][j] <- InstDataHelm[InstDataHelm$id == InstData$id[InstData$review == aut[j]], "Inst"]
      }
    }
  }
}

# maximum of all affiliations per author 
numMix <- "Mixed" # "Mixed" Cluster

maxInst <- c()
for (i in 1:length(AffilPerAu)) {
  if (length(unlist(AffilPerAu[i])) < 1) {
    maxInst[i] <- NA
  }
  else if (all(is.na(unlist(AffilPerAu[i])))){
    maxInst[i] <- NA
  }
  else{
    # check if HMGU is included
    if (length(grep("HMGU", AffilPerAu[[i]])) > 0) {
      welche <- grep("HMGU", AffilPerAu[[i]])
      # take maximum affiliation of all publication affiliations (not only HMGU)
      tab <- sort(table(unlist(ClusterPerAu[[i]][welche])), decreasing = TRUE)
      # more than one HMGU aff. and first two have same quantity
      if(length(tab) > 1 & sum(tab[1]) == sum(tab[2])){
        if(length(tab[which(sapply(tab, length) == length(which.max(tab)) & names(tab) != numMix)]) <= 1){ # numMix for Mixed
          maxInst[i] <- names(tab[which(sapply(tab, length) == length(which.max(tab)) & names(tab) != numMix)])
        }
        else 
        {
          set.seed(123)
          maxInst[i] <- sample(names(tab[which(sapply(tab, sum) == max(unname(tab)) & names(tab) != numMix)]), 1)
        }
      }
      # if first is Mixed and second is another institute, take the second one
      else 
      {
        if (names(tab)[1] == numMix){
          if (length(tab) == 1) { maxInst[i] <- numMix
          } else {
            maxInst[i] <- names(tab)[2] 
          }
        }
        else{
          maxInst[i] <- names(tab)[1]
        }
      }
    }
    else
    {
      maxInst[i] <- "notHMGU" 
    }
  }
}
names(maxInst) <- names(AffilPerAu)


# some adjusting by hand
maxInst[nchar(maxInst) > 7] <- "notHMGU"
maxInst[which(maxInst == "UNIV")] <- "notHMGU"
maxInst[which(maxInst == "FAK MED")] <- "notHMGU"
maxInst[which(maxInst == "LMU")] <- "notHMGU"
maxInst[which(maxInst == "BASF SE")] <- "notHMGU"
maxInst[which(maxInst == "AND")] <- "notHMGU"
maxInst[which(maxInst == "AOK")] <- "notHMGU"
maxInst[which(maxInst == "MDK")] <- "notHMGU"
maxInst[which(maxInst == "R207")] <- "notHMGU"
maxInst[which(maxInst == "QUART")] <- "notHMGU"
maxInst[which(maxInst == "FAC MED")] <- "notHMGU"
maxInst[which(maxInst == "CIPSM D")] <- "notHMGU"
maxInst[which(maxInst == "BIR")] <- "notHMGU"
maxInst[which(maxInst == "TUM")] <- "notHMGU"
maxInst[which(maxInst == "IFT")] <- "notHMGU"
maxInst[which(maxInst == "IDG D")] <- "IDG"

length(unique(maxInst))

MixedAffil <- AffilPerAu[names(which(maxInst == "Mixed"))]
mixedList <- AffilPerAu[names(maxInst[maxInst == "Mixed"])]

# we state all authors which have only Mixed Helmholtz Affiliations as "notHMGU" 
maxInst[which(maxInst == numMix)] <- "notHMGU"
maxInst[which(is.na(maxInst))] <- "notHMGU"

maxInstHelm <- maxInst[which(maxInst != "notHMGU")]
length(unique(maxInstHelm))
sort(table(maxInst))

# test
maxInst["FUCHS C"]
AffilPerAu[["FUCHS C"]]
ClusterPerAu[["FUCHS C"]]

maxInst["MARR C"]
maxInst["HILGENDORFF A"]
maxInst["PETERS A"]
maxInst["ZIEGLER A"]
maxInst["THEIS F"]
AffilPerAu["THEIS F"]
ClusterPerAu["THEIS F"]

# add DES, HPC (and Mixed) to distance matrix
FacDistMe <- cbind(FacDistMe, rep(NA, times = nrow(FacDistMe)), rep(NA, times = nrow(FacDistMe)), rep(NA, times = nrow(FacDistMe)))
FacDistMe <- rbind(FacDistMe, rep(NA, times = ncol(FacDistMe)), rep(NA, times = ncol(FacDistMe)), rep(NA, times = ncol(FacDistMe)))
colnames(FacDistMe)[c(ncol(FacDistMe)-2, ncol(FacDistMe)-1, ncol(FacDistMe))] <- c("DES", "HPC", "Mixed")
rownames(FacDistMe)[c(nrow(FacDistMe)-2, nrow(FacDistMe)-1, nrow(FacDistMe))] <- c("DES", "HPC", "Mixed")

# which names of maxInst are not (yet) in the distance matrix?
unique(maxInst)[which(unique(maxInst) %in% colnames(FacDistMe) == FALSE)]


rm(x, welche, tab, Pattern, pap, order, lung, keys, k, i, j, aut)
save.image("Workspaces/ClusterHMGU.Rdata")
