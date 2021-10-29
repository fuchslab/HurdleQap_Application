# prepare dataset for institute affiliations

library(RISmed)
library(tidyverse)
library(stringdist)
library(stringr)
library(readxl)

# Years 2015 - 2019
load("Workspaces/Distances_Bielefeld.Rdata")

# all affiliations for one author in one vector
AffilPerAu <- list()
for(i in 1:length(Affil)){
  AffilPerAu[[i]] <- unname(unlist(Affil[[i]]))
}
names(AffilPerAu) <- names(Affil)
AffilPerAu <- AffilPerAu[sort(names(AffilPerAu))]


### data preparation

# synonyms for Bielefeld
synoms <- c("UNIV BIELEFELD", 
            "BIELEFELD UNIV",
            "BIELEFELD UNIVERSITY",
            "UNIVERSITÄT BIELEFELD",
            "BIELEFELD UNIVERSITÄT", 
            "UNIVERSITY OF BIELEFELD",
            "UNIVERSITY BIELEFELD", 
            "UNIVERSITAET BIELEFELD")

# remove or change some terms
for(i in 1:length(AffilPerAu)){ 
  x <- AffilPerAu[[i]]
  x <- str_to_upper(x)
  
  for (j in 1:length(synoms)){
    x <- str_replace(x, synoms[j], "BIELEFELD UNIV")
  }
  
  x <- gsub("[[:punct:]]" , " ", x)
  x <- gsub("[[:space:]]+" , " ", x)  
  x <- gsub("Ü", "UE", x)  
  x <- gsub("Ä", "AE", x)  
  x <- gsub("Ö", "OE", x)
  x <- gsub("ß", "SS", x)
  
  x <- gsub("POSTFACH 10 01 31 D 33501 BIELEFELD", " ", x)  
  x <- gsub("POST BOX 100 131 D 33501 BIELEFELD", " ", x)  
  x <- gsub("POSTFACH 1001 31 BIELEFELD", " ", x) 
  x <- gsub("BOX 100131", " ", x)
  x <- gsub("POB", " ", x)
  x <- gsub("POSTBOX", " ", x) 
  x <- gsub("P O BOX 100 131", " ", x)
  x <- gsub("P O BOX", " ", x) 
  x <- gsub("PO BOX", " ", x) 
  x <- gsub(" P O", " ", x) 
  x <- gsub(" PO ", " ", x) 
  x <- gsub("POST", " ", x)  
  x <- gsub("BOX", " ", x) 
  x <- gsub("10 01 31 D 33501 BIELEFELD", " ", x)  
  x <- gsub("10 01 31 BIELEFELD D", " ", x)
  x <- gsub("100131 D 33501 BIELEFELD", " ", x)  
  x <- gsub("100 131 D 33501 BIELEFELD", " ", x) 
  x <- gsub("100131 BIELEFELD D", " ", x) 
  x <- gsub("100131", " ", x) 
  x <- gsub("10 01 31", " ", x) 
  x <- gsub("10 0131", " ", x) 
  x <- gsub("10 10 31", " ", x)
  x <- gsub("100 131", " ", x)
  x <- gsub("D 33501 BIELEFELD", " ", x) 
  x <- gsub("D 33615 BIELEFELD", " ", x) 
  x <- gsub("D 33619 BIELEFELD", " ", x)
  x <- gsub("D 33609 BIELEFELD", " ", x)
  x <- gsub("33501", " ", x)
  x <- gsub("33505", " ", x)
  x <- gsub("33015", " ", x) 
  x <- gsub("33 501", " ", x) 
  x <- gsub("30501", " ", x)
  x <- gsub("100181", " ", x)
  x <- gsub("83501", " ", x)
  x <- gsub("33594", " ", x)
  x <- gsub("33613", " ", x)
  x <- gsub("33619", " ", x)
  x <- gsub("33615", " ", x)
  x <- gsub("D 33739", " ", x)
  x <- gsub("D 33615", " ", x)
  x <- gsub("D33615", " ", x) 
  x <- gsub("3361", " ", x) 
  x <- gsub(" D ", " ", x)

  x <- gsub("GERMANY", " ", x) 
  x <- gsub("NORDRHEIN WESTF", " ", x) 
  x <- gsub("UNIVSTR 25", " ", x)  
  x <- gsub("UNIV STR 25", " ", x)  
  x <- gsub("UNIV STR 27", " ", x) 
  x <- gsub("INSPIRATION 1", " ", x)
  x <- gsub("INTERAKT 1", " ", x)  
  x <- gsub("KONSEQUENZ 45", " ", x)  
  x <- gsub("MORGENBREEDE 45", " ", x)
  x <- gsub("POSTFACH", " ", x) 
  x <- gsub(" FACH", " ", x) 
  x <- gsub("UNIVERSITAETSTASSE 25", " ", x) 
  x <- gsub("UNVERSITATSSTR 25", " ", x)
  x <- gsub("UNIVERSITAETSSTRAßE", " ", x)
  x <- gsub("UNIVERSITÄTSSTRAßE", " ", x)
  x <- gsub("UNIVERSITAETSSTRASSE", " ", x)
  x <- gsub("UNIVERSITAETSSTR", " ", x)
  x <- gsub("UNIVERSITA TSSTRASSE", " ", x)
  x <- gsub("UNIVERSITTSSTRASSE", " ", x)
  x <- gsub("UNIVERSITAETSTRASSE", " ", x)
  x <- gsub("UNIVERSITT ERSTITTSSTRAE", " ", x)
  x <- gsub("UNIVERSITT ERSTITTSSTRAE", " ", x)
  x <- gsub("UNIVERSITT ERSITTSSTR", " ", x)
  x <- gsub("UNIVERSITTSSTRAE", " ", x)
  x <- gsub("UNIVERSITTSTASSE", " ", x)
  x <- gsub("UNIVERSITATSSTRABE", " ", x)
  x <- gsub("UNIVERSITATSSTRASSE", " ", x)
  x <- gsub("UNIVERSITATSSTR", " ", x)
  x <- gsub("UNIVERSITY STRASSE", " ", x)
  x <- gsub("UNIVERSITTSTRAE", " ", x)
  x <- gsub("UNIVERSITTSSTRAE", " ", x) 
  x <- gsub("UNIVERSITTSSTR", " ", x)
  x <- gsub("UNVERSITATSTR", " ", x)
  x <- gsub(" STR", " ", x)
  x <- gsub("25", " ", x) 
  x <- gsub("27", " ", x) 
  x <- gsub(" APPL ", "APPLIED", x)  
  x <- gsub("BIELEFELD UNIV BIELEFELD", "BIELEFELD UNIV", x)  

  x <- trimws(x)  
  x <- gsub("BIELEFELD$", " ", x)
  
  x <- gsub("[[:space:]]+" , " ", x)
  x <- trimws(x) 
  
  AffilPerAu[[i]] <- x
  if( i %% 1000 == 0 ) cat(paste("iteration", i, "complete\n"))
}

# All possible unique institutes/Faculties
allAff <- unique(unname(unlist(AffilPerAu)))
allAff[-grep("BIELEFELD UNIV", allAff)] <- "notBielefeld"
length(unique(allAff))
# keep just Bielfeld affiliations
allAffBi <- allAff[which(allAff != "notBielefeld")]

# (look into it by hand and change the affiliation) or remove it
allAffBi2 <- gsub("BIELEFELD UNIV", "", allAffBi) 
allAffBi2 <- trimws(allAffBi2) 

# remove terms like department, faculty etc
x <- allAffBi2
x <- gsub("DEPARTMENT", " ", x)
x <- gsub("DEPT", " ", x)
x <- gsub("INSTITUTE", " ", x)
x <- gsub("CENTER", " ", x)
x <- gsub("UNIV BIELEFELD", " ", x)
x <- gsub("UNI BIELEFELD", " ", x)
x <- gsub("BIELEFELD", " ", x)
x <- gsub("UNIVERSITY", " ", x)
x <- gsub("UNIVERSITAET", " ", x)
x <- gsub("UNIV", " ", x)
x <- gsub("UNI", " ", x)
x <- gsub("ECOL", "ECOLOGY", x)
x <- gsub("ECON", "ECONOMY", x)
x <- gsub("[[:space:]]+" , " ", x)
x <- trimws(x) 
allAffBi2 <- x



#-------------------------------------------------------------------------------------------------------------

# institutes which should be the clusters at the end
colnames(FacDist) # -> ideally 28 cluster 

# alternative
InstData <- data.frame("id" = paste0("Aff", 1:length(allAffBi)), "review" = allAffBi)
InstDataBi <- data.frame("id" = paste0("Aff", 1:length(allAffBi2)), "review" = allAffBi2)
InstDataBi$review <- as.character(InstDataBi$review)

# remove affiliations which are too short to have input
which(nchar(InstDataBi$review) < 3)
InstDataBi <- InstDataBi[which(nchar(InstDataBi$review) >= 3),]

InstData$id <- as.character(InstData$id)
InstDataBi$id <- as.character(InstDataBi$id)

# which ids were deleted in InstDataBi?
InstData$id[-which(InstData$id %in% InstDataBi$id)]

save.image("Workspaces/ClusterBielefeld_zwischen.Rdata")

#------------------------------------------------------------------------------------------------------------
GebKnoten <- read_excel("Daten/Bielefeld/Fakultat_Knotenpunkte.xlsx", na = "NA")
GebKnoten$ID <- paste0("ID", 1:nrow(GebKnoten))
GebKnoten$keywords <- trimws(GebKnoten$keywords) 

InstDataBi$Inst <- NA

# reorder institutes order to avoid overwriting (e.g. Diabetes)
smallGebKnoten <- GebKnoten[which(!is.na(GebKnoten$keywords)), ]

orderFak <- c("Technische", "CITEC", "Physik", "Chemie", "Biologie", "CeBiTec", 
              "Mathematik", "Wirtschaftswissenschaften",
              "Sportwissenschaft", "Psychologie und Sportwissenschaft",
              "Applied Sciences", "Zentrum für interdisziplinäre Forschung",
              "Institut für interdisziplinäre Konflikt- und Gewaltforschung (IKG)", 
              "Institut für Mathematische Wirtschaftsforschung")

order <- unlist(c(unname(sapply(orderFak, function(x) which(smallGebKnoten$Fakultat == x))),
           which(!smallGebKnoten$Fakultat %in% orderFak)))
smallGebKnoten <- smallGebKnoten[order,]

# set institutes according to keywords
for (k in 1:nrow(smallGebKnoten)){
  keys <- unlist(strsplit(smallGebKnoten$keywords[k], ", "))
  Pattern <- paste(keys, collapse = "|")
  for (i in 1:nrow(InstDataBi)){
    if (grepl(Pattern, InstDataBi$review[i]))
      InstDataBi$Inst[i] <- smallGebKnoten$Fakultat[k]
  }
}

InstDataBi$Inst[is.na(InstDataBi$Inst)] <- "Mixed"

Mixed <- InstDataBi[InstDataBi$Inst == "Mixed", ]
Mixed <- cbind(Mixed, InstData[InstData$id %in% Mixed$id, "review"])

sort(table(InstDataBi$Inst))
#----------

# data which was removed at beginning
InstData$id[-which(InstData$id %in% InstDataBi$id)]
InstDataBi <- rbind(InstDataBi,  cbind(InstData[-which(InstData$id %in% InstDataBi$id),], Inst = "Mixed"))

# change cluster number so that we have only one number per Affiliation
topics <- setNames(c(1:length(sort(unique(InstDataBi$Inst)))), sort(unique(InstDataBi$Inst)))
InstDataBi$cluster <- topics[InstDataBi$Inst]


#-----------------------------------------
# anschließend für jeden Autor wieder zuordnen und dann erst das Maximum nehmen
ClusterPerAu <- AffilPerAu

for (i in 1:length(ClusterPerAu)){
  aut <- ClusterPerAu[[i]]
  if (length(aut) > 0){
    for(j in 1:length(aut)){
      if (aut[j] %in% InstData$review){
        ClusterPerAu[[i]][j] <- InstDataBi[InstDataBi$id == InstData$id[InstData$review == aut[j]], "cluster"]
      }
    }
  }
}

# maximum Institute per author

numMix <- as.integer(topics[which(names(topics) =="Mixed")]) # "Mixed" Cluster

maxInst <- c()
for (i in 1:length(AffilPerAu)) {
  if (length(unlist(AffilPerAu[i])) < 1) {
    maxInst[i] <- NA
  }
  else if (all(is.na(unlist(AffilPerAu[i])))){
    maxInst[i] <- NA
  }
  else{
    # check if Bielefeld is included
    if (length(grep("BIELEFELD UNIV", AffilPerAu[[i]])) > 0) {
      welche <- grep("BIELEFELD UNIV", AffilPerAu[[i]])
      # take maximum affiliation of all publication affiliations (not only HMGU)
      tab <- sort(table(unlist(ClusterPerAu[[i]][welche])), decreasing = TRUE)
      # more than one Bielefeld aff. and first two have same quantity
      if(length(tab) > 1 & sum(tab[1]) == sum(tab[2])){
        if(length(tab[which(sapply(tab, length) == length(which.max(tab)) & names(tab) != numMix)]) <= 1){ # numMix for Mixed
          maxInst[i] <- names(tab[which(sapply(tab, length) == length(which.max(tab)) & names(tab) != numMix)])
        }
        else 
        {
          set.seed(123)
          maxInst[i] <- sample(names(tab[which(sapply(tab, sum) == max(unname(tab)) & (names(tab) != numMix))]), 1)
        }
      }
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
      maxInst[i] <- "notBielefeld" 
    }
  }
}
names(maxInst) <- names(AffilPerAu)

length(unique(maxInst))

MixedAffil <- AffilPerAu[names(which(maxInst == numMix))]
mixedList <- AffilPerAu[names(maxInst[maxInst == numMix])]

maxInst[nchar(maxInst) > 2] <- "notBielefeld"
length(unique(maxInst))
sort(table(maxInst))

sort(table(maxInst))
sort(table(names(topics[as.integer(maxInst)])))

# Institut für Innovationstransfer and SFB 8822 have only one/two authors, therefore we put them to other faculties
which(maxInst == topics["Institut für Innovationstransfer"])
which(maxInst == topics["SFB 8822"])
maxInst["MAY M"] <- as.character(topics["Soziologie"])
maxInst["BROECKEL M"] <- as.character(topics["Soziologie"])
maxInst["BROECKEL M"] <- as.character(topics["Institut für interdisziplinäre Konflikt- und Gewaltforschung (IKG)"])
maxInst["BUSSMANN K"] <- as.character(topics["notBielefeld"])


which(maxInst == topics["Interdisziplinäres Zentrum für Geschlechterforschung (IZG)"])
maxInst <- maxInst[which(names(maxInst) != "ROTH V")] # same as SCHMALENSTROTH V
maxInst <- maxInst[which(names(maxInst) != "WINTER S")] # not Bielefeld but Jena
which(maxInst == topics["Zentrum für interdisziplinäre Forschung"])
maxInst["AGBIH S"] <- as.character(topics["Geschichtswissenschaft, Philosophie und Theologie"])
maxInst["BERNS C"] <- as.character(topics["Mathematik"])
maxInst["BISLEY J"] <- as.character(topics["notBielefeld"])
maxInst["IGREJA V"] <- as.character(topics["notBielefeld"])
maxInst["MAHR D"] <- as.character(topics["Geschichtswissenschaft, Philosophie und Theologie"])
maxInst["HOLLINSHEAD G"] <- as.character(topics["Mixed"]) # eigentlich ZiF
maxInst["SCHALENBERG M"] <- as.character(topics["Mixed"]) # eigentlich ZiF
maxInst["THIEMANN A"] <- as.character(topics["Gesundheitswissenschaften"]) 

which(maxInst == topics["Institut für interdisziplinäre Konflikt- und Gewaltforschung (IKG)"])

#AffilPerAu[names(which(maxInst == topics["Institut für interdisziplinäre Konflikt- und Gewaltforschung (IKG)"]))]
which(maxInst == topics["Forschungsschwerpunkt Mathematische Modellierung"])
#AffilPerAu[names(which(maxInst == topics["Forschungsschwerpunkt Mathematische Modellierung"]))]
maxInst["HUELSMANN F"] <- as.character(topics["Mixed"])
maxInst["JAKOB J"] <- as.character(topics["Technische"])
maxInst["KOMARITZAN M"] <- as.character(topics["Technische"])

which(maxInst == topics["Institut für Mathematische Wirtschaftsforschung"])
maxInst["CLAAS O"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["FERRARI G"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["GAUR F"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["GRIGOROVA M"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["HELLMANN T"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["HERZBERG F"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["KOCH T"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["KUZMICS C"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["MURAVIEV I"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["NENDEL M"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["RIEDEL F"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["SASS A"] <- as.character(topics["notBielefeld"])
maxInst["SASS L"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["SCHMECK M"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["SCHUHMANN P"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["STEG J"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["STUPNYTSKA Y"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["SURUCU O"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["SURUECUE O"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["TIWISINA J"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["ZAHARIEVA A"] <- as.character(topics["Wirtschaftswissenschaften"])
maxInst["TIWARI V"] <- as.character(topics["not Bielefeld"])
maxInst["ZAGKOURIS K"] <- as.character(topics["not Bielefeld"])

AffilPerAu[which(maxInst == 11)]

# mixed category
which(maxInst == numMix)
# we state all authors which have only Mixed Bielefeld Affiliations as "notBielefeld" 
maxInst[which(maxInst == numMix)] <- "notBielefeld"
maxInst[which(is.na(maxInst))] <- "notBielefeld"

maxInstBi <- maxInst[which(maxInst != "notBielefeld")]
length(unique(maxInstBi))
sort(table(maxInstBi))

# Check if institute is right for R Langrock, D Bauer, C Fuchs and S Kurtenbach
maxInst["LANGROCK R"]
AffilPerAu[["LANGROCK R"]]
ClusterPerAu[["LANGROCK R"]]

InstDataBi[InstDataBi$id == InstData$id[InstData$review == "BIELEFELD UNIV DEPT BUSINESS ADM ECON"],]

maxInst["BAUR D"]
AffilPerAu[["BAUR D"]]
ClusterPerAu[["BAUR D"]]

maxInst["FUCHS C"]
AffilPerAu[["FUCHS C"]]
ClusterPerAu[["FUCHS C"]]

maxInst["KURTENBACH S"]
AffilPerAu[["KURTENBACH S"]]
ClusterPerAu[["KURTENBACH S"]]

# add Mixed to distance matrix
FacDistMe <- cbind(FacDistMe, rep(NA, times = nrow(FacDistMe)))
FacDistMe <- rbind(FacDistMe, rep(NA, times = ncol(FacDistMe)))
colnames(FacDistMe)[ncol(FacDistMe)] <- c("Mixed")
rownames(FacDistMe)[ncol(FacDistMe)] <- c("Mixed")

# which names of maxInst are not (yet) in the distance matrix?
unique(maxInst)[which(unique(names(topics[as.integer(maxInst)])) %in% colnames(FacDistMe) == FALSE)]

# change faculty names from german in english
names(topics) <- c("Applied Sciences", "Biology", "CeBiTec", "Chemistry",
                   "CITEC", "Educational Science", "Research Centre Mathematical Modelling",
                   "History and Philosophy and Theology", "Public Health",
                   "Institute for Interdisciplinary Research on Conflict and Violence",
                   "Center for Mathematical Economics", "Interdisciplinary Center for Gender Studies", "Linguistics and Literary Studies",
                   "Mathematics", "Mixed", "Physics", "Psychology and Sports Science", "Law",
                   "Research Institute for Cognition and Robotics", "SFB 8822", "Sociology",
                   "Technology", "Business Administration and Economics", "Center for Interdisciplinary Research"
)

facultiesBi <- topics[sort(as.integer(unique(maxInstBi)))]

# rename colnames/rownames of distance matrix
colnames(FacDistMe) <- rownames(FacDistMe) <- c("Biology", "BiSEd", "CeBiTec", "Chemistry",
                                                "CITEC", "Educational Science", "Research Centre Mathematical Modelling",
                                                "History and Philosophy and Theology", "Public Health",
                                                "Institute for innovation transfer", "Institute for Interdisciplinary Research on Conflict and Violence",
                                                "Center for Mathematical Economics", "Interdisciplinary Center for Gender Research",
                                                "Lab school",
                                                "Linguistics and Literary Studies",
                                                "Mathematics", "Physics", "Psychology and Sports Science", "Law",
                                                "Research Institute for Cognition and Robotics", "SFB 8822", "Sociology", "Sports",
                                                "Technology", "Business Administration and Economics", "Center for Interdisciplinary Research",
                                                "Center for Teaching and Learning", "Applied Sciences", "Mixed"
)
# 
# rename faculties in FacKnoten
FacKnoten$Fakultat <- c("Biology", "BiSEd", "CeBiTec", "Chemistry",
                        "CITEC", "Educational Science", "Research Centre Mathematical Modelling",
                        "History and Philosophy and Theology", "Public Health",
                        "Institute for innovation transfer", "Institute for Interdisciplinary Research on Conflict and Violence",
                        "Center for Mathematical Economics", "Interdisciplinary Center for Gender Research",
                        "Lab school",
                        "Linguistics and Literary Studies",
                        "Mathematics", "Physics", "Psychology and Sports Science", "Law",
                        "Research Institute for Cognition and Robotics", "SFB 8822", "Sociology", "Sports",
                        "Technology", "Business Administration and Economics", "Center for Interdisciplinary Research",
                        "Center for Teaching and Learning", "Applied Sciences"
)

rm(x, welche, tab, Pattern, pap, order, keys, k, i, j, aut, del,
   affilData, affil_all, w, s, p, l, test,
   scopusData)

save.image("Workspaces/ClusterBielefeld.Rdata")
