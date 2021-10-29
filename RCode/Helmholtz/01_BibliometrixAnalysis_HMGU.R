library(bibliometrix)
library(dplyr)
library(RISmed)
library(stringr)
library(pubmedR)
library(readxl)
library(tidyverse)
#---------------------------------------
#--- 1 Read data

## HMGU
# WOS Artcile Year 2015-2019
W <- convert2df(file = "Data/databases/12_10_2020/HMGU_wosCoreCollection1.txt", dbsource = "isi", format = "plaintext")
for(i in 2:17){ 
  filename <- paste0("Data/databases/12_10_2020/HMGU_wosCoreCollection", i, ".txt")
  X <- convert2df(file = filename, dbsource = "isi", format = "plaintext")
  # merge data sets
  W <- bind_rows(W, X)
}

# scopus
O <- convert2df(file = "Data/databases/12_10_2020/HMGU_scopus2015.bib", dbsource = "scopus", format = "bibtex")
for(i in 2016:2019){
  filename <- paste0("Data/databases/12_10_2020/HMGU_scopus", i, ".bib")
  X <- convert2df(filename, dbsource = "scopus", format = "bibtex")
  # merge data sets
  O <- bind_rows(O, X)
}

# pubmed (query date 04/06/2021)
myquery1 <- "(Helmholtz Zentrum Munchen[Affiliation] OR 
 Helmholtz Zentrum Muenchen[Affiliation] OR 
Helmholtz Zentrum Munich[Affiliation] OR 
Helmholtz Center Munich[Affiliation] OR 
Helmholtz Center Munchen[Affiliation] OR 
Helmholtz Center Muenchen[Affiliation] OR 
Helmholtz Research Center Munchen[Affiliation] OR 
Helmholtz Research Center Muenchen[Affiliation] OR 
Helmholtz Research Center Munich[Affiliation] OR 
Research Center for Environmental Health München [Affiliation] OR 
Research Center for Environmental Health Muenchen [Affiliation] OR 
Research Center for Environmental Health Munich[Affiliation]OR 
German Research Center for Environmental Health[Affiliation] OR 
Helmholtz Research Center for Environmental Health[Affiliation] OR 
Helmholtz German Research Center for Environmental Health[Affiliation] OR 
Helmholtz-Zentrum Munchen[Affiliation] OR 
Helmholtz-Zentrum Muenchen[Affiliation] OR 
Helmholtz-Zentrum Munich[Affiliation]) AND 
( 2015/01/01[PDat] : 2015/12/31[PDat])"


myquery2 <-"(Helmholtz Zentrum Munchen[Affiliation] OR 
 Helmholtz Zentrum Muenchen[Affiliation] OR 
Helmholtz Zentrum Munich[Affiliation] OR 
Helmholtz Center Munich[Affiliation] OR 
Helmholtz Center Munchen[Affiliation] OR 
Helmholtz Center Muenchen[Affiliation] OR 
Helmholtz Research Center Munchen[Affiliation] OR 
Helmholtz Research Center Muenchen[Affiliation] OR 
Helmholtz Research Center Munich[Affiliation] OR 
Research Center for Environmental Health München [Affiliation] OR 
Research Center for Environmental Health Muenchen [Affiliation] OR 
Research Center for Environmental Health Munich[Affiliation]OR 
German Research Center for Environmental Health[Affiliation] OR 
Helmholtz Research Center for Environmental Health[Affiliation] OR 
Helmholtz German Research Center for Environmental Health[Affiliation] OR 
Helmholtz-Zentrum Munchen[Affiliation] OR 
Helmholtz-Zentrum Muenchen[Affiliation] OR 
Helmholtz-Zentrum Munich[Affiliation]) AND 
( 2016/01/01[PDat] : 2016/12/31[PDat])"

myquery3 <- "(Helmholtz Zentrum Munchen[Affiliation] OR 
 Helmholtz Zentrum Muenchen[Affiliation] OR 
Helmholtz Zentrum Munich[Affiliation] OR 
Helmholtz Center Munich[Affiliation] OR 
Helmholtz Center Munchen[Affiliation] OR 
Helmholtz Center Muenchen[Affiliation] OR 
Helmholtz Research Center Munchen[Affiliation] OR 
Helmholtz Research Center Muenchen[Affiliation] OR 
Helmholtz Research Center Munich[Affiliation] OR 
Research Center for Environmental Health München [Affiliation] OR 
Research Center for Environmental Health Muenchen [Affiliation] OR 
Research Center for Environmental Health Munich[Affiliation]OR 
German Research Center for Environmental Health[Affiliation] OR 
Helmholtz Research Center for Environmental Health[Affiliation] OR 
Helmholtz German Research Center for Environmental Health[Affiliation] OR 
Helmholtz-Zentrum Munchen[Affiliation] OR 
Helmholtz-Zentrum Muenchen[Affiliation] OR 
Helmholtz-Zentrum Munich[Affiliation]) AND 
( 2017/01/01[PDat] : 2017/12/31[PDat])"

myquery4 <- "(Helmholtz Zentrum Munchen[Affiliation] OR 
 Helmholtz Zentrum Muenchen[Affiliation] OR 
Helmholtz Zentrum Munich[Affiliation] OR 
Helmholtz Center Munich[Affiliation] OR 
Helmholtz Center Munchen[Affiliation] OR 
Helmholtz Center Muenchen[Affiliation] OR 
Helmholtz Research Center Munchen[Affiliation] OR 
Helmholtz Research Center Muenchen[Affiliation] OR 
Helmholtz Research Center Munich[Affiliation] OR 
Research Center for Environmental Health München [Affiliation] OR 
Research Center for Environmental Health Muenchen [Affiliation] OR 
Research Center for Environmental Health Munich[Affiliation]OR 
German Research Center for Environmental Health[Affiliation] OR 
Helmholtz Research Center for Environmental Health[Affiliation] OR 
Helmholtz German Research Center for Environmental Health[Affiliation] OR 
Helmholtz-Zentrum Munchen[Affiliation] OR 
Helmholtz-Zentrum Muenchen[Affiliation] OR 
Helmholtz-Zentrum Munich[Affiliation]) AND 
( 2018/01/01[PDat] : 2018/12/31[PDat])"

myquery5 <- "(Helmholtz Zentrum Munchen[Affiliation] OR 
 Helmholtz Zentrum Muenchen[Affiliation] OR 
Helmholtz Zentrum Munich[Affiliation] OR 
Helmholtz Center Munich[Affiliation] OR 
Helmholtz Center Munchen[Affiliation] OR 
Helmholtz Center Muenchen[Affiliation] OR 
Helmholtz Research Center Munchen[Affiliation] OR 
Helmholtz Research Center Muenchen[Affiliation] OR 
Helmholtz Research Center Munich[Affiliation] OR 
Research Center for Environmental Health München [Affiliation] OR 
Research Center for Environmental Health Muenchen [Affiliation] OR 
Research Center for Environmental Health Munich[Affiliation]OR 
German Research Center for Environmental Health[Affiliation] OR 
Helmholtz Research Center for Environmental Health[Affiliation] OR 
Helmholtz German Research Center for Environmental Health[Affiliation] OR 
Helmholtz-Zentrum Munchen[Affiliation] OR 
Helmholtz-Zentrum Muenchen[Affiliation] OR 
Helmholtz-Zentrum Munich[Affiliation]) AND 
( 2019/01/01[PDat] : 2019/12/31[PDat])"

pubmed_list1 <- pmApiRequest(query = myquery1, limit = 1500, api_key = "6e910e9b6ad4eaaf21ac7367a2fa70aa6a09")
pubmed_list2 <- pmApiRequest(query = myquery2, limit = 1500, api_key = "6e910e9b6ad4eaaf21ac7367a2fa70aa6a09")
pubmed_list3 <- pmApiRequest(query = myquery3, limit = 1500, api_key = "6e910e9b6ad4eaaf21ac7367a2fa70aa6a09")
pubmed_list4 <- pmApiRequest(query = myquery4, limit = 1500, api_key = "6e910e9b6ad4eaaf21ac7367a2fa70aa6a09")
pubmed_list5 <- pmApiRequest(query = myquery5, limit = 1500, api_key = "6e910e9b6ad4eaaf21ac7367a2fa70aa6a09")

P1 <- convert2df(pubmed_list1, dbsource = "pubmed", format = "api")
P2 <- convert2df(pubmed_list2, dbsource = "pubmed", format = "api")
P3 <- convert2df(pubmed_list3, dbsource = "pubmed", format = "api")
P4 <- convert2df(pubmed_list4, dbsource = "pubmed", format = "api")
P5 <- convert2df(pubmed_list5, dbsource = "pubmed", format = "api")

P <- rbind(P1, P2, P3, P4, P5)

'# above pubmed calculations done on server (otherwise: error because of too many requests)
# load pubmed workspace from server
#load("Workspaces/Bibliometrix_Pubmed_HMGU_server.Rdata")
load("Workspaces/Bibliometrix_Pubmed_HMGU_18_06_2020.Rdata")'

rm(pubmed_list1, pubmed_list2, pubmed_list3, pubmed_list4, pubmed_list5, 
   P1, P2, P3, P4, P5, X, D, S, myquery1, myquery2, myquery3, myquery4, myquery5, i, filename)

# data set correction
W$TI <- enc2utf8(W$TI)
W$AB <- enc2utf8(W$AB)

O$TI <- enc2utf8(O$TI)
O$AB <- enc2utf8(O$AB)

P$TI <- enc2utf8(P$TI)
P$AB <- enc2utf8(P$AB)

# combine datasets, look for duplicates
data <- mergeDbSources(W, P, O, remove.duplicated = TRUE)

#--------------------------------------------------------
# with WoS data from Core collection
cols <-  c("AU", "AF", "TI", "SO", "JI", "DT", "DE", "ID", "AB", "C1", "CR", "TC", "PY", "UT", "RP", "DB", "SC", "SN")
data <- data[, which(colnames(data) %in% cols)]
# remove question marks from author names
data$AU <- str_replace_all(data$AU, "\\?", "")
data$AU[str_detect(data$AU, "\\?")]

results <- biblioAnalysis(data)

# AU: Authors
# AF: Author full names
# DE: Author Keywords
# ID: Keywords Plus
# CR: Cited Reference 
# AB: Abstract
# C1: Author Address
# DT: Document Type
# JI: ISO Source Abbreviation
# PY: Year Published
# SO: Publication Name
# TC: Web of Science Core Collection Times Cited Count
# TI: Document title
# UT: Accession Number
# DB: Database
# SC: Subject Category (Research area)

# all author names
authors <- sort(names(results$Authors))

# number of authors per paper
quantile(results[["nAUperPaper"]], c(0.85, 0.88, 0.89, 0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98,  0.99, 1))
# remove all papers which have more than 35 authors
which(results[["nAUperPaper"]] > 35)
data <- data[-which(results[["nAUperPaper"]] > 35),]

# keep only data from years 2015-2018
table(data$PY)
data <- data[data$PY %in% c("2015", "2016", "2017", "2018", "2019"), ]

# repeat results with new data
results <- biblioAnalysis(data)
summary(results, k = 10, pause = FALSE)
authors <- sort(names(results$Authors))
head(authors)
authors[str_detect(authors, "\\(")]
authors <- authors[-which(authors %in% authors[str_detect(authors, "\\(")])]

# List with all papers per author 
PapersPerAu <- list()
for (i in 1:length(authors)){
  PapersPerAu[[i]] <- data$TI[grep(authors[i], data$AU)]
  print(i)
}
names(PapersPerAu) <- authors
PapersPerAu <- PapersPerAu[sort(names(PapersPerAu))]

save.image("Workspaces/pubmedPrep_HMGU.Rdata")

#---------------------------------------------------------------------
#                   GET AFFILIATIONS                                #
authors_orig <- authors
# name preparation
authors <- enc2utf8(authors)
while(any(str_detect(authors, "Ö")) == TRUE){
  authors <- str_replace(authors, "Ö", "OE")
}

while(any(str_detect(authors, "Ä")) == TRUE){
  authors <- str_replace(authors, "Ä", "AE")
}

while(any(str_detect(authors, "Ü")) == TRUE){
  authors <- str_replace(authors, "Ü", "UE")
}

authors <- str_replace(authors, "Ã¤", "AE")
authors <- str_replace(authors, "Ã¼", "UE")
authors <- str_replace(authors, "Ã˜", "O")
authors <- str_replace(authors, "Ã", "OE")

authors <- str_replace(authors, "AE", "A")
authors <- str_replace(authors, "UE", "U")
authors <- str_replace(authors, "OE", "O")


authors <- authors %>% str_replace("\\s([[:alpha:]])([[:alpha:]])*$", ", \\1")
authors <- str_replace(authors, ", [[:alpha:]]+",  str_extract(authors, ", [[:alpha:]]"))
authors <- str_replace_all(authors, "[[:punct:]]", "")
doublenames <- which(str_detect(authors, "[[:alpha:]]+[[:blank:]][[:alpha:]]+[[:blank:]]"))
authors[doublenames] <- unlist(lapply(str_split(authors[doublenames], " "), function(x) {
  paste(paste0(x[1:(length(x)-1)], collapse = ""), x[length(x)])
}))



#---------------------------------------------
# get affiliation data from web of science
affilData <- read_excel("Data/databases/07_04_2021/wos1.xls")
for(i in 2:12){ 
  filename <- paste0("Data/databases/07_04_2021/wos", i, ".xls")
  X <- read_excel(filename)
  # merge data sets
  affilData  <- rbind(affilData , X)
}


# all WOS authors 
aut_wos <- unique(unlist(str_split(affilData$Authors, "; ")))
aut_wos <- str_replace(aut_wos, ", [[:alpha:]]+",  str_extract(aut_wos, ", [[:alpha:]]"))
aut_wos <- unique(aut_wos)

affil_wos <- list()
for (i in 1:length(aut_wos)){
  affil_wos[[i]] <- as.list(affilData$Addresses[which(str_detect(affilData$Addresses, aut_wos[i]))])
  affil_wos[[i]] <- str_split(affil_wos[[i]], "; \\[")
  for (j in 1:length(affil_wos[[i]])){
    affil_wos[[i]][j] <- unlist(str_split(unlist(affil_wos[[i]][j])[which(str_detect(unlist(affil_wos[[i]][j]), aut_wos[i]))], "\\] "))[2]
  }
}
names(affil_wos) <- aut_wos[1:length(affil_wos)]
affil_wos <- affil_wos[sort(names(affil_wos))]

names(affil_wos) <- str_replace_all(names(affil_wos), "[[:punct:]]", "")
affil_wos <- affil_wos[sort(names(affil_wos))]
names(affil_wos) <- str_to_upper(names(affil_wos))

while(any(str_detect(names(affil_wos), "Ö")) == TRUE){
  names(affil_wos) <- str_replace(names(affil_wos), "Ö", "OE")
}

while(any(str_detect(names(affil_wos), "Ä")) == TRUE){
  names(affil_wos) <- str_replace(names(affil_wos), "Ä", "AE")
}

while(any(str_detect(names(affil_wos), "Ü")) == TRUE){
  names(affil_wos) <- str_replace(names(affil_wos), "Ü", "UE")
}


names(affil_wos) <- str_replace(names(affil_wos), "AE", "A")
names(affil_wos) <- str_replace(names(affil_wos), "UE", "U")
names(affil_wos) <- str_replace(names(affil_wos), "OE", "O")

affilWos <- affil_wos
names_wos <- unique(names(affilWos))
affil_wos <- list()
for(i in 1:length(names_wos)){
  affil_wos[[i]] <- as.list(unlist(unname(affilWos[which(names(affilWos) == names_wos[i])])))
}
names(affil_wos) <- names_wos[1:length(affil_wos)]
rm(affilWos)

# Scopus Affiliations
scopusData <- read_csv("Data/databases/07_04_2021/scopus_HMGU_2015.csv")
for(i in 2016:2019){ 
  filename <- paste0("Data/databases/07_04_2021/scopus_HMGU_", i, ".csv")
  X <- read_csv(filename)
  # merge data sets
  scopusData  <- rbind(scopusData , X)
}


test <- scopusData$`Authors with affiliations`
test <- enc2utf8(test)
test2 <- c()
for(i in 1:length(test)){
  x <- str_split(test[i], "; ")
  test2 <- c(test2, str_to_upper(unlist(x)))
}

restAffil <- test2
restAffil <- str_split(restAffil, "\\., ")
for(i in 1:length(restAffil)){
  names(restAffil)[i] <- unlist(restAffil[[i]][1])
  restAffil[[i]] <- unlist(restAffil[[i]][2])
}
restAffil <- unlist(restAffil)


names_scopus <- unique(names(restAffil))
affilScopus <- list()
for(i in 1:length(names_scopus)){
  affilScopus[[i]] <- as.list(unname(restAffil[which(names(restAffil) == names_scopus[i])]))
}
names(affilScopus) <- names_scopus[1:length(affilScopus)]

names(affilScopus) <- str_replace_all(names(affilScopus), "[[:punct:]]", "")
names(affilScopus) <- str_to_upper(names(affilScopus))
names(affilScopus) <- enc2utf8(names(affilScopus))

while(any(str_detect(names(affilScopus), "Ö")) == TRUE){
  names(affilScopus) <- str_replace(names(affilScopus), "Ö", "OE")
}

while(any(str_detect(names(affilScopus), "Ä")) == TRUE){
  names(affilScopus) <- str_replace(names(affilScopus), "Ä", "AE")
}

while(any(str_detect(names(affilScopus), "Ü")) == TRUE){
  names(affilScopus) <- str_replace(names(affilScopus), "Ü", "UE")
}

names(affilScopus) <- str_replace(names(affilScopus), "AE", "A")
names(affilScopus) <- str_replace(names(affilScopus), "UE", "U")
names(affilScopus) <- str_replace(names(affilScopus), "OE", "O")

names(affilScopus) <- names(affilScopus) %>% str_replace("\\s([[:alpha:]])([[:alpha:]])*$", ", \\1")
names(affilScopus) <- str_replace(names(affilScopus), ", [[:alpha:]]+",  str_extract(names(affilScopus), ", [[:alpha:]]"))
names(affilScopus) <- str_replace_all(names(affilScopus), "[[:punct:]]", "")
affilScopus <- affilScopus[sort(names(affilScopus))]
length(unique(names(affilScopus)))

names_scopus <- unique(names(affilScopus))
affil_Scopus <- list()
for(i in 1:length(names_scopus)){
  affil_Scopus[[i]] <- as.list(unlist(unname(affilScopus[which(names(affilScopus) == names_scopus[i])])))
}
names(affil_Scopus) <- names_scopus[1:length(affil_Scopus)]
rm(affilScopus)

# Pubmed
singleString <- paste(readLines("Data/databases/07_04_2021/pubmed-HelmholtzZ-set_edit.txt", encoding = "UTF-8"), collapse=" ")
pubmedVector <-  unlist(str_split(singleString, ";"))

pubmed_names <- pubmedVector[str_detect(pubmedVector, "FAU ")]
pubmed_names <- str_replace(pubmed_names, "FAU   ", "")
pubmed_names <- enc2utf8(pubmed_names)
pubmed_names <- str_to_upper(pubmed_names)
pubmed_affil <- pubmedVector[str_detect(pubmedVector, "AU ")]
pubmed_affil <- pubmed_affil[which(str_detect(pubmed_affil, "FAU ") == FALSE)]
pubmed_affil <- str_replace(pubmed_affil, "AU    ", "")

affilPubmed <- list()
for (i in 1:length(pubmed_affil)){
  affilPubmed[[i]] <-  str_split(pubmed_affil[i], "  AD   ")[[1]][2]
  names(affilPubmed)[i] <-  str_split(pubmed_affil[i], "  AD   ")[[1]][1]
}
names(affilPubmed) <- str_to_upper(names(affilPubmed))

names(affilPubmed) <- str_replace_all(names(affilPubmed), "[[:punct:]]", "")
names(affilPubmed) <- names(affilPubmed) %>% str_replace("\\s([[:alpha:]])([[:alpha:]])*$", ", \\1")
names(affilPubmed) <- str_replace(names(affilPubmed), ", [[:alpha:]]+",  str_extract(names(affilPubmed), ", [[:alpha:]]"))
names(affilPubmed) <- str_replace_all(names(affilPubmed), "[[:punct:]]", "")
affilPubmed <- affilPubmed[sort(names(affilPubmed))]

while(any(str_detect(names(affilPubmed), "Ö")) == TRUE){
  names(affilPubmed) <- str_replace(names(affilPubmed), "Ö", "OE")
}

while(any(str_detect(names(affilPubmed), "Ä")) == TRUE){
  names(affilPubmed) <- str_replace(names(affilPubmed), "Ä", "AE")
}

while(any(str_detect(names(affilPubmed), "Ü")) == TRUE){
  names(affilPubmed) <- str_replace(names(affilPubmed), "Ü", "UE")
}

names(affilPubmed) <- str_replace(names(affilPubmed), "AE", "A")
names(affilPubmed) <- str_replace(names(affilPubmed), "UE", "U")
names(affilPubmed) <- str_replace(names(affilPubmed), "OE", "O")

names_pubmed <- unique(names(affilPubmed))
affil_pubmed <- list()
for(i in 1:length(names_pubmed)){
  affil_pubmed[[i]] <- as.list(unlist(unname(affilPubmed[which(names(affilPubmed) == names_pubmed[i])])))
}
names(affil_pubmed) <- names_pubmed[1:length(affil_pubmed)]
rm(affilPubmed)


# put all three affiliations together

allPubNames <- sort(unique(c(names(affil_pubmed), names(affil_Scopus), names(affil_wos))))
length(which(authors %in% allPubNames))

affil_all <- list()
for(i in 1:length(allPubNames)){
  if (allPubNames[i] %in% names(affil_wos)){
    affil_all[[i]] <- affil_wos[[allPubNames[i]]]
  } else if (allPubNames[i] %in% names(affil_Scopus)) {
    affil_all[[i]] <- affil_Scopus[[allPubNames[i]]]
  } else if (allPubNames[i] %in% names(affil_pubmed)) {
    affil_all[[i]] <- affil_pubmed[[allPubNames[i]]]
  } else {
    affil_all[[i]] <- NA
  }
}
names(affil_all) <- allPubNames[1:length(affil_all)]

authorsWithoutAff <- authors[which(authors %in% allPubNames == FALSE)]
l <-  vector(mode = "list", length = length(authorsWithoutAff))
for(i in 1:length(l)){
  l[[i]] <- as.list(l[[i]])
}
names(l) <- authorsWithoutAff
Affil <- append(affil_all, l)
Affil <- Affil[sort(names(Affil))]

rm(cols, i, j, n, y, l, filename)

save.image("Workspaces/workspaceHMGU_DatPrep_Bibliometrix.Rdata")
