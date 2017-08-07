rm(list = ls(all=T))

# ?Rprofile
.libPaths('D:/R/library')
.libPaths() # Defined in Rprofile

setwd("D:/Google Drive/EEA/R/")
getwd()
load("D:/Google Drive/EEA/R/EEA.RData")
# save(list=ls(all=TRUE), file="D:/Google Drive/EEA/R/EEA.RData") # save RDATA for later use

library(rJava) # Make sure that the Windows environment settings are set correctly for Java!
library(xlsx)
library(taxize)
library(rgbif)
library(XML)
library(rjson)
library(jsonlite)
library(RJSONIO)
library(httr)
library(curl)
library(RCurl)
library(plyr)
library(dplyr)
library(stringr)
library(wikitaxa)

# ES <- read.xlsx('D:/Google Drive/EEA/enlargement_species 20170310.xlsx', 1) # Issues with character encoding
ES <- read.csv('D:/Google Drive/EEA/enlargement_species 20170310_corrected.csv', header=T) # Stenobothrodes eurasius - wrong brackets
head(ES); dim(ES) # 194   7
ES$ID <- 1:nrow(ES)
head(ES); dim(ES) # 194   7
str(ES)
ES$species_name <- as.character(ES$species_name) # from factor to charater
str(ES)
levels(ES$priority)
# https://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-remove-trailing-leading-spaces
# ?regex
str(ES$species_name[1])

# Capitalize all genus names https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string

gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", ES$species_name[1], perl=TRUE) # \\1 --> first part expression (^|[[:space:]]) - \\2 --> second part of the expression ([[:alpha:]]) - \\U --> Upper case. NB: Don't use spaces!!!
gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", "aconitum firmum", perl=TRUE)
gsub("(^)([[:alpha:]])", "\\1\\U\\2", "aconitum firmum", perl=TRUE) # Genus capitalized

ES$species_name <- gsub("(^)([[:alpha:]])", "\\1\\U\\2", ES$species_name, perl=TRUE)
head(ES); dim(ES) # 194   7

# Remove double spaces https://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-remove-trailing-leading-spaces

gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", " Try   this ", perl=TRUE)
ES$species_name <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", ES$species_name, perl=TRUE)
head(ES); dim(ES)

### Get genus, species and subspecies names ####

# PLANTS infraspecific https://en.wikipedia.org/wiki/Infraspecific_name
# ANIMALS trinomen https://en.wikipedia.org/wiki/Trinomen; In case of animals the subspecies name follows directly after the species name
# Replace ssp and ssp. spp and spp. and subsp BY subsp.

x <- 'Aconitum firmum (Reichenb.) Neilr ssp. moravicum Skalicky'
x
gsub("ssp.", "subsp.", x)
ES$species_name <- gsub("subsp ", "subsp. ", ES$species_name)
ES$species_name <- gsub("ssp ", "subsp. ", ES$species_name)
ES$species_name <- gsub("ssp. ", "subsp. ", ES$species_name)
ES$species_name <- gsub("spp ", "subsp. ", ES$species_name)
ES$species_name <- gsub("spp. ", "subsp. ", ES$species_name)
head(ES); dim(ES)

# https://stackoverflow.com/questions/36694028/extracting-words-from-a-string-in-r - We match the pattern one or more non-white space (\\S+) followed by one or more white space (\\s+) that gets repeated 1 times ({1}) followed by word that is captured in a group ((\\w+)) followed by one or more characters. We replace it by the second backreference
sub("(\\S+\\s){1}(\\w+).*", "\\2", ES$species_name[111]) # (Not space (\\S) at least 1 time (+) followed by space (\\s)), exactly 1 time ({1}), followed by word characters (\\w) at least 1 time (+) followed by a character (.) at least 0 times (*)
gsub("(\\S+\\s){1}(\\S+).*", "\\2", ES$species_name[111])

# ES$species <- gsub("(\\S+\\s){1}(\\S+).*", "\\2", ES$species_name) # Species
head(ES); dim(ES) # 194  7

x
gsub("(\\S+\\s){1}(\\S+).*", "\\1", x) # Genus
gsub("([A-Za-z]+).*", "\\1", ES$species_name[111]) # Genus
ES[111, c("species_name")]
strsplit(ES[111, c("species_name")], "(\\s)")[[1]][1] # Genus

gsub("(\\S+\\s){1}(\\S+).*", "\\2", x) # Species
gsub("(\\S+\\s){1}(\\S+).*", "\\2", ES$species_name[111]) # Species
strsplit(ES[111, c("species_name")], "(\\s)")[[1]][2] # Species

strsplit(ES[111, c("species_name")], "(\\s)")[[1]][3] # Subspecies animals
strsplit(ES[111, c("species_name")], "([[:space:]])")[[1]][3]

for(i in 1:nrow(ES)){
ES$genus[i] <- strsplit(ES$species_name, "(\\s)")[[i]][1] # Genus
ES$species[i] <- strsplit(ES$species_name, "(\\s)")[[i]][2] # Species
ES$subspecies[i] <- strsplit(ES$species_name, "(\\s)")[[i]][3] # Subspecies Animals
}

head(ES); dim(ES) # 194  10

# Remove all subspecies content starting with a capital or punctation

x <- grep("[[:upper:]]|[[:punct:]]", ES$subspecies)
x
ES$subspecies[x] <- ""
ES[is.na(ES)] <- ""

head(ES); dim(ES) # 194  10

# Get ssp. names https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
# x <- 'aabb.ccdd'
# sub('bb.*', '', x)

i=2
x <- grep(" subsp. ", ES$species_name) # Subspecies Plants
x
str(x)

head(ES); dim(ES) # 194  10

for(i in x){
  ES[i, c('subspecies')] <- strsplit(ES[i, c('species_name')], " subsp. ")[[1]][2] # Subspecies Plants
  ES[i, c('subspecies')] <- strsplit(ES[i, c('subspecies')], "(\\s)")[[1]][1] # Subspecies Plants
}

head(ES); dim(ES) # 194  10
ES[67,] # Dianthus plumarius subsp. regis-stephani (Rapcs.) Baksay

# ES$subspecies <- gsub("([A-Za-z]+).*", "\\1", ES$subspecies) # Get first word
# ES$subspecies <- gsub("([A-Za-z]+).*", "\\1", ES$subspecies) # Get first word
ES$subspecies <- tolower(ES$subspecies)
head(ES); dim(ES) # 194  10

# Get canonical trinomial name

ES$trinomial <- paste(ES$genus, ES$species, ES$subspecies, sep=" ")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ES$trinomial <- trim(ES$trinomial)
head(ES); dim(ES) # 194  11

write.csv(ES, 'ES.trinomial.csv', row.names = F)
ES <- read.csv('ES.trinomial.csv', header=T)
head(ES); dim(ES) # 194  11

#############################
### 1. Get accepted name ####
#############################

ES[1, c('trinomial')] # Aconitum firmum moravicum
sources <- gnr_datasources() # taxize
sources # 100 taxonomic databases including CoL
# write.csv(sources, 'GNR.sources.R.csv', row.names = F)
CoL <- sources$id[sources$title == 'Catalogue of Life']
CoL

# Resolve names using Global Names Resolver http://resolver.globalnames.org or http://gni.globalnames.org
gnr_resolve(ES[1, c('trinomial')])
gnr_resolve(ES[1, c('trinomial')], canonical = F, preferred_data_sources=CoL, http='get')
gnr_resolve(ES[1, c('species_name')], canonical = T, preferred_data_sources=CoL) # remove author epithets

### Global Names Crossmapper http://listresolver.globalnames.org/ ####
# Export xlsx to csv using UTF-8: Save as -> csv -> Tools -> Web Options... -> TAB Encoding --> Unicode (UTF-8) https://help.surveygizmo.com/help/encode-an-excel-file-to-utf-8-or-utf-16
# Open returned csv in Excel: From text -> delimited -> comma

### CoL - Webservices http://webservice.catalogueoflife.org/col/webservice ####

# url.xml <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[1, c('trinomial')]), '&format=xml&response=full')
# url.xml

# Species 1 = infraspecies; species 3 = species and synonym --> Different JSON output because species 3 is a synonym
# response can be full or terse (limited data)
# Start with Terse. In case of a synonym name --> Get accepted name id --> then full response --> to get all synonym and common names

### CoL Terse response ####

# Species 1 - accepted species
if (ES[1, c('subspecies')]=="") {
  url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[1, c('trinomial')]), '&format=json&response=terse&rank=species')
} else {
  url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[1, c('trinomial')]), '&format=json&response=terse&rank=infraspecies')
}

# Species 3 - synonym species
if (ES[3, c('subspecies')]=="") {
  url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[3, c('trinomial')]), '&format=json&response=terse&rank=species')
} else {
  url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[3, c('trinomial')]), '&format=json&response=terse&rank=infraspecies')
}

# Species 111 - multiple results
if (ES[111, c('subspecies')]=="") {
  url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[111, c('trinomial')]), '&format=json&response=terse&rank=species')
} else {
  url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[111, c('trinomial')]), '&format=json&response=terse&rank=infraspecies')
}

url.json

# json <- getURL(url.json)
# json
json <- fromJSON(getURL(url.json))
json
names(json)
json[[1]]
json[['name']]
json[[2]]
json[[3]]
json[[4]] # 14 for sp 111
json[[9]]
json[['results']]
str(json[['results']]) # List of 11
str(json[['results']][[1]])
str(json[['results']][[2]])
length(json[[9]][[1]]) # 11
names(json[[9]][[1]])
json[['results']][[1]]['id']
json[['results']][[1]]['name']
json[['results']][[1]]['rank']
json[['results']][[1]]['name_status'] # Synonym
json[['results']][[1]]['online_resource']
json[['results']][[1]]['source_database']
json[['results']][[1]]['source_database_url']
json[['results']][[1]]['bibliographic_citation']
json[['results']][[1]]['name_html']
json[['results']][[1]]['url']
json[['results']][[1]]['accepted_name']

str(json[['results']][[1]]['accepted_name']) # List of 12
names(json[['results']][[1]]['accepted_name'][['accepted_name']])
json[['results']][[1]]['accepted_name'][['accepted_name']]['id']
json[['results']][[1]]['accepted_name'][['accepted_name']]['name']

# If 'name_status' is 'accepted' --> use 'name' and 'id'; if 'name_status' is 'synonym' --> use [['accepted_name']]['name'] and [['accepted_name']]['id']

# Add empty columns
head(ES); dim(ES)
ES$col.id <- ""
ES$name_status <- ""
ES$col.id.accepted <- ""
ES$accepted_name <- ""
ES$name_html <- ""
head(ES); dim(ES)

### Loop CoL terse through species list ####
i=3

pb <- txtProgressBar(min = 0, max = nrow(ES), style = 3)

# for(i in 1:3){
for(i in 1:nrow(ES)){
  
  Sys.sleep(0.2)
  
  if (ES[i, c('subspecies')]=="") {
    url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[i, c('trinomial')]), '&format=json&response=terse&rank=species')
  } else {
    url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[i, c('trinomial')]), '&format=json&response=terse&rank=infraspecies')
  }
  url.json
  json <- fromJSON(getURL(url.json))
  json
  
  if (json[['total_number_of_results']] == 0) {
    ES[i, c('col.id')] <- ""
    ES[i, c('name_status')] <- ""
    ES[i, c('col.id.accepted')] <- ""
    ES[i, c('accepted_name')] <- ""
    ES[i, c('name_html')] <- ""
  } else {
    ES[i, c('col.id')] <- json[['results']][[1]]['id']
    ES[i, c('name_status')] <- json[['results']][[1]]['name_status']
    
    if (json[['results']][[1]]['name_status']=="synonym") {
      ES[i, c('col.id.accepted')] <- json[['results']][[1]]['accepted_name'][['accepted_name']]['id']
      ES[i, c('accepted_name')] <- json[['results']][[1]]['accepted_name'][['accepted_name']]['name']
      ES[i, c('name_html')] <- json[['results']][[1]]['accepted_name'][['accepted_name']]['name_html']
    } else {
      ES[i, c('col.id.accepted')] <- json[['results']][[1]]['id']
      ES[i, c('accepted_name')] <- json[['results']][[1]]['name']
      ES[i, c('name_html')] <- json[['results']][[1]]['name_html']
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)

head(ES); dim(ES) #  194  16
ES$name_html <- gsub("<i>", "", ES$name_html)
ES$name_html <- gsub("</i>", "", ES$name_html)
head(ES); dim(ES) #  194  16
ES[10,]

write.csv(ES, 'ES.terse.csv', row.names=F)
ES <- read.csv('ES.terse.csv', header = T)

head(ES); dim(ES) #  194  16

### Combine Global Names Crossmapper with CoL

GN.crossmapper <- read.csv('match enlargement_species 20170310_corrected_cleaned.csv', header=T)
head(GN.crossmapper); dim(GN.crossmapper) # 194  18
ES.GN <- cbind(ES, GN.crossmapper[,c('synonymStatus', 'acceptedName', 'matchTaxonID', 'matchedScore')])
head(ES.GN)
write.csv(ES.GN, 'ES.GN.csv', row.names=F)

### CDM Fuzzy search ####

# http://zevross.com/blog/2015/02/12/using-r-to-download-and-parse-json-an-example-using-data-from-an-open-data-portal/
# http://api.cybertaxonomy.org/col/name_catalogue/fuzzy

http://api.cybertaxonomy.org/col/name_catalogue/fuzzy.json?query=Anacamptis urvilleana&accuracy=0.6&hits=2

url.json.cdm <- paste0("http://api.cybertaxonomy.org/col/name_catalogue/fuzzy.json?query=", ES[1, c('trinomial')], '&accuracy=0.8&hits=1')
url.json.cdm

# json.cdm <- read_json(url.json.cdm) # simplifyDataFrame = T
json.cdm <- fromJSON(url.json.cdm)
json.cdm
class(json.cdm) # list
names(json.cdm[[1]])
str(json.cdm)
View(json.cdm)
str(json.cdm[[1]][['response']])
str(json.cdm[[1]][['response']][[1]])
json.cdm[[1]][['response']][[1]]$name

ES$cdm.score <- ""
ES$cdm.name <- ""
head(ES)

i=4

pb <- txtProgressBar(min = 0, max = nrow(ES), style = 3)

# for(i in 1:3){
for(i in 1:nrow(ES)){
  
  Sys.sleep(0.2)
  
  url.json.cdm <- paste0("http://api.cybertaxonomy.org/col/name_catalogue/fuzzy.json?query=", ES[i, c('trinomial')], '&accuracy=0.8&hits=1')
  url.json.cdm
  json.cdm <- fromJSON(url.json.cdm)
  json.cdm
  length(names(json.cdm[[1]]))
  if(length(names(json.cdm[[1]])) == 1){
    ES[i, c('cdm.score')] <- ""
    ES[i, c('cdm.name')] <- ""
  } else {
    ES[i, c('cdm.score')] <- json.cdm[[1]][['response']][[1]]$score
    ES[i, c('cdm.name')] <- json.cdm[[1]][['response']][[1]]$name
  }
  
  setTxtProgressBar(pb, i)
}

close(pb)

head(ES)
str(ES)
ES$trinomial <- as.character(ES$trinomial)

ES$tri.vs.cdm <- ""

for(i in 1:nrow(ES)){
  if(ES[i, c('trinomial')] == ES[i, c('cdm.name')]){
    ES[i, c('tri.vs.cdm')] <- 1
  } else {
    ES[i, c('tri.vs.cdm')] <- 0
  }
}

head(ES)

write.csv(ES, 'ES.terse.cdm.csv', row.names=F)
ES <- read.csv('ES.terse.cdm.csv', header = T)

### Global Names Index http://gni.globalnames.org/name_strings #####

http://gni.globalnames.org/name_strings/1265133.json?all_records=false
http://gni.globalnames.org/name_strings/1265133.json?all_records=0
http://gni.globalnames.org/name_strings/1265133.xml
http://gni.globalnames.org/name_strings/1265133
http://gni.globalnames.org/parsers.xml?names=Plantago+minor|Homo+sapiens
# http://gni.globalnames.org/parsers.json?names=Aconitum+firmum+moravicum&callback=displayNames
http://gni.globalnames.org/parsers.xml?names=Aconitum+firmum+(reichenb.)+neilr+subsp.+moravicum+skalicky
http://gni.globalnames.org/parsers.xml?names=Aconitum+firmum+moravicum








ES.col <- ES[! ES$col.id == "",]
head(ES.col); dim(ES.col) # 166  17

ES.cdm.fuzzy <- ES[ES$col.id == "",]
head(ES.cdm.fuzzy); dim(ES.cdm.fuzzy) # 28  17


### CoL Full response ####

if (ES[3, c('subspecies')]=="") {
  url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[3, c('trinomial')]), '&format=json&response=full&rank=species')
} else {
  url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[3, c('trinomial')]), '&format=json&response=full&rank=infraspecies')
}

# Terse
if (ES[3, c('subspecies')]=="") {
  url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[3, c('trinomial')]), '&format=json&response=terse&rank=species')
} else {
  url.json <- paste0('http://webservice.catalogueoflife.org/col/webservice?name=', gsub(" ", "+", ES[3, c('trinomial')]), '&format=json&response=terse&rank=infraspecies')
}

url.json

json <- getURL(url.json)
json
json <- fromJSON(getURL(url.json))
json
names(json)
json[[1]]
json[[2]]
json[[3]]
json[[9]]
json[['results']]
str(json[['results']])
str(json[['results']][[1]])
length(json[[9]])
length(json[[9]][[1]]) # 24

json[[9]][[1]]
json[[9]][[1]][1]; json[['results']][[1]]['id']
"http://webservice.catalogueoflife.org/col/webservice?name=Anacamptis+urvilleana&format=json&response=full&rank=species"
"http://webservice.catalogueoflife.org/col/webservice?id=3fa393bab9b113f27233471cdf445a36&format=json&response=full"
json[[9]][[1]][2]; json[['results']][[1]]['name']
json[[9]][[1]][3]; json[['results']][[1]]['rank']
json[[9]][[1]][4]; json[['results']][[1]]['name_status']
json[[9]][[1]][5]; json[['results']][[1]]['genus']
json[[9]][[1]][6]; json[['results']][[1]]['subgenus']
json[[9]][[1]][7]; json[['results']][[1]]['species']
json[[9]][[1]][8]; json[['results']][[1]]['infraspecies_marker']
json[[9]][[1]][9]; json[['results']][[1]]['infraspecies']
json[[9]][[1]][10]; json[['results']][[1]]['author']
json[[9]][[1]][11]; json[['results']][[1]]['record_scrutiny_date']; # str(json[['results']][[1]]['record_scrutiny_date'])
json[[9]][[1]][12]; json[['results']][[1]]['online_resource']
json[[9]][[1]][13]; json[['results']][[1]]['is_extinct']
json[[9]][[1]][14]; json[['results']][[1]]['source_database']
json[[9]][[1]][15]; json[['results']][[1]]['source_database_url']
json[[9]][[1]][16]; json[['results']][[1]]['bibliographic_citation']
json[[9]][[1]][17]; json[['results']][[1]]['name_html']
json[[9]][[1]][18]; json[['results']][[1]]['url']
json[[9]][[1]][19]; json[['results']][[1]]['distribution']
json[[9]][[1]][20]; json[['results']][[1]]['references']
json[[9]][[1]][21]; json[['results']][[1]]['classification']
json[[9]][[1]][22]; json[['results']][[1]]['child_taxa']
json[[9]][[1]][23]; json[['results']][[1]]['synonyms']
json[[9]][[1]][24]; json[['results']][[1]]['common_names']
str(json[['results']][[1]]['accepted_name'])
length(json[['results']][[1]][['accepted_name']]) # 24
json[['results']][[1]][['accepted_name']]['name']



### Synonyms ####

test <- synonyms(ES[3, c('trinomial')], db="col"); test
str(json[['results']][[1]]['id'])
x <- unlist(json[['results']][[1]]['id'])
str(x)
test <- synonyms(x, db="col"); test

test.poa.annua <- synonyms("Poa annua", db="col")[[1]]; test.poa.annua
test.poa.annua <- synonyms("3b35900f74ff6e4b073ddb95c32b1f8d", db="col") # use accepted name id
test.poa.annua$`3b35900f74ff6e4b073ddb95c32b1f8d`
str(test.poa.annua$`3b35900f74ff6e4b073ddb95c32b1f8d`)
test.poa.annua <- test.poa.annua$`3b35900f74ff6e4b073ddb95c32b1f8d`
head(test.poa.annua)
names(test.poa.annua)

synonyms(get_tsn("Poa annua"))
synonyms("Poa annua", db="itis")
name_suggest(q='Adenophora lilifolia') # rgbif

########################