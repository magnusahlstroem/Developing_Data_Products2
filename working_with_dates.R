library(dplyr); library(ggmap)
##########################################################################
# Michael L. Bernauer
# mlbernauer@gmail.com
# 12/14/2014
# Module for parsing PubMed Medline files.
# Files should be downloaded to your
# computer and loaded into R by passing the
# file path into the medline function.
# The function returns a list containing
# each Medline entry.
#
# USAGE:
# source('medline.R')
# medline_records <- medline("/home/user/Downloads/pubmed_results.txt")
##########################################################################
setwd("C:/Kurser/Developing_Data_Products2")
medline = function(file_name){
  lines <- readLines(file_name)
  medline_records <- list()
  key <- 0
  record <- 0
  for(line in lines){
    header <- sub(" {1,20}", "", substring(line, 1, 4))
    value <- sub("^.{6}", "", line)
    if(header == "" & value == ""){
      next
    }
    else if(header == "PMID"){
      record = record + 1
      medline_records[[record]] <- list()
      medline_records[[record]][header] <- value
    }
    else if(header == "" & value != ""){
      medline_records[[record]][key] <- paste(medline_records[[record]][key], value)
    }
    else{
      key <- header
      if(is.null(medline_records[[record]][key][[1]])){
        medline_records[[record]][key] <- value
      }
      else { 
        medline_records[[record]][key] <- paste(medline_records[[record]][key], value, sep=";")
      }
    }
  }
  return(medline_records)
}

load("C:/Kurser/Data_medline/parsed_2018_1.Rdata")
load("C:/Kurser/Data_medline/parsed_2018_2.Rdata")
load("C:/Kurser/Data_medline/parsed_2018_3.Rdata")
load("C:/Kurser/Data_medline/parsed_2018_4.Rdata")
load("C:/Kurser/Data_medline/parsed_2018_5.Rdata")
load("C:/Kurser/Data_medline/parsed_2018_6.Rdata")
load("C:/Kurser/Data_medline/parsed_2018_7.Rdata")



affiliations <- lapply(parsed_2018_7, function(x) {
    fa_aff <- if (!is.null(x$AD)) strsplit(x$AD, ";")[[1]][1] else "NA"
    fa_aff <- gsub("\\-", "\\ ", fa_aff)
    fa_aff <- gsub("&", "and", fa_aff)
    fa_aff <- gsub("(?!,)[[:punct:]]", " ", fa_aff, perl=TRUE)
    Dates <- if (!is.null(x$PHST)) x$PHST else "NA"
    ri <- grepl("recieved", strsplit(Dates, ";")[[1]])
    ai <- grepl("accepted", strsplit(Dates, ";")[[1]])
    Date_received <- substr(strsplit(Dates, ";")[[1]][ri], 1, 10)
    Date_accepted <- substr(strsplit(Dates, ";")[[1]][ai], 1, 10)
    Date_received <- if(identical(Date_received, character(0))) NA else Date_received
    Date_accepted <- if(identical(Date_accepted, character(0))) NA else Date_accepted
    Published <- if (!is.null(x$DP)) x$DP else "NA"
    Created <- if (!is.null(x$DA)) x$DA else "NA"
    CTDT <- if (!is.null(x$CTDT)) x$CTDT else "NA"
    Last_revised <- if (!is.null(x$LR)) x$LR else "NA"
    DEP <- if (!is.null(x$DEP)) x$DEP else "NA"
    LR <- if(!is.null(x$LR)) x$LR else "NA"
    index_uni <- regexpr("\\,[[:alpha:][:space:]]*[Uu][Nn][Ii][Vv][[:alpha:][:space:]]*\\,", fa_aff)
    index_uni <- if (index_uni == -1 | identical(index_uni, integer(0))) regexpr("^[[:alpha:][:space:]]*?[Uu][Nn][Ii][Vv][[:alpha:][:space:]]*\\,", fa_aff) else index_uni
    index_uni <- if (index_uni == -1 | identical(index_uni, integer(0))) regexpr("\\,[[:alpha:][:space:]]*[Uu][Nn][Ii][Vv][[:alpha:][:space:]]*[[:punct:]]?$", fa_aff) else index_uni
    index_uni <- if (index_uni == -1 | identical(index_uni, integer(0))) regexpr("^[[:alpha:][:space:]]*?[Uu][Nn][Ii][Vv][[:alpha:][:space:]]*[[:punct:]]?$", fa_aff) else index_uni
    uni <- sub("^\\,\\ ", "", substr(fa_aff, index_uni, index_uni[1] + attributes(index_uni)[[1]]))
    uni_cleansed <- trimws(sub("\\,\\ $", "", sub("^\\,\\ *", "", uni)))
    list(University = uni_cleansed, PMID = x$PMID, 
         Date_received = Date_received, Date_accepted = Date_accepted, 
         Created = Created, Published = Published, CTDT = CTDT, DEP = DEP,
         LR = LR)
})

affiliations2 <- do.call(rbind.data.frame, c(affiliations, stringsAsFactors = F)) %>%
  mutate(Published = sub("\\/", "-", Published),
         Published = sub("\\ -\\ ", "-", Published),
         Published = ifelse(nchar(Published) == 4, NA, Published),
         Published = sub("[[:alpha:]]{3}-", "", Published),
         Published = as.Date(Published, format = "%Y %b %d"),
         CTDT = as.Date(CTDT, format = "%Y%m%d"),
         DEP = as.Date(DEP, format = "%Y%m%d"),
         LR = as.Date(LR, format = "%Y%m%d"),
         Created = as.Date(Created, format = "%Y%m%d"),
         Published = ifelse(is.na(Published), CTDT, Published),
         Published = ifelse(is.na(Published), DEP, Published),
         Published = ifelse(is.na(Published), LR, Published),
         Published = ifelse(is.na(Published), Created, Published),
         Published = as.Date(Published, origin = "1970-01-01"),
         University = gsub("\\ {1,}", " ", University),
         University = sub("\\,[[:alpha:]]*?$", "", University),
         University = sub("^[[:alpha:]]\\ ", "", University),
         University = sub("Univeristy", "University", University),
         University = sub("^[Aa]ffiliated Hospital of ", "", University),
         University = sub("^[Aa]ffiliated to ", "", University),
         University = sub("^[Aa]ffiliated with ", "", University),
         University = sub("\\ [[:upper:]]*$", "", University)) %>%
  #mutate(University = as.character(University)) %>%
  filter(University != "") %>%
  select(University, PMID, Published)

#load("C:/Kurser/Developing_Data_Products/homework1/lon_lat.Rdata")
#affiliations_lon_lat2 <- left_join(affiliations2, lon_lat2, by = "University")

#missing_lon_lat <- filter(affiliations_lon_lat2, is.na(lon))

#unis <- unique(missing_lon_lat$University)
#ordered <- unis[order(unis)]


#lon_lat_dsk2500 <- geocode(ordered[1:2500], source = "dsk")
#lon_lat_dsk5000 <- geocode(ordered[2501:5000], source = "dsk")
#lon_lat_dsk7500 <- geocode(ordered[5001:7500], source = "dsk")
#lon_lat_dsk10000 <- geocode(ordered[7501:10000], source = "dsk")
#lon_lat_dsk12500 <- geocode(ordered[10001:12500], source = "dsk")
#lon_lat_dsk12568 <- geocode(ordered[12501:12568], source = "dsk")

#lon_lat3 <- cbind(ordered, do.call(rbind, mget(ls()[grepl("lon_lat_dsk", ls())][c(4,5,6,1,2,3)]))) %>%
#  rename(University = ordered)
#save(lon_lat3, file = "C:/Kurser/Data_medline/lon_lat3.Rdata")
#load("C:/Kurser/Data_medline/lon_lat3.Rdata")
#coords <- filter(lon_lat3, !is.na(lon))
#no_coords <- filter(lon_lat3, is.na(lon))

#ud_med <- data.frame(lon = c(-101.8921, 2.16667, 11.97200, 12.00000, 14.48333, 16.49035, 33.50640, 103.78306), 
#                     lat = c(33.58928, 7.25000, 51.48300, 4.75000, 12.16667, 47.32442, 39.84530, 1.29528))

#lon_lat <- bind_rows(lon_lat2, coords) %>%
#  select(-n) %>%
#  mutate(uni_Ls = nchar(University)) %>%
#  arrange(lon, lat, uni_Ls) %>%
#  group_by(lon, lat) %>%
#  mutate(n_to_all = 1:n()) %>%
#  filter(!(lon %in% ud_med$lon & lat %in% ud_med$lat))

#search_name <- group_by(lon_lat, lon, lat) %>%
#  slice(1) %>%
#  mutate(University = sub("[Uu]niv\\ ", "University\\ ", University),
#         University = sub("^[Tt]he\\ ?", "", University),
#         University = sub("^[Aa]nd\\ ?", "", University)) %>%
#  arrange(University) %>%
#  rename(Search = University)
  

#lon_lat <- left_join(
#  lon_lat, 
#  search_name,
#  by = c("lon", "lat")) %>%
#  select(-starts_with("uni_Ls"), -starts_with("n_to_all"))

#save(lon_lat, file = "C:/Kurser/Data_medline/lon_lat.Rdata")

load("C:/Kurser/Data_medline/lon_lat.Rdata")

affiliations3 <- left_join(affiliations2, lon_lat, by= "University") %>%
  filter(!is.na(Search))

final <- bind_rows(aff1.3, aff2.3, aff3.3, aff4.3, aff5.3, aff6.3, affiliations3)
library(readr)
write_csv(final, "final_data_for_app.csv")
