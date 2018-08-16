load("C:/Kurser/Data_medline/parsed_2018_1.Rdata")

aff1 <- lapply(parsed_2018_1, function(x) {
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

aff1.2 <- do.call(rbind.data.frame, c(aff1, stringsAsFactors = F)) %>%
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

aff1.3 <- left_join(aff1.2, lon_lat, by= "University") %>%
  filter(!is.na(Search))

rm(parsed_2018_1)

load("C:/Kurser/Data_medline/parsed_2018_2.Rdata")

aff2 <- lapply(parsed_2018_2, function(x) {
  fa_aff <- if (!is.null(x$AD)) strsplit(x$AD, ";")[[1]][1] else "NA"
  fa_aff <- gsub("\\-", "\\ ", fa_aff)
  fa_aff <- gsub("&", "and", fa_aff)
  fa_aff <- gsub("(?!,)[[:punct:]]", " ", fa_aff, perl=TRUE)
  Dates <- if (!is.null(x$PHST)) x$PHST else "NA"
  ri <- grepl("recieved", strsplit(Dates, ";")[[1]])
  ai <- grepl("accepted", strsplit(Dates, ";")[[1]])
  Date_received <- substr(strsplit(Dates, ";")[[1]][ri], 1, 10)[1]
  Date_accepted <- substr(strsplit(Dates, ";")[[1]][ai], 1, 10)[1]
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


aff2.2 <- do.call(rbind.data.frame, c(aff2, stringsAsFactors = F)) %>%
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

aff2.3 <- left_join(aff2.2, lon_lat, by= "University") %>%
  filter(!is.na(Search))

rm(parsed_2018_2)


load("C:/Kurser/Data_medline/parsed_2018_3.Rdata")

aff3 <- lapply(parsed_2018_3, function(x) {
  fa_aff <- if (!is.null(x$AD)) strsplit(x$AD, ";")[[1]][1] else "NA"
  fa_aff <- gsub("\\-", "\\ ", fa_aff)
  fa_aff <- gsub("&", "and", fa_aff)
  fa_aff <- gsub("(?!,)[[:punct:]]", " ", fa_aff, perl=TRUE)
  Dates <- if (!is.null(x$PHST)) x$PHST else "NA"
  ri <- grepl("recieved", strsplit(Dates, ";")[[1]])
  ai <- grepl("accepted", strsplit(Dates, ";")[[1]])
  Date_received <- substr(strsplit(Dates, ";")[[1]][ri], 1, 10)[1]
  Date_accepted <- substr(strsplit(Dates, ";")[[1]][ai], 1, 10)[1]
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

aff3.2 <- do.call(rbind.data.frame, c(aff3, stringsAsFactors = F)) %>%
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

aff3.3 <- left_join(aff3.2, lon_lat, by= "University") %>%
  filter(!is.na(Search))

rm(parsed_2018_3)

load("C:/Kurser/Data_medline/parsed_2018_4.Rdata")

aff4 <- lapply(parsed_2018_4, function(x) {
  fa_aff <- if (!is.null(x$AD)) strsplit(x$AD, ";")[[1]][1] else "NA"
  fa_aff <- gsub("\\-", "\\ ", fa_aff)
  fa_aff <- gsub("&", "and", fa_aff)
  fa_aff <- gsub("(?!,)[[:punct:]]", " ", fa_aff, perl=TRUE)
  Dates <- if (!is.null(x$PHST)) x$PHST else "NA"
  ri <- grepl("recieved", strsplit(Dates, ";")[[1]])
  ai <- grepl("accepted", strsplit(Dates, ";")[[1]])
  Date_received <- substr(strsplit(Dates, ";")[[1]][ri], 1, 10)[1]
  Date_accepted <- substr(strsplit(Dates, ";")[[1]][ai], 1, 10)[1]
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

aff4.2 <- do.call(rbind.data.frame, c(aff4, stringsAsFactors = F)) %>%
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

aff4.3 <- left_join(aff4.2, lon_lat, by= "University") %>%
  filter(!is.na(Search))

rm(parsed_2018_4)

load("C:/Kurser/Data_medline/parsed_2018_5.Rdata")

aff5 <- lapply(parsed_2018_5, function(x) {
  fa_aff <- if (!is.null(x$AD)) strsplit(x$AD, ";")[[1]][1] else "NA"
  fa_aff <- gsub("\\-", "\\ ", fa_aff)
  fa_aff <- gsub("&", "and", fa_aff)
  fa_aff <- gsub("(?!,)[[:punct:]]", " ", fa_aff, perl=TRUE)
  Dates <- if (!is.null(x$PHST)) x$PHST else "NA"
  ri <- grepl("recieved", strsplit(Dates, ";")[[1]])
  ai <- grepl("accepted", strsplit(Dates, ";")[[1]])
  Date_received <- substr(strsplit(Dates, ";")[[1]][ri], 1, 10)[1]
  Date_accepted <- substr(strsplit(Dates, ";")[[1]][ai], 1, 10)[1]
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

aff5.2 <- do.call(rbind.data.frame, c(aff5, stringsAsFactors = F)) %>%
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

aff5.3 <- left_join(aff5.2, lon_lat, by= "University") %>%
  filter(!is.na(Search))

rm(parsed_2018_5)

load("C:/Kurser/Data_medline/parsed_2018_6.Rdata")

aff6 <- lapply(parsed_2018_6, function(x) {
  fa_aff <- if (!is.null(x$AD)) strsplit(x$AD, ";")[[1]][1] else "NA"
  fa_aff <- gsub("\\-", "\\ ", fa_aff)
  fa_aff <- gsub("&", "and", fa_aff)
  fa_aff <- gsub("(?!,)[[:punct:]]", " ", fa_aff, perl=TRUE)
  Dates <- if (!is.null(x$PHST)) x$PHST else "NA"
  ri <- grepl("recieved", strsplit(Dates, ";")[[1]])
  ai <- grepl("accepted", strsplit(Dates, ";")[[1]])
  Date_received <- substr(strsplit(Dates, ";")[[1]][ri], 1, 10)[1]
  Date_accepted <- substr(strsplit(Dates, ";")[[1]][ai], 1, 10)[1]
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

aff6.2 <- do.call(rbind.data.frame, c(aff6, stringsAsFactors = F)) %>%
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

aff6.3 <- left_join(aff6.2, lon_lat, by= "University") %>%
  filter(!is.na(Search))

rm(parsed_2018_6)