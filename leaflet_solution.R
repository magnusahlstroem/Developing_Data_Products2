library(leaflet);
library(dplyr);
library(ggmap)
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


#parsed <- medline("C:/Users/mgah/Downloads/pubmed_result (1).txt")
#parsed <- medline("C:/Users/mras0142/Documents/GitHub/Developing_Data_Products/Data/pubmed_result_(20180701_20180801).txt")
#save(parsed, file = "Data/Parsed_medline_records.Rdate")
load("Data/Parsed_medline_records.Rdate")

affiliations <- lapply(parsed, function(x) {
    fa_aff <- if (!is.null(x$AD)) strsplit(x$AD, ";")[[1]][1] else NA
    Dates <- if (!is.null(x$PHST)) x$PHST else "NA"
    ri <- grepl("recieved", strsplit(Dates, ";")[[1]])
    ai <- grepl("accepted", strsplit(Dates, ";")[[1]])
    Date_received <- substr(strsplit(Dates, ";")[[1]][ri], 1, 10)
    Date_accepted <- substr(strsplit(Dates, ";")[[1]][ai], 1, 10)
    Date_received <- if(identical(Date_received, character(0))) NA else Date_received
    Date_accepted <- if(identical(Date_accepted, character(0))) NA else Date_accepted
    index_uni <- regexpr("\\,[[:alpha:][:space:]]*[Uu][Nn][Ii][[:alpha:][:space:]]*\\,", fa_aff)
    uni <- sub("^\\,\\ ", "", substr(fa_aff, index_uni, index_uni[1] + attributes(index_uni)[[1]]))
    uni_cleansed <- trimws(sub("\\,\\ $", "", sub("^\\,\\ *", "", uni)))
    list(University = uni_cleansed, PMID = x$PMID, Date_received = Date_received, Date_accepted = Date_accepted)
})

affiliations2 <- do.call(rbind.data.frame, c(affiliations, stringsAsFactors = F)) %>%
    #mutate(University = as.character(University)) %>%
    filter(University != "" & !is.na(University))

#unique_uni_names_2500 <- names(table(affiliations2$University))[order(table(affiliations2$University), decreasing = T)][1:2500]
#unique_uni_names <- names(table(affiliations2$University))[order(table(affiliations2$University), decreasing = T)]
#lon_lat_uni_google <- geocode(unique_uni_names_2500, source = "google")

#lon_lat_uni_dsk <- geocode(unique_uni_names_2500, source = "dsk")
#save(lon_lat_uni_dsk, file = "C:/Users/mras0142/Documents/GitHub/Developing_Data_Products/homework1/lon_lat.Rdata")
#save(lon_lat2, file = "C:/Users/mras0142/Documents/GitHub/Developing_Data_Products/homework1/lon_lat2.Rdata")
load("C:/Users/mras0142/Documents/GitHub/Developing_Data_Products/homework1/lon_lat.Rdata")

uni_coord_merged <- cbind.data.frame(unique_uni_names_2500, lon_lat_uni_dsk) %>%
    filter(!is.na(lon)) %>%
    filter(lon != -101.8921 & lat != 33.58928) %>%
    mutate(University = unique_uni_names_2500) %>%
    select(-unique_uni_names_2500)

articles_by_uni <- left_join(affiliations2, uni_coord_merged, by = "University") %>%
    filter(!is.na(lon)) %>%
    mutate(Date = as.Date(substr(Date_accepted, 1, 10), format = "%Y/%m/%d")) %>%
    arrange(University, desc(Date_accepted))
    

summarised <- articles_by_uni %>%
    group_by(University, lon, lat) %>% 
    summarise(n = n()) %>%
    mutate(group = cut(n, breaks = c(0,10,25,100,200,Inf), 
                       labels = c("green", "light_green", "yellow", "orange", "red"))) %>%
    arrange(desc(n))


#affiliations2 <- tbl_df(left_join(affiliations2, affiliations3, by = "University")) %>% arrange(University)
#lon_lat <- geocode(affiliations3$University)
#affiliations2 <- affiliations[!(affiliations == "" | is.na(affiliations))]
#topUnis <- names(table(affiliations2))[order(table(affiliations2), decreasing = T)]
#lon_lat <- geocode(topUnis[1:1000], source = "google")
#lon_lat2 <- data.frame(University = topUnis[1:1000], lon = lon_lat[,1], lat = lon_lat[,2])
#lon_lat2 <- lon_lat2[complete.cases(lon_lat2),] #%>%
#group_by(lon, lat) %>%
#summarize(University = University[1], n = n())
#dupl_lon_lon
#lapply(dupl_lon_lon, function(x)
#    filter(lon_lat2, lon == lon_lat2$lon[x] & lat == lon_lat2$lat[x])
#)
#save(lon_lat2, file = "C:/Kurser/Developing_Data_Products/homework1/lon_lat.Rdata")
#load("C:/Kurser/Developing_Data_Products/homework1/lon_lat.Rdata")

articles_to_display <- left_join(select(articles_by_uni, University, PMID, Date), summarised, by = "University") %>%
    filter(!is.na(lon)) %>%
    group_by(University) %>%
    slice(1:5)

art_display_string <- lapply(unique(articles_to_display$University), function(x) {
    pubs <- filter(articles_to_display, University == x) %>% pull(PMID)
    as_a_link <- lapply(pubs, function(t) paste("<a href='https://www.ncbi.nlm.nih.gov/pubmed/", t, "'>", t, "</a>", sep = ""))
    data.frame(University = x, link = do.call(paste, c(as_a_link, sep = " <br> ")))
})

art_display_string_df <- do.call(rbind.data.frame, art_display_string)
final_for_display <- left_join(summarised, art_display_string_df, by = "University")
quakeIcons <- iconList(green = makeIcon("C:/Users/mras0142/Documents/GitHub/Developing_Data_Products/Map_markers/map-marker-green.png", iconWidth = 32, iconHeight =32, iconAnchorX = 16, iconAnchorY = 32),
                       light_green = makeIcon("C:/Users/mras0142/Documents/GitHub/Developing_Data_Products/Map_markers/map-marker-light-green.png", iconWidth = 32, iconHeight =32, iconAnchorX = 16, iconAnchorY = 32),
                       yellow = makeIcon("C:/Users/mras0142/Documents/GitHub/Developing_Data_Products/Map_markers/map-marker-yellow.png", iconWidth = 32, iconHeight =32, iconAnchorX = 16, iconAnchorY = 32),
                       orange = makeIcon("C:/Users/mras0142/Documents/GitHub/Developing_Data_Products/Map_markers/map-marker-orange.png", iconWidth = 32, iconHeight =32, iconAnchorX = 16, iconAnchorY = 32),
                       red = makeIcon("C:/Users/mras0142/Documents/GitHub/Developing_Data_Products/Map_markers/map-marker-red.png", iconWidth = 32, iconHeight =32, iconAnchorX = 16, iconAnchorY = 32))
for_pop_ups <- paste0(final_for_display $University, 
                      "<br> Publications ", 
                      final_for_display$n,
                      "<br> First 5 publications <br>",
                      final_for_display$link
)
leaflet(data = final_for_display) %>% 
    addTiles() %>%
    addMarkers(lng = ~lon, 
               lat = ~lat, 
               popup = for_pop_ups, 
               icon = ~quakeIcons[group],
               clusterOptions = markerClusterOptions())