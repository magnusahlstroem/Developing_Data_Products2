library(shiny)
library(readr)
library(dplyr)
library(leaflet)

uni <- read_csv("final_data_for_app.csv") %>%
    arrange(Search) %>%
    filter(Search != "University Hospital")
Search_opts <- unique(uni$Search)

shinyServer(function(input, output) {
    
    output_frame <- reactive({
        if(input$Uni != "") {
            u <- filter(uni, Search == input$Uni 
                   & Published >= input$from 
                   & Published <= input$to) %>% 
                group_by(Search) %>%
                summarise(n = n(), lon = lon[1], lat = lat[1]) %>%
                arrange(desc(n)) %>%
                slice(1:input$no)
            
        } else {
            u <- filter(uni, Published >= input$from 
                   & Published <= input$to) %>%
            group_by(Search) %>%
                summarise(n = n(), 
                          lon = lon[1], 
                          lat = lat[1]) %>%
                arrange(desc(n)) %>%
                slice(1:input$no)
        }
        
        art <- lapply(u$Search, function(x) {
            pubs <- filter(uni, Search == x) %>% arrange(desc(Published)) %>% slice(1:5) %>% pull(PMID)
            aslink <- lapply(pubs, function(t) paste("<a href='https://www.ncbi.nlm.nih.gov/pubmed/", t, "'>", t, "</a>", sep = ""))
            data.frame(Search = x, 
                       link = paste0(x, " <br> 5 most recenst articles <br>", do.call(paste, c(aslink, sep = " <br> "))), 
                       stringsAsFactors = F)
        })
        art_df <- do.call(bind_rows, art)
        left_join(u, art_df, by = "Search")
    })
   

    
  output$text <- renderText({

    if(input$Uni != "") {
        paste(input$Uni, 
              "had", pull(output_frame(), n), "publication",
              "from", input$from, "to", input$to)  
    } else {
        ""
    }
     
  })
  
  output$mapPlot <- renderLeaflet({
      
      leaflet(data = output_frame()) %>% 
      addTiles() %>%
      addMarkers(lng = ~lon, 
                 lat = ~lat, 
                 popup = ~link)
                 #icon = ~quakeIcons[group],
                 #clusterOptions = markerClusterOptions())
  })
  
  output$table <- renderTable({
      if(input$Uni == "") {
          out <- select(output_frame(), -link, -lon, -lat) %>%
              rename(University = Search, Publications = n)
      } else {
          out <- NULL
      }
  
      #, 
      #popup = for_pop_ups, 
      #icon = ~quakeIcons[group],
      #clusterOptions = markerClusterOptions())
  })
  
  
})
