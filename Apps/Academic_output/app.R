library(shiny)
library(readr)
library(dplyr)
library(leaflet)

uni <- read_csv("final_data_for_app.csv") %>%
    arrange(Search) %>%
    filter(Search != "University Hospital")
Search_opts <- unique(uni$Search)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("Academic output of universities"),
    #h3("Based on first Author of publications"),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #submitButton("Done"),
            h4("How to use the app"),
            p("Enter a start and end date and/or a university and see how many 
              publications the university published in the chosen period of time, 
              also you can find a link to the 5 most recent puclications at the 
              markers. You can also manually search the map and find your 
              university of choice. Finally if you dont enter a university
              you can specify how many universities should be displayed.
              The universities are ranked according to number of publications 
              during the specified time period."),
            selectizeInput(
                'Uni', 'University', choices = Search_opts,
                selected = "University of Copenhagen",
                options = list(
                    placeholder = 'Please select a University',
                    onInitialize = I('function() { this.setValue(""); }')
                )
            ),
            dateInput("from", "Start date", 
                      value = "2018-04-01", 
                      min = "2018-01-01", 
                      max = "2018-07-31",
                      format = "yyyy-mm-dd"),
            dateInput("to", "End date", 
                      value = "2018-04-15", 
                      min = "2018-01-01", 
                      max = "2018-07-31",
                      format = "yyyy-mm-dd"),
            numericInput("no", "How many Universities should be displayed", 
                         value = 10, min = 1, max = length(Search_opts))
            ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h3("Documentation"), 
            p("The app is based on Author affiliations from the medline database, 
              specifically first author affiliations. From the author affiliation
              the name of affiliated university was extracted (if available). 
              Coordinates of universities were extracted with the ggmap R package:"),
            helpText(   
                a("Link to ggmap package article", 
                  href="http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf")
            ),
            
            leafletOutput("mapPlot"),
            br(),
            tableOutput("table"),
            div(textOutput("text"), style = "color:red; font-size:125%")
            )
    )
))

server <- shinyServer(function(input, output) {
    
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

shinyApp(ui = ui, server = server)