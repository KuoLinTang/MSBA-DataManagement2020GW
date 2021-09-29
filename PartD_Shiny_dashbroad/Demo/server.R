#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)  # tidyverse provides data manipulation functions and pipeline
library(shiny)  # shiny makes it easy to build interactive web applications with R
library(ggplot2)  # Makes it easy to plot
library(plotly)   # Plotly creates interactive plots
library(ggpubr)   # ggpubr provides ggarrange function that can arrange multiple ggplots on the same page
# install.packages("ggpubr")


server = function(input, output, session){
  data_no_na_geo = data %>%
    filter(!is.na(Longitude))
  
  # Tab 1: select a city
  observeEvent(input$City,{
    updateSelectInput(session,'BusinessType',
                      choices=c("Select a business type", data_no_na_geo %>%
                                  filter(LocalAuthorityName == input$City) %>%
                                  distinct(BusinessType) %>%
                                  arrange(BusinessType)))
  })
  
  # If the size of selection is 1, updateSelectInput will use the column name as the choice ("https://stackoverflow.com/questions/43098849/display-only-one-value-in-selectinput-in-r-shiny-app"), so I create another dataframe and change its column name to the selection to solve this problem
  temp = reactive({
    df = data_no_na_geo
    if (nrow(df %>% 
             filter(LocalAuthorityName == input$City) %>% 
             filter(BusinessType == input$BusinessType) %>%
             distinct(RatingValue)) == 1){
      temp_name = df %>% 
        filter(LocalAuthorityName == input$City) %>% 
        filter(BusinessType == input$BusinessType) %>%
        select(RatingValue) %>%
        distinct(RatingValue) %>%
        as.vector()
      colnames(df) = c(colnames(df)[1:7], temp_name, colnames(df)[9:22]) 
      return(df)
    }
    else{
      return(df)
    }
  })
  
  # Tab 1: Select a business type
  observeEvent(input$BusinessType,{
    updateSelectInput(session,'RatingValue',
                      choices=c("Select a rating value",  
                                temp() %>%
                                  filter(LocalAuthorityName == input$City &
                                           BusinessType == input$BusinessType) %>%
                                  select(colnames(temp())[8]) %>%
                                  distinct() %>%
                                  arrange() %>%
                                  as.vector()
                      )
    )
  })
  
  # Get data from a specified city and a particular type of business
  filteredData = reactive({
    df = data_no_na_geo %>% 
      filter(LocalAuthorityName == input$City) %>% 
      filter(BusinessType == input$BusinessType) %>% 
      filter(RatingValue == input$RatingValue)
    return(df)    
  })
  
  # Plot the whole map
  output$mymap = renderLeaflet({
    leaflet(
      options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {L.control.zoom({position: 'topright'}).addTo(this)}") %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      setView(lng = -4.53067, lat = 54.22864, zoom = 5)
  })
  
  # Add markers to the map
  observe({
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'white',
      library = 'ion',
      markerColor = "red"
    )
    mymap_proxy <- leafletProxy("mymap", data = filteredData()) %>% 
      clearMarkers() %>% 
      addAwesomeMarkers(filteredData()$Longitude, 
                        filteredData()$Latitude, 
                        icon = icons, 
                        popup = paste("<b>",filteredData()$BusinessName,"</b>", "<br>",
                                      "<b>","Postcode:","</b>", filteredData()$PostCode, "<br>",
                                      "<b>","Address:","</b>", filteredData()$AddressLine, "<br>",
                                      "<b>","Food hygiene ratings:","</b>",filteredData()$RatingValue, "<br>",
                                      "<b>","Rating Scheme:","</b>",filteredData()$SchemeType, "<br>",
                                      "<b>","Hygiene Score:","</b>",filteredData()$Hygiene, "<br>",
                                      "<b>","Structural Score:","</b>",filteredData()$Structural, "<br>",
                                      "<b>","Management Score:","</b>",filteredData()$ConfidenceInManagement) %>% 
                          lapply(htmltools::HTML)) %>% 
      flyToBounds(lng1 = max(filteredData()$Longitude), lng2 = min(filteredData()$Longitude),
                  lat1 = max(filteredData()$Latitude), lat2 = min(filteredData()$Latitude))
    
  })
  
  
  ## Tab 2: Plots
  # Plot1 - Scheme Type share
  output$myplot2 = renderPlotly({
    a = ggplot(data = data, aes(x = SchemeType)) + 
      geom_bar() +
      theme(axis.text.x = element_text(face="bold", color="#993333", size=14), 
            axis.text.y = element_text(face="bold", color="#993333", size=14),
            axis.title.x = element_text(face="bold", color="black", size=14),
            axis.title.y = element_text(face="bold", color="black", size=14)) +
      labs(x = "Rating Scheme Type", y = "Total Number") + 
      scale_y_continuous(breaks = c(0, 250000, 500000), labels = c("0", "250K", "500K"))
    ggplotly(a)
  })
  
  # Plot2 - New Rating Pending share
  output$myplot4 = renderPlotly({
    a = ggplot(data = data, aes(x = NewRatingPending)) + 
      geom_bar() +
      theme(axis.text.x = element_text(face="bold", color="#993333", size=14), 
            axis.text.y = element_text(face="bold", color="#993333", size=14),
            axis.title.x = element_text(face="bold", color="black", size=14),
            axis.title.y = element_text(face="bold", color="black", size=14)) +
      labs(x = "New Rating Pending", y = "Total Number") + 
      scale_y_continuous(breaks = c(0, 200000, 400000, 600000), labels = c("0", "200K", "400K", "600K"))
    ggplotly(a)
  })
  
  
  # Remove data with missing scores
  data_no_missing_score = data %>%
    filter(!(is.na(Hygiene) & is.na(Structural) & is.na(ConfidenceInManagement)))
  
  output$myplot5 = renderPlotly({
    a = ggplot(data = data.frame(Scores = c("NA", 
                                            "Non-NA"),
                                 Count = c(nrow(data)-nrow(data_no_missing_score), 
                                           nrow(data_no_missing_score))),
               aes(x = Scores, y = Count)) +
      geom_bar(stat = "identity") + 
      theme(axis.text.x = element_text(face="bold", color="#993333", size=14), 
            axis.text.y = element_text(face="bold", color="#993333", size=14),
            axis.title.x = element_text(face="bold", color="black", size=14),
            axis.title.y = element_text(face="bold", color="black", size=14)) +
      labs(x = "Scores", y = "Total Number") + 
      scale_y_continuous(breaks = c(0, 200000, 400000, 600000), labels = c("0", "200K", "400K", "600K"))
    ggplotly(a)
  })
  
  
  ## Tab 3: Controlled plot
  # Plot1 - the bar chart for rating values in each city
  # Select a scheme type
  observeEvent(input$SchemeForPlot,{
    updateSelectInput(session,'CityForPlot',
                      choices=c(data %>%
                                  filter(SchemeType == substr(input$SchemeForPlot, start = 1, stop = 4)) %>%
                                  arrange(LocalAuthorityName) %>%
                                  distinct(LocalAuthorityName)))})
  
  # Get data from a particular city and a specified type of scheme
  filteredData2 = reactive({
    df = data %>%
      filter(SchemeType == substr(input$SchemeForPlot, start = 1, stop = 4)) %>%
      filter(LocalAuthorityName == input$CityForPlot)
    return(df)
  })
  
  # Plot the bar chart
  output$myplot1 = renderPlot({
    if (input$SchemeForPlot == "Overall"){
      g1 = ggplot(data = data %>% filter(SchemeType == "FHIS"), 
                  aes(x = RatingValue)) + 
        geom_bar(fill = "cadetblue1", col = "black") +
        geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
        ggtitle("FHIS") +
        theme(axis.text.x = element_text(face="bold", color="#993333", size=9), 
              axis.text.y = element_text(face="bold", color="#993333", size=12),
              axis.title.x = element_text(face="bold", color="black", size=14),
              axis.title.y = element_text(face="bold", color="black", size=14),
              plot.title = element_text(size = 22, face = "bold")) +
        labs(x = "Rating Value", y = "Total Number")
      g2 = ggplot(data = data %>% filter(SchemeType == "FHRS"), aes(x = RatingValue)) +
        geom_bar(fill = "cadetblue1", col = "black") +
        geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
        ggtitle("FHRS") +
        theme(axis.text.x = element_text(face="bold", color="#993333", size=9), 
              axis.text.y = element_text(face="bold", color="#993333", size=12),
              axis.title.x = element_text(face="bold", color="black", size=14),
              axis.title.y = element_text(face="bold", color="black", size=14),
              plot.title = element_text(size = 22, face = "bold")) +
        labs(x = "Rating Value", y = "Total Number")
      ggpubr::ggarrange(g1, g2, nrow = 2)
    }
    else{
      ggplot(data = filteredData2(), aes(x = RatingValue)) + 
        geom_bar(fill = "cadetblue1", col = "black") +
        geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
        ggtitle(paste0("Rating Value distribution in ", 
                       input$CityForPlot)) +
        theme(axis.text.x = element_text(face="bold", color="#993333", size=12), 
              axis.text.y = element_text(face="bold", color="#993333", size=12),
              axis.title.x = element_text(face="bold", color="black", size=14),
              axis.title.y = element_text(face="bold", color="black", size=14),
              plot.title = element_text(size = 18, face = "bold")) +
        labs(x = "Rating Value", y = "Total Number")
    }
  })
  
  # Plot2 - bar chart for business type share
  # Select a city
  data_city = reactive({
    df = data %>%
      filter(LocalAuthorityName == input$CityForPlot2) %>%
      group_by(BusinessType) %>%
      summarise(Count = n()) %>%
      mutate(Share = paste0((round(Count/sum(Count) * 100, 1)), "%"))
    return(df)
  })
  
  data_full_bar = data %>%
    group_by(BusinessType) %>%
    summarise(Count = n()) %>%
    mutate(Share = paste0((round(Count/sum(Count) * 100, 1)), "%"))
  
  
  # Plot a bar chart
  output$myplot3 = renderPlot({
    if (input$CityForPlot2 == "Overall"){
      ggplot(data = data_full_bar, aes(x = reorder(BusinessType, Count), y = Count, fill = Count)) +
        geom_bar(stat = "identity") + coord_flip() +
      scale_fill_gradient(low = "grey", high = "navy") +
        labs(x = "Total Number", y = "Business Type") +
        geom_text(aes(label = Share, hjust = -0.05), size = 4, col = "grey21")+
        ggtitle(paste0("Overall Business Type distribution")) +
        # scale_fill_brewer("Blues") +
        theme_minimal() +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size=22, face="bold"),
          legend.position = "none"
        ) +
        theme(axis.text.x=element_blank())
    }
    else{
      ggplot(data = data_city(), aes(x = reorder(BusinessType, Count), y = Count, fill = Count)) +
        geom_bar(stat = "identity") + coord_flip() +
        scale_fill_gradient(low = "grey", high = "navy") +
        labs(x = "Total Number", y = "Business Type") +
        geom_text(aes(label = Share, hjust = -0.05), size = 4, col = "grey21")+
        ggtitle(paste0("Business Type distribution in ", input$CityForPlot2)) +
        # scale_fill_brewer("Blues") +
        theme_minimal() +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size=22, face="bold"),
          legend.position = "none"
        ) +
        theme(axis.text.x=element_blank())
    }
  })
  
  # Plot3 - score distribution
  
  filteredData3 = reactive({
    df = data.frame(Score = as.integer(data_no_missing_score[, input$ScorePlot])) %>%
      arrange(Score)
    return(df)
  })
  output$myplot6 = renderPlotly({
    score.plot = ggplot(data = filteredData3(), aes(x = Score)) +
      geom_bar(fill = "cadetblue1", col = "black") +
      ggtitle(paste0(input$ScorePlot, " Score Distribution")) + 
      theme(axis.text.x = element_text(face="bold", color="#993333", size=12), 
            axis.text.y = element_text(face="bold", color="#993333", size=12),
            axis.title.x = element_text(face="bold", color="black", size=14),
            axis.title.y = element_text(face="bold", color="black", size=14),
            plot.title = element_text(size = 20, face = "bold")) + 
      labs(y = "Total Number") + 
      scale_y_continuous(breaks = c(0, 50000, 100000, 150000, 200000), labels = c("0", "50K", "100K", "150K", "200K"))
    ggplotly(score.plot)
  })
  
  
  
  # Display our data
  output$mydata = DT::renderDataTable(
    DT::datatable(data)
  )
}
