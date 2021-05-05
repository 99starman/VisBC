library(shiny)
library(tidyverse)
library(rgdal)
library(dygraphs)
library(xts)
library(leaflet)


url <- "http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv"
data <- read.csv(url)
map <- readOGR("./data/health_area/HA_2018.shp")

# projection
PRO <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
map <- sp::spTransform(map,PRO)

# preprocess
data <- rename(data, cases = Cases_Reported, 
                cases_smooth = Cases_Reported_Smoothed)

region_data <- data %>%
               filter(HA != "All" & HA != "Out of Canada" & HSDA == "All")
total_data <- data %>%
               filter(HA == "All" & HSDA == "All")
currDate <- dplyr::last(total_data$Date)


# ui
ui <- fluidPage(
  titlePanel(p("VisBC For Covid-19", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("date",
                  "Date:",
                  min = as.Date("2020-01-29","%Y-%m-%d"),
                  max = as.Date(currDate),
                  value=as.Date("2020-01-29"),
                  timeFormat="%Y-%m-%d"),
      p("Made with", a("Shiny", href = "http://shiny.rstudio.com"), "."),
      img(
        src = "imageShiny.png",
        width = "50px", height = "50px"
      )
    ),
    mainPanel(
      leafletOutput(outputId = "map"),
      fluidRow(dygraphOutput(outputId = "timetrend", height=400, width=600),
               style = "padding-top:50px")
    )
  )
)


# server
server <- function(input, output) {

  output$timetrend <- renderDygraph({
    dataxts <- NULL
    area_list <- unique(region_data$HA)

    for (l in 1:length(area_list)) {
      data_area <- region_data[region_data$HA == area_list[l], ]
      dd <- xts(
        x = data_area[, "cases_smooth"],
        order.by = as.Date(data_area$Date)
      )
      dataxts <- cbind(dataxts, dd)
    }
    colnames(dataxts) <- area_list

    dygraph(dataxts, main = "Smoothed cases (avg cases of a week) across health areas") %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1

    d1$x$css <- "
 .dygraph-legend > span {display:none;}
 .dygraph-legend > span.highlight { display: inline; }
 "
    d1
  })

  output$map <- renderLeaflet({
    
    # Add data to map
    dateSelected <- input$date
    datafiltered <- region_data[which(as.Date(region_data$Date) == dateSelected), ]
    datafiltered$HA = factor(datafiltered$HA)
    colnames(datafiltered)[colnames(datafiltered) == 'HA'] <- 'HA_Name'
    map@data <- left_join(map@data, datafiltered)

    
    # Create leaflet
    pal <- colorBin("YlOrRd", domain = map$cases, bins = 5)
    
    labels <- sprintf("<strong>%s</strong>: %g", map$HA_Name, map$cases) %>%
      lapply(htmltools::HTML)
    
    l <- leaflet(map) %>%
        addTiles() %>%
      addPolygons(
        fillColor = ~ pal(cases),
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~region_data$cases,
        opacity = 0.7, title = NULL
      )
  })
}

# shinyApp()
shinyApp(ui = ui, server = server)