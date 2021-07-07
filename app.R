#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

library(shiny)



#Housing Prices dataset
my_data <- read_excel("DataFrameNew.xlsx")
covid_col = "#cc4c02"
states <- geojson_read("gz_2010_us_040_00_500k.json", what = "sp")
states <- states[-c(17),]
vec <- c(states@data[["NAME"]])
my_data[1:51,] <- my_data[match(c(states@data[["NAME"]]), my_data$Value), ]  
years = seq(from = 1996, to = 2020, by = 1)
# Create a continuous palette function
bins = c(0,500,1000,2000,4000,8000,Inf)
pal <- colorBin("Oranges", bins = bins)
# pal <- colorNumeric(
#   palette = "Blues",
#   domain = c(0,10000))

#create housing map
basemap = leaflet(states) %>% 
  addTiles() %>% 
  # addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
  #             color = ~pal(CENSUSAREA)) %>%
   addProviderTiles(providers$CartoDB.Positron) %>%
   setView(-96.5795,40.5 ,zoom = 4)
  # %>%
  # addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$deaths_per_million,
  #           title = "<small>Deaths per million</small>") 










# Define UI for application that draws a histogram
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
            HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Monthly Mortgage Costs</a>'), id="nav",
            windowTitle = "Monthly Mortgage Costs",
            
            tabPanel("Cost by State",
                     div(class="outer",
                         tags$head(includeCSS("styles.css")),
                         leafletOutput("mymap", width="100%", height="100%"),
                         
                         absolutePanel(id = "controls", class = "panel panel-default",
                                       top = 75, left = 55, width = 250, fixed=TRUE,
                                       draggable = TRUE, height = "auto",
                                       
                                       span(tags$i(h6("Payments are calculated using median sales price and average annual interest rates")), style="color:#045a8d"),
                                       h3('Most Expensive States', align = "left"),
                                       h4(textOutput('max_states'), align = "left"),
                                       h3('Least Expensive States', align = "left"),
                                       h4(textOutput('min_states'), align = "left"),                                      
                                       
                                       sliderTextInput("plot_date",
                                                       label = h5("Select mapping date"),
                                                       choices = years,
                                                       selected = "2020",
                                                       grid = FALSE,
                                                       animate=animationOptions(interval = 3000, loop = FALSE)),
                                      numericInput('downpayment', 'Downpayment Percentage', value = 0 ,min= 0, max = 100),
                                      radioButtons(input = 'mortgage', label = 'Mortgage', choices = c('15'=15,'30'= 30), inline=TRUE),
                                      
                                       
                         ),
                     )
            )
                       
                    
                     )
            )


# Define server logic required to draw a histogram
server <- function(input, output) {
  formatted_date = reactive({
    format(as.Date(input$plot_date, format="%d %b %y"), "%Y-%m-%d")
  })
  
  get_m <- reactive({
    
  })
  #filters by a given year, then will modify the time column and return the modified dataframe
  #dataframe columns = state,year, ...modifiable params, time-to-pay
  reactive_db = reactive({
    annual_prices <- my_data %>% select(toString(input$plot_date))
    #get mean price by year
    m  = rep(0,nrow(annual_prices))
    downpayment <- .01 * input$downpayment 
    for(i in 1:length(m)){
      if(i <= 51){
        monthly_payment <- 0
        principle <- (1-downpayment) * annual_prices[[1]][i] 
        #print(principle)
        if (input$mortgage == 15){
          interest <- (annual_prices[[1]][52] * .01)/12
          #print(interest)
          monthly_payment <- principle * (interest * (1+interest)**180)/(((1+interest)**180)-1)
          #print(monthly_payment)
        }
        else if (input$mortgage == 30){
          interest <- (annual_prices[[1]][52]* .01)/12
          #print(interest)
          monthly_payment <- principle * (interest * (1+interest)**360)/(((1+interest)**360)-1)
          #print(monthly_payment)
        }
        #print(monthly_payment)
        m[i] <- monthly_payment
      }
      else{
        m[i] <- 0
      }
    }
    my_data$monthly = m
    my_data
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   reactive_polygons = reactive({
     print(my_data$monthly)
     states
   })

   #shows new data when timeline changes
   observeEvent({input$plot_date
                 input$downpayment
                 input$mortgage}, {
     
     temp <- reactive_db()[1:51,]
     output$max_states <- renderText({ 
       max_states <- temp %>% arrange(monthly) %>% tail(n=3)
       sprintf("%s, %s, %s", max_states$Value[1],max_states$Value[2],max_states$Value[3])
       })
     output$min_states <- renderText({ 
       min_states <- temp %>% arrange(monthly) %>% head(n=3)
       sprintf("%s, %s, %s", min_states$Value[1],min_states$Value[2],min_states$Value[3])
     })
     leafletProxy("mymap") %>%
       clearMarkers() %>%
       clearShapes() %>%
       addPolygons(data = reactive_polygons(), 
                   stroke = FALSE, 
                   smoothFactor = 0.1, 
                   fillOpacity = 0.4, 
                   fillColor = ~pal(temp$monthly),
                  label = sprintf("<strong>%s</strong><br/>%f/month",my_data[1:51,]$Value, temp$monthly) 
                  %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
                                                            )
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

