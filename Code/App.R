


#### 3. SHINY ###################################################################################################3

pacman::p_load(dplyr, tidyr, tibble, lubridate, zoo, ggplot2, scales, plyr, shiny, reshape2,
               sqldf, xlsx, data.table, forecast, imputeTS, timeDate, bsts, readxl, readr,shinydashboard,
               shiny, xts,
               highcharter
)


ui <- dashboardPage(
  dashboardHeader(title = "Ubiqum Energy Monitor",
                  titleWidth=400
  ),
  ## Sidebar content
  dashboardSidebar(
    
    sidebarMenu(id="menu",
                menuItem("Customer Layout", tabName = "customer", icon =icon("bar-chart-o"), startExpanded = FALSE,
                         
                         dateRangeInput('dateRange',
                                        label = (HTML("<i class='glyphicon glyphicon-calendar'></i> Select Date")),
                                        start = as_date("2007-02-01"), end = as_date("2007-12-31"),
                                        separator = "to", 
                                        weekstart = 1),
                         
                         tags$style(HTML(".datepicker {z-index:99999 !important;}")),
                         selectInput("Frequency", "Select frequency", choices=c("Year", "Month", "Week", "Day of Week", "Day", "Hour")),
                         selectInput(inputId = "Variable", label = "Variable",choices=c("ActiveEnergy", "ReactiveEnergy", "Kitchen", "Laundry", "EWAC", "OtherRooms")),
                         
                         menuSubItem("Historic", tabName = "CustBarPlot"),
                         menuSubItem("Forecasts", tabName = "CustTS")
                         
                ),
                
                
                menuItem("Analyst Layout", tabName = "analyst", icon =icon("line-chart"), startExpanded = FALSE,
                         selectInput(inputId ="FrequencyAnalyst", label="Select frequency", choices=c("Year", "Month", "Week", "Day")),
                         selectInput(inputId = "VariableAnalyst", label = "Variable",choices=c("ActiveEnergy", "ReactiveEnergy", "Kitchen", "Laundry", "EWAC", "OtherRooms")),
                         menuSubItem("Time Series", tabName = "AnTimeSeries"),
                         menuSubItem("Time Series All", tabName = "AnTimeSeriesAll"),
                         menuSubItem("Models", tabName = "AnModels")
                ),
                
                menuItem("About", tabName = "readme", icon = icon("mortar-board"), selected = TRUE)
                
                
                
    )
  ),
  
  
  
  dashboardBody(tabItems(
    # First Tab Content
    tabItem(tabName = "CustBarPlot",
            fluidRow(
              box(highchartOutput("table") ,width="12")
            )
    ),
    
    tabItem(tabName = "AnTimeSeries",
            box(plotOutput("TimeSeries", height = 500,width = 700 ))),
    
    
    tabItem(tabName = "AnTimeSeriesAll",
            box(plotOutput("TimeSeriesAll", height = 500,width = 700))),
    
    tabItem(tabName = "AnModels",
            fluidRow(
              box(plotOutput("PlotModels", height = 250)),
              
              box(
                title= "SelectModel",
                selectInput(inputId = "SelectModel", label = "Select Model",choices=c("Naive", "SNaive", "HoltWinters", "Arima"))
              )),
            
            fluidRow(
              box(tableOutput("ErrorModel")))
    )
    
  )
  
  )
)

server <- function(input, output, session) {
  
  observe({
    
    input1<-input$Frequency
    
    choice1<-if (difftime(input$dateRange[2]+1, input$dateRange[1])==(as.Date(as.yearmon(input$dateRange[1])+1) - as.Date(as.yearmon(input$dateRange[1]))))
      c("Year", "Month", "Week", "Day of Week", "Day", "Hour") else {
        
        if ((difftime(as_date(input$dateRange[2])+1,as_date(input$dateRange[1])))==day(LastDayInMonth(as_date(input$dateRange[2]))))
          c( "Month", "Week", "Day of Week", "Day", "Hour") else {
            
            if ((difftime(as_date(input$dateRange[2])+1,as_date(input$dateRange[1])))== 7)
              c("Week", "Day of Week", "Day", "Hour") else {
                
                if((difftime(as_date(input$dateRange[2])+1,as_date(input$dateRange[1])))==1)
                  
                  c("Day of Week", "Day", "Hour") else c("Day")
              }
          }
      }
    
    updateSelectInput(session,"Frequency",choices=choice1,selected=input1)
    
  })
  
  get.data <- reactive({
    switch(input$Frequency,
           "Year" = Data_ByYear,
           "Month" = Data_ByMonth,
           "Week" = Data_ByWeek,
           "Day of Week" = Data_ByWDay,
           "Day" = Data_ByDay,
           "Hour" = Data_ByHour)
  })
  
  get.ts <- reactive({
    switch(input$FrequencyAnalyst,
           "Year" = tsYear,
           "Month" = tsMonth,
           "Week" = tsWeek,
           "Day" = tsDay)
  })
  
  
  # get.variable <- reactive({
  #   switch(input$Variable,
  #          "ActiveEnergy" = ActiveEnergy,
  #          "ReactiveEnergy" = ReactiveEnergy,
  #          "Kitchen" = Kitchen,
  #          "Laundry" = Laundry,
  #          "EWAC" = EWAC,
  #          "OtherRooms" = OtherRooms)
  # })
  
  #### filtro el data set (yo le llamo df en la query de dplyr) que tu has creado con el if statement por las fechas de start y end que he seleccionado
  #en el calendario
  
  filteredData <- reactive({
    get.data() %>% filter(Date>=input$dateRange[1] & Date<input$dateRange[2])
    
  })
  
  output$table <- renderHighchart ({
    
    test<-filteredData()
    test$ActiveEnergy<-round(test$ActiveEnergy,2)
    
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Energy consumption (Watt-hour)", align="center") %>% 
      hc_xAxis(categories = test$Time) %>% 
      hc_add_series(data = test$ActiveEnergy,
                    name = "Wh")%>% 
      hc_add_theme(hc_theme_538())
    
  })
  
  output$table <- renderHighchart ({
    
    dfts <- xts(round(Data_ByDay$ActiveEnergy,2), as.Date(Data_ByDay$Date, format='%d/%m/%Y'))
    hchart(test) %>%
      hc_title(text = "Energy consumption (Watt-hour)") %>%
      hc_add_theme(hc_theme_538())
    
  })
  
  
  
  output$TimeSeries<-renderPlot({
    
    VariableTS<-as.character(input$VariableAnalyst)
    
    # Filter Variables by Year
    if(input$FrequencyAnalyst=="Year"){
      
      plot.ts(tsYear[, VariableTS],  ylab = paste0(VariableTS, "(Wh)"), xlab="Per Years")
    }
    
    # Filter Variables by Month
    if(input$FrequencyAnalyst=="Month"){
      
      plot.ts(tsMonth[, VariableTS],  ylab = paste0(VariableTS, "(Wh)"), xlab="Per Months")
    }
    
    # Filter Variables by Week
    if(input$FrequencyAnalyst=="Week"){
      
      plot.ts(tsWeek[, VariableTS],  ylab = paste0(VariableTS, "(Wh)"), xlab="Per Weeks")
    }    
    
    # Filter Variables by Day
    if(input$FrequencyAnalyst=="Day"){
      
      plot.ts(tsDay[, VariableTS],  ylab = paste0(VariableTS, "(Wh)"), xlab="Per Days")
    }})
  
  output$TimeSeriesAll<-renderPlot({
    
    # Filter Variables by Year
    if (input$FrequencyAnalyst=="Year"){
      
      autoplot(tsYear[, c(3,4,5,6,7,8)], facets = FALSE, ylab = "Energy (Wh)", xlab="Years")
      
    }
    
    # Filter Variables by Month
    else if(input$FrequencyAnalyst=="Month"){
      
      autoplot(tsMonth[, c(4,5,6,7,8,9)], facets = FALSE, ylab = "Energy (Wh)", xlab="Months")
    }
    
    # Filter Variables by Week
    else if(input$FrequencyAnalyst=="Week"){
      
      autoplot(tsWeek[, c(4,5,6,7,8,9)], facets = FALSE, ylab = "Energy (Wh)", xlab="Weeks")
    }    
    
    # Filter Variables by Day
    else if(input$FrequencyAnalyst=="Day"){
      
      autoplot(tsDay[, c(5,6,7,8,9,10)], facets = FALSE, ylab = "Energy (Wh)", xlab="Days")
    }})
  
  output$PlotModels<-renderPlot({
    
    if(input$FrequencyAnalyst=="Month"){
      
      if(input$VariableAnalyst=="ActiveEnergy"){
        
        ifelse(input$SelectModel=="Naive",  plot(ListGraphsMonhs[[1]][[1]][[1]]),
               ifelse(input$SelectModel== "SNaive",plot(ListGraphsMonhs[[1]][[2]][[1]]),
                      ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsMonhs[[1]][[3]][[1]]),
                             ifelse(input$SelectModel=="Arima",plot(ListGraphsMonhs[[1]][[4]][[1]])
                             ))))}   
      
      if(input$VariableAnalyst=="ReactiveEnergy"){
        
        ifelse(input$SelectModel=="Naive",  plot(ListGraphsMonhs[[2]][[1]][[1]]),
               ifelse(input$SelectModel== "SNaive",plot(ListGraphsMonhs[[2]][[2]][[1]]),
                      ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsMonhs[[2]][[3]][[1]]),
                             ifelse(input$SelectModel=="Arima",plot(ListGraphsMonhs[[2]][[4]][[1]])
                             ))))}         
      
      if(input$VariableAnalyst=="Kitchen"){
        
        ifelse(input$SelectModel=="Naive",  plot(ListGraphsMonhs[[3]][[1]][[1]]),
               ifelse(input$SelectModel== "SNaive",plot(ListGraphsMonhs[[3]][[2]][[1]]),
                      ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsMonhs[[3]][[3]][[1]]),
                             ifelse(input$SelectModel=="Arima",plot(ListGraphsMonhs[[3]][[4]][[1]])
                             ))))}         
      
      if(input$VariableAnalyst=="Laundry"){
        
        ifelse(input$SelectModel=="Naive",  plot(ListGraphsMonhs[[4]][[1]][[1]]),
               ifelse(input$SelectModel== "SNaive",plot(ListGraphsMonhs[[4]][[2]][[1]]),
                      ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsMonhs[[4]][[3]][[1]]),
                             ifelse(input$SelectModel=="Arima",plot(ListGraphsMonhs[[4]][[4]][[1]])
                             ))))}         
      
      if(input$VariableAnalyst=="EWAC"){
        
        ifelse(input$SelectModel=="Naive",  plot(ListGraphsMonhs[[5]][[1]][[1]]),
               ifelse(input$SelectModel== "SNaive",plot(ListGraphsMonhs[[5]][[2]][[1]]),
                      ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsMonhs[[5]][[3]][[1]]),
                             ifelse(input$SelectModel=="Arima",plot(ListGraphsMonhs[[5]][[4]][[1]])
                             ))))   
      }}
    
    if (input$FrequencyAnalyst=="Week"){
      
      if(input$VariableAnalyst=="ActiveEnergy"){
        
        ifelse(input$SelectModel=="Naive",  plot(ListGraphsWeeks[[1]][[1]][[1]]),
               ifelse(input$SelectModel== "SNaive",plot(ListGraphsWeeks[[1]][[2]][[1]]),
                      ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsWeeks[[1]][[3]][[1]]),
                             ifelse(input$SelectModel=="Arima",plot(ListGraphsWeeks[[1]][[4]][[1]])
                             ))))} 
      
      
      if(input$VariableAnalyst=="ReactiveEnergy"){
        
        ifelse(input$SelectModel=="Naive",  plot(ListGraphsWeeks[[2]][[1]][[1]]),
               ifelse(input$SelectModel== "SNaive",plot(ListGraphsWeeks[[2]][[2]][[1]]),
                      ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsWeeks[[2]][[3]][[1]]),
                             ifelse(input$SelectModel=="Arima",plot(ListGraphsWeeks[[2]][[4]][[1]])
                             ))))}         
      
      if(input$VariableAnalyst=="Kitchen"){
        
        ifelse(input$SelectModel=="Naive",  plot(ListGraphsWeeks[[3]][[1]][[1]]),
               ifelse(input$SelectModel== "SNaive",plot(ListGraphsWeeks[[3]][[2]][[1]]),
                      ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsWeeks[[3]][[3]][[1]]),
                             ifelse(input$SelectModel=="Arima",plot(ListGraphsWeeks[[3]][[4]][[1]])
                             ))))}         
      
      if(input$VariableAnalyst=="Laundry"){
        
        ifelse(input$SelectModel=="Naive",  plot(ListGraphsWeeks[[4]][[1]][[1]]),
               ifelse(input$SelectModel== "SNaive",plot(ListGraphsWeeks[[4]][[2]][[1]]),
                      ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsWeeks[[4]][[3]][[1]]),
                             ifelse(input$SelectModel=="Arima",plot(ListGraphsWeeks[[4]][[4]][[1]])
                             ))))}         
      
      if(input$VariableAnalyst=="EWAC"){
        
        ifelse(input$SelectModel=="Naive",  plot(ListGraphsWeeks[[5]][[1]][[1]]),
               ifelse(input$SelectModel== "SNaive",plot(ListGraphsWeeks[[5]][[2]][[1]]),
                      ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsWeeks[[5]][[3]][[1]]),
                             ifelse(input$SelectModel=="Arima",plot(ListGraphsWeeks[[5]][[4]][[1]])
                             ))))   
      }}
    
  })
  
  output$ErrorModel<-renderTable({
    
    if(input$FrequencyAnalyst=="Month"){
      
      if(input$VariableAnalyst=="ActiveEnergy"){
        
        ifelse(input$SelectModel=="Naive",  ListGraphsMonhs[[1]][[1]][2],
               ifelse(input$SelectModel== "SNaive",ListGraphsMonhs[[1]][[2]][2],
                      ifelse(input$SelectModel=="HoltWinters",ListGraphsMonhs[[1]][[3]][2],
                             ifelse(input$SelectModel=="Arima",ListGraphsMonhs[[1]][[4]][2]
                             ))))}
      
      else if(input$VariableAnalyst=="ReactiveEnergy"){
        
        ifelse(input$SelectModel=="Naive", ListGraphsMonhs[[2]][[1]][2],
               ifelse(input$SelectModel== "SNaive",ListGraphsMonhs[[2]][[2]][2],
                      ifelse(input$SelectModel=="HoltWinters",ListGraphsMonhs[[2]][[2]][2],
                             ifelse(input$SelectModel=="Arima",ListGraphsMonhs[[2]][[4]][2]
                             ))))}
      
      else if(input$VariableAnalyst=="Kitchen"){
        
        ifelse(input$SelectModel=="Naive", ListGraphsMonhs[[3]][[1]][2],
               ifelse(input$SelectModel== "SNaive",ListGraphsMonhs[[3]][[2]][2],
                      ifelse(input$SelectModel=="HoltWinters",ListGraphsMonhs[[3]][[2]][2],
                             ifelse(input$SelectModel=="Arima",ListGraphsMonhs[[3]][[4]][2]
                             ))))}
      
      else if(input$VariableAnalyst=="Laundry"){
        
        ifelse(input$SelectModel=="Naive", ListGraphsMonhs[[4]][[1]][2],
               ifelse(input$SelectModel== "SNaive",ListGraphsMonhs[[4]][[2]][2],
                      ifelse(input$SelectModel=="HoltWinters",ListGraphsMonhs[[4]][[2]][2],
                             ifelse(input$SelectModel=="Arima",ListGraphsMonhs[[4]][[4]][2]
                             ))))}
      
      else if(input$VariableAnalyst=="EWAC"){
        
        ifelse(input$SelectModel=="Naive", ListGraphsMonhs[[5]][[1]][2],
               ifelse(input$SelectModel== "SNaive",ListGraphsMonhs[[5]][[2]][2],
                      ifelse(input$SelectModel=="HoltWinters",ListGraphsMonhs[[5]][[2]][2],
                             ifelse(input$SelectModel=="Arima",ListGraphsMonhs[[5]][[4]][2]
                             ))))}
    }
    
    else if(input$FrequencyAnalyst=="Week"){
      
      if(input$VariableAnalyst=="ActiveEnergy"){
        
        ifelse(input$SelectModel=="Naive",  ListGraphsWeeks[[1]][[1]][2],
               ifelse(input$SelectModel== "SNaive",ListGraphsWeeks[[1]][[2]][2],
                      ifelse(input$SelectModel=="HoltWinters",ListGraphsWeeks[[1]][[3]][2],
                             ifelse(input$SelectModel=="Arima",ListGraphsWeeks[[1]][[4]][2]
                             ))))}
      
      else if(input$VariableAnalyst=="ReactiveEnergy"){
        
        ifelse(input$SelectModel=="Naive", ListGraphsWeeks[[2]][[1]][2],
               ifelse(input$SelectModel== "SNaive",ListGraphsWeeks[[2]][[2]][2],
                      ifelse(input$SelectModel=="HoltWinters",ListGraphsWeeks[[2]][[2]][2],
                             ifelse(input$SelectModel=="Arima",ListGraphsWeeks[[2]][[4]][2]
                             ))))}
      
      else if(input$VariableAnalyst=="Kitchen"){
        
        ifelse(input$SelectModel=="Naive", ListGraphsWeeks[[3]][[1]][2],
               ifelse(input$SelectModel== "SNaive",ListGraphsWeeks[[3]][[2]][2],
                      ifelse(input$SelectModel=="HoltWinters",ListGraphsWeeks[[3]][[2]][2],
                             ifelse(input$SelectModel=="Arima",ListGraphsWeeks[[3]][[4]][2]
                             ))))}
      
      else if(input$VariableAnalyst=="Laundry"){
        
        ifelse(input$SelectModel=="Naive", ListGraphsWeeks[[4]][[1]][2],
               ifelse(input$SelectModel== "SNaive",ListGraphsWeeks[[4]][[2]][2],
                      ifelse(input$SelectModel=="HoltWinters",ListGraphsWeeks[[4]][[2]][2],
                             ifelse(input$SelectModel=="Arima",ListGraphsWeeks[[4]][[4]][2]
                             ))))}
      
      else if(input$VariableAnalyst=="EWAC"){
        
        ifelse(input$SelectModel=="Naive", ListGraphsWeeks[[5]][[1]][2],
               ifelse(input$SelectModel== "SNaive",ListGraphsWeeks[[5]][[2]][2],
                      ifelse(input$SelectModel=="HoltWinters",ListGraphsWeeks[[5]][[2]][2],
                             ifelse(input$SelectModel=="Arima",ListGraphsWeeks[[5]][[4]][2]
                             ))))}
      
    }
  })
  
}

shinyApp(ui, server)