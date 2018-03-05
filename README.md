# myrep


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if (!require("shiny")) install.packages("shiny")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("shiny")
if (!require("stringr")) install.packages("stringr")
if (!require("DT")) install.packages("DT")
if (!require("ggmap")) install.packages("ggmap")
if (!require("data.table")) install.packages("data.table")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("leaflet")) install.packages("leaflet")

# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(
    theme=shinytheme("simplex"),
    
    # Application title
    titlePanel(""),
    
    fluidRow(column(2, wellPanel(uiOutput("Crime"))),
             column(4, wellPanel(uiOutput("CostofAttendance"))),
             column(3, wellPanel(uiOutput("NumofAcc"))),
             column(3, wellPanel(uiOutput("State")))),
    # br(),br(),br(), br(), 
    navbarPage("Interactions", 
               tabPanel("Welcome", 
                        br(),
                        HTML("<h4>Hello, Welcome to the R Shiny data visualization tool. This tool is solely designed for illustrative purposes and data shown henceforth in this tool is in no way representative of the actual data. This application is built to explore and present the various reporting, automation and data visualation opportunities provided by R Shiny</h4>"),
                        br(),
                        br(),
                        HTML("<h4>Tool uses a sample datatset of some of the US universities and includes details such as address, geographical location, cost of attendance, number of accredations received and so on. Details on the application are follows:-</h4>"),
                        br(),
                        br(),
                        HTML("<h4><b>Data Explorer: </b>Allows user to filter the data across various cuts and one can also download the filtered data</h4>"), br(),
                        HTML("<h4/><b>Interactive Map: </b> Provides an interactive map with universities and their details</h4>"), br(),
                        HTML("<h4/><b>Chart View: </b> Provides an interactive graphs with universities and their cost of attendance vs salaries post graduation. User can download the graph into a powerpoint slide</h4>"), br(),
                        HTML("<h4/><b>Compare and Decide: </b> User can compare and decide the preferred university based on one's criteria</h4>"), br(),
                        br(),
                        br(),
                        actionButton("Start", "Let's Get Started..."),
                        br(),
                        br(),
                        HTML("<h4/>Please contact <a href='mailto:ckulkar2@uncc.edu?Subject=R-Shiny%20Webpage' target='_top'>Chinmayi Kulkarni</a> if you find an error or if you have suggestions/questions about the implemention, further enhancements or this webpage.</h4>")),
               tabPanel("Data Explorer", DT::dataTableOutput("table")),
               tabPanel("Interactive Map",
                        # div(class="outer",
                        #       tags$head(
                        #           # Include our custom CS
                        #           includeScript("gomap.js")),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="1000", height="500")),
               tabPanel("Chart View", plotOutput("gradSalChart")),
               tabPanel("Compare & Decide", sidebarLayout(sidebarPanel(uiOutput("University")),mainPanel(tableOutput("compareTable")))))
  )
  
)


# setwd("E:/UNCC/Spring2017/BigData/Project/Project Phase 1/fstevens-intimidataers-project")
setwd("H:/Extra/Other/Chinu Shiny")

options(scipen=999)

df <- read.csv("Project_Output_v2_3.14.17.csv", header=TRUE, stringsAsFactors = F)
df$Violent_Crime_Percentile <- round(df$Violent_Crime_Percentile, 2)

#Server Logic
shinyServer(function(input, output, session) {
  
  land <- reactiveValues(check=F)
  
  observeEvent(input$Start, {
    land$check = T
  })
  
  observe({
    if(land$check==T){
      slidervalues <- reactive({
        
        data.frame(
          name = c( "Violent Crime Percentile",
                    "Cost of Attendance"
                    
          ),
          value = as.character(c( input$VCP,
                                  input$COA
          )),
          
          stringsAsFactors = FALSE)
        
      })
      
      output$Crime<-renderUI({
        sliderInput("VCP","Crime Percentile:", min= 0, max =1, value =c(0,1))
      })
      
      
      output$CostofAttendance<-renderUI({
        sliderInput("COA","Cost of Attendance:", min= 0, max =70000, value = c(0,50000), step = 1000)
      })
      
      output$NumofAcc<-renderUI({
        checkboxGroupInput("NOA", "Number of Accreditions:", c('1' = '1', '2' = '2'), selected = c('1' = '1', '2' = '2'))
      })
      
      
      observe({
        updateSliderInput(session, 'COA', value = c(0,15000), min = min(df$COSTT4_A), max = round(max(df$COSTT4_A)/10000)*10000, step = 2000)
      })
      
      output$State<-renderUI({
        selectInput("State", "State", choices=c('ALL', unique(df$STABBR)), selected='ALL')
      })
      
      output$University<-renderUI({
        checkboxGroupInput("University", "Select Universities to Compare:", unique(filterData()$INSTNM))
      })
      
      # Filter data based on selections
      output$table <- renderDataTable(datatable({
        dta <- filterData()
        select_colnames <- c('INSTNM', 'CITY','STABBR', 'Number_Of_Accredagency', 'MN_EARN_WNE_P10', 'Program_Offered', 'COSTT4_A','Violent_Crime_Percentile')
        dta <- dta[, select_colnames]
        colnames(dta) <- c('INSTITUTION NAME', 'CITY', 'STATE', 'NO. OF ACCREDATIONS', 'AVG. GRAD. SALARY', 'PROGRAMS OFFERED', 'COST OF ATTENDANCE', 'CRIME PERCENTILE')
        dta
      }))
      
      output$compareTable <- renderTable({
        dta <- filterData()
        if(nrow(dta)>0){
          select_colnames <- c('INSTNM', 'Branch', 'Url', 'Address', 'CITY','STABBR', 'Sector', 'ACCREDAGENCY', 'MN_EARN_WNE_P10', 'Program_Offered', 'COSTT4_A','Violent_Crime_Percentile')
          proper_names    <- c('INSTITUTION NAME', 'URL', 'BRANCH', 'ADDRESS', 'CITY', 'STATE', 'SECTOR', 'ACCREDATION AGENCIES', 'AVG. GRAD. SALARY', 'PROGRAMS OFFERED', 'COST OF ATTENDANCE', 'CRIME PERCENTILE')
          dta <- dta[dta$INSTNM %in% input$University, select_colnames, drop = F]
          dta$ACCREDAGENCY <- gsub("[|]", ", ", dta$ACCREDAGENCY)
          colnames(dta)    <- proper_names
          dta <- t(dta)
          dta
        }
      }, colnames = F, rownames = T)
      
      output$map <- renderLeaflet({
        dta <- filterData()
        
        leaflet() %>%
          addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>% 
          addCircleMarkers(lng = dta$Long, lat = dta$Lat, radius = 5, layerId = dta$ZIP, fillColor = "#FF0000", fillOpacity = 0.4, stroke=F) %>%
          setView(lng = -93.85, lat = 37.45, zoom = 4)
        
      })
      
      
      # Show a popup at the given location
      showUniversityPopUp <- function(zipcode, lat, lng) {
        
        dta <-filterData()
        
        selectedZip <- dta[dta$ZIP == zipcode,]
        
        content <- as.character(tagList(
          
          tags$h4(selectedZip$INSTNM),
          
          tags$strong(HTML(sprintf("%s, %s %s",
                                   
                                   selectedZip$CITY, selectedZip$STABBR, selectedZip$ZIP
                                   
          ))), tags$br(),
          
          sprintf("Cost of Attendance: %s", paste0("$",selectedZip$COSTT4_A)), tags$br(),
          
          sprintf("Crime Percentile: %s%%", as.integer(selectedZip$Violent_Crime_Percentile)), tags$br(),
          
          sprintf("Accredations: %s", gsub("[|]", ", ", selectedZip$ACCREDAGENCY)), tags$br()
          
          # sprintf("Adult population: %s", selectedZip$adultpop)
          
        ))
        
        leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
        
      }
      
      
      # When map is clicked, show a popup with city info
      observe({
        
        leafletProxy("map") %>% clearPopups()
        
        event <- input$map_marker_click
        
        if (is.null(event))
          
          return()
        
        isolate({
          
          showUniversityPopUp(event$id, event$lat, event$lng)
          
        })
        
      })
      
      output$gradSalChart <- renderPlot({
        dta <- filterData()
        dta <- dta[, c('INSTNM', 'COSTT4_A', 'MN_EARN_WNE_P10')]
        dta[is.na(dta)] = 0
        dta <- dta[(dta$COSTT4_A!=0 & dta$MN_EARN_WNE_P10!=0),]
        rownames(dta) <- c()
        bp <- barplot(as.matrix(t(dta[, 2:3])), ylim=c(-100000, 1.05*max(dta[, 2:3], na.rm = T)),  beside = T, las = 3, space = c(0, 0.15), 
                      ylab = "(Amount in $)", col = c('#5b9bd5', '#ed7d31'), legend.text = c('Cost of Attendance', 'Avg. Post-Grad Salaray'), xpd = T)
        # axis(1, at = bp[1,], labels = dta$INSTNM)
        title('Cost of Attendance vs Avg Post Graduation Salary')
        text(x=bp[1,], y = 0.9*dta$COSTT4_A, labels = dta$COSTT4_A, cex = 0.75)
        text(x=bp[2,], y = 0.9*dta$MN_EARN_WNE_P10, labels = dta$MN_EARN_WNE_P10, cex = 0.75)
        midpoints <- (bp[1, ] + bp[2, ])/2
        text(x=midpoints, y=-40000, srt = 45, labels = substr(dta$INSTNM,1,30))
      })
      
      
      filterData <- reactive({
        data <- df
        
        data$Number_Of_Accredagency <- (str_count(data$ACCREDAGENCY, '[|]') + 1 )
        
        data <- data[(data$Violent_Crime_Percentile <= input$VCP[2] & data$Violent_Crime_Percentile >= input$VCP[1]), ]
        
        
        data <- data[(data$COSTT4_A >= input$COA[1] & data$COSTT4_A <= input$COA[2] ), ]
        
        
        data <- data[(data$Number_Of_Accredagency %in% as.numeric(input$NOA)), ]
        
        if(input$State!='ALL')
          data <- data[(data$STABBR == input$State), ]
        
        
        #if (input$St != "All") { data <- data[data$STABBR == input$St,]}
        
        rownames(data) <- c()  
        
        data
      })
      
    }
  })
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)

