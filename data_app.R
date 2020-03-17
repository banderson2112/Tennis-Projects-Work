# This is a Shiny web application. Let's make one to select data by player
# From the Match Charting Project

#ALL PACKAGES
library(shinythemes)
library(shiny)
library(dplyr)
library(readr)
library(DT)
library(rsconnect)
library(ggplot2)
library(tools)
library(stringr)



matchChartingProject <- read.csv("mcp_revised_data.csv")

attach(matchChartingProject)

matchChartingProject$date <- as.Date(date)

mindate <- "2008-01-05"
maxdate <- "2018-11-04"

# Define UI for application that shows data table for each server
ui <- fluidPage(
  tags$style(HTML("a {color: blue}")),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      #Data table shit
        
        h3("Subsetting Players by Serve"),    # Third level header: Subsetting
        
        # Select which types of servers to show data for
          selectInput(inputId = "server",
                      label = "Select server(s):",
                      choices = unique(Serving),
                      selected = "",multiple = TRUE),
      
        #Select date range
        dateRangeInput(inputId = "date_range",
                    label = "Date range:",
                    start = "2018-01-01",end = "2018-11-04",
                    min = mindate, max = maxdate, startview = "month" ),
      # Show data table
        checkboxInput(inputId = "show_data",
                      label = "Show data table",
                      value = TRUE),
      
    #csv or tsv button
    radioButtons(inputId = "filetype",
                 label = "Select filetype:",
                 choices = c("csv", "tsv"),
                 selected = "csv"),
    
    # Built with Shiny by RStudio
    h5("Built with",
       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "35px"),
       "by",
       img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "35px"),
       ".")
    ),
    
    # Output:
    mainPanel(
      
      h3("Data table"),     # Third level header: Data table
      DT::dataTableOutput(outputId = "servingtable"),
      
      HTML("Select filetype, players, and date range of desire, then hit 'Download data'."),
      br(), br(), # line break and some visual separation
      downloadButton(outputId = "download_data", label = "Download data"),
      br(), br(),
      HTML("Data is courtesy of Jeff Sackmann. More information can be found on his site at the"), a(href="http://www.tennisabstract.com/charting/meta.html", "Match Charting Project",col="blue")
    )
  ), theme = shinytheme("sandstone")
)

# Define server function required to create the output
server <- function(input, output) {
  
  # Create a subset of data filtering for selected title types
  server_selected <- reactive({
    req(input$server)  
    req(input$date_range) # ensure availablity of values before proceeding
    filter(matchChartingProject, Serving %in% input$server & as.integer(format(date, "%Y%m%d")) >= as.integer(format(input$date_range[1],"%Y%m%d")) & as.integer(format(date, "%Y%m%d")) <= as.integer(format(input$date_range[2], "%Y%m%d")) )
  })
  
  # Print data table if checked
  output$servingtable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = server_selected(), 
                    options = list(pageLength = 10, scrollX = TRUE), 
                    rownames = FALSE)
    }
  )
  
  #Download data as csv
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("servers.", input$filetype)
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(server_selected(), path = file) 
      }
      if(input$filetype == "tsv"){ 
        write_tsv(server_selected(), path = file) 
      }
    }
  )
}
# Create Shiny app object
shinyApp(ui = ui, server = server)