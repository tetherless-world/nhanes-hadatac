#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### Library and Data Imports ####
source("./modules/preprocess_functions.R")
source("./modules/Equity_metrics.R")
source("./modules/antidiabetic_drug_distribution.R")
source("./modules/medication_heatmap.R")



library(shiny)
library(haven)
library(dplyr)
library(ggplot2)   
library(reshape2)
library(ComplexHeatmap)
library(circlize)
mycols <- colorRamp2(breaks = c(-4, 0, 4), 
                     colors = c("#ab2328", "#d4e6e8", "#00205b"))

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      uiOutput("conditionInputs")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      plotOutput("themap")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  df_file_background<- reactive({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
  })
  
  output$conditionInputs <- renderUI({
      column(12, class = "side_sub",
             selectInput(inputId = "HbA1c_input", label = "Select HbA1c",
                         c("≥ 9%","6% - 9%","< 6%")),
             selectInput(inputId = "Insurance_input", label = "Select Insurance Type",
                         c("Private Insurance", "Non-Private Insurance")),
             selectInput(inputId = "CCI_input", label = "Select CCI Level",
                         c("None","Mild","Moderate","Severe")),
             textInput("figure_title", "Caption", "")

      )
    
  })
  
  
  
  output$themap = renderPlot({ 
    unique_ID_df <- df_file_background()
    NHANES_all_mec_unique <- svydesign(data=unique_ID_df, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE)
    #NHANES_all_medications <- subset(NHANES_all_mec_unique, inTargetDiabetesMedication)
    #diabetesTarget_by_race<-as.data.frame(svytable(~Race_Ethnicity,NHANES_all_mec_unique,addNA = TRUE,na.action=NULL,round=TRUE))%>%rename(background_n=`Freq`)%>% mutate(Background_Rate = background_n/sum(background_n))
    
    #jointdataset_medications<-drug_for_race(NHANES_all_medications,antidiabetic_name_code)
    
    #print(jointdataset_medications)
    
    col_order<-nhanesSubgroupProcess_colversion_class3(NHANES_all_mec_unique, c("≥ 9%"), c("Private Insurance"), c("None"), "Poor Controlled HbA1c, Private Insurance, CCI = 0")
    #print(col_order)
    #nhanesSubgroupProcess_class3(NHANES_all_mec_unique, c("≥ 9%"), c("Private Insurance"), c("None"), "Poor Controlled HbA1c, Private Insurance, CCI = 0",col_order)
    nhanesSubgroupProcess_class3(NHANES_all_mec_unique,input$HbA1c_input , input$Insurance_input, input$CCI_input, input$figure_title,col_order)
    
  })
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)