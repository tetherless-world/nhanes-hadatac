library(shiny)

ui <- fluidPage(
  titlePanel("NHANES R test app"),
  sidebarLayout(
    sidebarPanel(
      selectInput("filenames", "File:", list.files(path = "data/Henrique", pattern = '.csv')),
      varSelectInput("variable", "Variable:", nhanes),
      selectInput("variable", "Variable:",
                  c("Study year" = "STUDY.ID")),
      checkboxInput("outliers", "Show outliers", TRUE)
    ),
    
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("varPlot")
    )
  )
)

server <- function(input, output) {
  formulaText <- reactive({
    paste("Subject.Human.Age.Year.Screening.Time ~", input$variable)
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  output$varPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = nhanes,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
  datasetInput <- reactive({
    switch(input$filenames,
           filenames)
    nhanes <- read.csv("data/Henrique/nhanes.csv")
  })
  
}

shinyApp(ui, server)