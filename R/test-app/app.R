library(shiny)

ui <- fluidPage(
  titlePanel("NHANES R test app"),
  sidebarLayout(
    sidebarPanel(
      selectInput("filenames", "File:", list.files(path = "/data", pattern = '.csv')),
      actionButton("btnLoad", "Load"),
      varSelectInput("variable0", "Variable:", NULL),
      varSelectInput("variable1", "Variable:", NULL),
      checkboxInput("outliers", "Show outliers", TRUE),
      actionButton("btnPlot", "Plot")
    ),
    
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("varPlot")
    )
  )
)

server <- function(input, output, session) {
  formulaText <- reactive({
    paste(input$variable0, "~", input$variable1)
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  observeEvent(input$btnLoad, {
    nhanes <- read.csv(paste("/data/", input$filenames, sep = ""))
    updateVarSelectInput(session, "variable0", data = nhanes)
    updateVarSelectInput(session, "variable1", data = nhanes)
  })
  
  observeEvent(input$btnPlot, {
    nhanes <- read.csv(paste("/data/", input$filenames, sep = ""))
    output$varPlot <- renderPlot({
      boxplot(as.formula(formulaText()),
              data = nhanes,
              outline = input$outliers,
              col = "#75AADB", pch = 19)
    })
  })
}

shinyApp(ui, server)
