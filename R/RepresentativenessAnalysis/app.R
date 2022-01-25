#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/Equity_metrics.R")
source("modules/sunburst_process.R")
source("modules/studies_comparison.R")

######################################
ui <- dashboardPage(skin = "black", title = "RCT Representativeness Visualization (HADatAc)",
                    dashboardHeader(title = tags$div(
                      class = "title-text",
                      tags$div(id = "logo_block", tags$img(src="Rensselaer.png", id="header_logo"))

                    ),
                    titleWidth = "330px"
                    ),
                    dashboardSidebar(
                      width = 350,
                      sidebarMenu(
                        id = "tabs",
                        menuItem("About", tabName = "profiler"),
                        menuItem("Representativeness Analysis",tabName = "main"),
                        uiOutput("taskInputs"),
                        uiOutput("trialInputs"),
                        uiOutput("characteristicInputs"),
                        uiOutput("orderInputs"),
                        uiOutput("settingsInputs"),
                        uiOutput("metricInputs"),
                        tags$div(
                          class = "graphic sidebar key",
                          strong("Color Reference for Representativeness"),
                          tags$img(src = "color.png")
                        )

                      )
                    ),
                    dashboardBody(
                      style="height:100%;",
                      shinyjs::useShinyjs(),
                      tabItems(
                        tabItem(tabName="profiler",
                                uiOutput("trialProfiler")
                        ),
                        tabItem(tabName="main",
                                uiOutput("mainResult")
                        )
                        
                      ),
                      tags$script(HTML('
         $(document).ready(function() {
                     $(\'head\').append(\'<link rel="stylesheet" href="spread-style.css" type="text/css" />\');
$("header").find("nav").append(\'<div class="clinic title" style="font-size: 1.8em; vertical-align: middle">RCT Representativeness <b><span style = "color: #990000;"><span style="padding-left:2px; padding-right: 2px;">Visualization (HADatAc)</span></span></b></div>\');
// $(".sidebar-toggle").insertBefore(".tab-content");
          })
       ')),
                      

                      
                    )
)

###################################################################
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  options(dplyr.summarise.inform = FALSE)
  ############ initialization ############
  # these will be used to determine which page to show
  showBackgroundUploader<-reactiveVal(TRUE)
  # Tracks the type of task 
  taskType <- reactiveVal("researcher_metric_evaluation")
  
  # Tracks the type of disease 
  diseaseType <- reactiveVal("diabetes")

  # Stores RCT data file
  trialName <- reactiveVal("ACCORD")
  trialData <- reactiveVal()

  # Stores background data
  targetName <- reactiveVal("Target Population (type-II diabetes)")
  backgroundData <- reactiveVal() 
  
  # Tracks the type of inequity metric 
  metricType <- reactiveVal("LDI")
  
  # Tracks the thresholds
  upperThreshold <- reactiveVal(0.4)
  lowerThreshold <- reactiveVal(0.2)
  significanceThreshold <- reactiveVal(0.05)
  
  #Tracks patient characteristics
  patientCharacteristics<-reactiveValues()
  
  ############ Start Pages ############
  
  output$trialProfiler <- renderUI({
    if (showBackgroundUploader()) {
      showModal(modalDialog(
        tags$div(
          class = "ctrial modal-container start-modal",
          HTML("<h1 class = 'start-modal'>RCT Representativeness<b><span style = 'color: #990000;'><span style='padding-left:5px; padding-right: 5px;'>Visualization (HADatAc)</span></span></b></h1>"),
          div(class="ctrial modal-body start-modal-copy",
          h2('Evaluate representativeness of clinical trials.\n', class="start-modal"),
          h2('Compare representations across subgroups among mutliple studies.\n', class="start-modal"),
          h2('Get insights on how to improve the clinical trial and health equity.\n', class="start-modal"),
          actionButton("closeStartModal", "GET STARTED", class = "btn-get-started btn-continue")
        )),
        footer = tags$div(class = "modal-footer", style = "background-color:#fff;margin-top:0",
                          HTML('<a href="https://github.com/TheRensselaerIDEA/ClinicalTrialEquity" target = "_blank">GitHub</a>  |  <a href="https://info.rpi.edu/web-privacy-statement">Privacy Policy</a>'),#<a href="">Clinical Trial Data Template</a>
        )
      ))
      
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")

      column(12,

            tags$div(
                  tags$div(
                    class = "eqiutyMetrics",
                    h2(class="zip", "Instructions"),
                    h3("Step 1: Choose an RCT for Analysis"),
                    p("Users should select one sample RCT from the three available studies (ACCORD for type-II diabetes, ALLHAT for hypertension, and SPRINT for hypertension) that they would like to explore. For the task of comparative analysis, multiple RCTs with the same target population can be selected to compare."),
                    h3("Step 2: Select the Target Population"),
                    p("The target population of interest for our sample RCTs will be automatically selected according to the features of RCTs and the user-selected participant attributes."),
                    
                    fileInput("backgroundUploader", "Choose CSV File(s) for target population",
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    
                    h3("Step 3: Select the Objective and Attributes"),
                    p("(3A) Select a Task: We implement three functions to perform representativeness evaluation of RCTs. 
                    'Representativeness Evaluation' is designed to measures the representativeness level of RCT subgroups with selected participant characteristics;
                    'Study vs Target Population' helps to compare the distribution of participant characteristics between the RCT and target population;
                    'Comparative Study of Representativeness' enables users to compare the representativeness score of a same set of characteristics among different RCTs. 
                        Users can perform one function each time."),
                    p("(3B) Choose Attributes for Evaluation: The selected attributes are regarded as the protected attributes to perform the evaluation."),
                    p("(3C) Variable Order: Users can rearrange the order of participant attributes through drag and drop the attributes shown in the sidebar to generate different sunburst plots. The attributes will be ordered from the inner to the outer rings on the sunburst plots."),
                    h3("Step 4: Select a Metric and its Settings"),
                    p("(4A) Select a Metric: Users can select one of our example representativeness metrics for analysis."),
                    p("(4B) Set Lower Threshold: This value is used to distinguish ranges of metric values from inequitable representation to equitable representation of subgroups. It is usually determined by the published literatures."),
                    p("(4C) Set Upper Threshold: We additionally design this value to distinguish ranges of metric values from highly inequitable representation to inequitable representation of subgroups. It can be selected based on different study scenarios/goals by users."),
                    p("(4D) Set Significance Threshold: This value is the minimum significant difference between RCT and target population subgroup rates that is considered as an equitable representation.")
                  ),
                  
                  actionButton(inputId = "continue2main", label = "Continue", class="btn-continue", style="margin: 5% 0 5% 0")

            ), 
            tags$div(
                    class = "modal-footer",
                    HTML("<p class = 'zip'>For more information about the project, please see the <a href='https://github.com/TheRensselaerIDEA/ClinicalTrialEquity' target = '_blank'> GitHub repository.</a>"),
              ),
                 
        )


    }
  })
  
  
  
  #### task definition####
  output$taskInputs <- renderUI({
      column(12, class = "side_sub",
             fluidRow(
                useShinyFeedback(), # include shinyFeedback
                actionButton("back2Background", label = "Return to About", class = "profiler-button")),             
                uiOutput("user_selected_file_researcher"),
                uiOutput("user_selected_file_researcher_background"),
                selectInput(inputId = "tasks_researcher", label = "Step 3A: Select a Task",
                         c("Representativeness Evaluation" = "metric_r",
                           "Study vs Target Population" = "distribution_p",
                           "Comparative Study of Representativeness" = "comparison_r"),
                         selected ="metric_r" )%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Tasks", content = "'Representativeness Evaluation' measures the representativeness level of RCT subgroups with selected participant characteristics;
                     'Study vs Target Population' compares the distribution of participant characteristics between the RCT and target population;
                     'Comparative Study of Representativeness' compares the representativeness score of a same set of characteristics among different RCTs", placement = "left"
                   )
               )
      )
      

  })
  
  output$metricInputs <- renderUI({
    if (taskType() != "physician_comparison_distribution"){
      column(12, class = "side_sub",
         selectInput(inputId = "equity_metrics", label = "Step 4A: Select a Metric",
                     c("Log Disparity" = "DI_metric",
                       "Normalized Parity" = "AEO_metric"))%>%
           shinyInput_label_embed(
             shiny_iconlink() %>%
               bs_embed_popover(
                 title = "Representativeness Metrics", content = "Representativeness metrics are functions of disease-specific rates of protected subgroups in the RCT. 
                 Log Disparity ensures that the subgroup assignment rates in RCT is similar to those in the matched target population.
                 Normalized Parity ensures that the proportions of people assigned in an ideal RCT but with different protected attribute 
                 values should be assigned into the real clinical trial with equal rates.", placement = "left"
               )
           ),
         use_bs_popover(),
         numericInput("equity_cutoff1", "Step 4B: Set Lower Threshold", 0.2, min = 0.0, max = 1.0, step = 0.01)%>%
           shinyInput_label_embed(
             shiny_iconlink() %>%
               bs_embed_popover(
                 title = "Lower Threshold", content = "It is used to distinguish ranges of values from INEQUITABLE representation to EQUITABLE representation.", placement = "left"
               )
           ),
         numericInput("equity_cutoff2", "Step 4C: Set Upper Threshold", 0.4, min = 0.0, max = 1.0, step = 0.01)%>%
           shinyInput_label_embed(
             shiny_iconlink() %>%
               bs_embed_popover(
                 title = "Upper Threshold", content = "It is used to distinguish ranges of values from HIGHLY INEQUITABLE representation to INEQUITABLE representation.", placement = "left"
               )
           ),
         numericInput("significance_cutoff", "Step 4D: Set Significance Threshold", 0.05, min = 0.0, max = 1.0, step = 0.01)%>%
           shinyInput_label_embed(
             shiny_iconlink() %>%
               bs_embed_popover(
                 title = "Significance Level", content = "It is the minimum significant difference between RCT and target population subgroup rates that is considered as an equitable representation.", placement = "left"
               )
           )
      )
    }

  })
  
  
  output$characteristicInputs<- renderUI({
  if(taskType() != "researcher_comparison_trial"){
      column(12, class = "side_sub",
        uiOutput("user_var_display")
      )
    }
    
  })
  
  
  output$orderInputs<- renderUI({
    if (taskType() == "researcher_metric_evaluation"){
      column(12, class = "side_sub",
             uiOutput("user_var_order")
      )
    }

    
  })
  

  
  ##### Task Inputs ###########
  output$settingsInputs <- renderUI({
    if (taskType() == "researcher_metric_evaluation") {
      column(12, class = "side_sub",
             numericInput("zoom_w", "Zoom Sunburst:", 450, min = 0, max = 10000, step = 50)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = "Zoom Sunburst", content = "Please zoom in and out by adjusting the sunburst size here.", placement = "left"
                   )
               ),
             use_bs_popover(),
             tags$h5("Abbreviations: TC= total cholesterol; FPG = fasting glucose; SBP = systolic blood pressure")
      )
    } 
    
    
    
  })
  
  
  
  ############## Results #####################
  
  output$mainResult <- renderUI({
    if (taskType() == "physician_comparison_distribution"){
      column(12,
        tags$div(
          style="padding-bottom: 50px;background-color: #ffffff;padding-left: 15px;padding-right: 15px;padding-top: 50px;margin-bottom: 20px;",
          h2("Comparison of Participant Characterisitcs between the RCT and the Target Population"),
          h5("Please select the participant characteristics in the sidebar to explore the multi-characteristics subgroups distribution"),
          plotlyOutput(outputId = "comparePlot"),
        ),
        tags$div(
          style="padding-bottom: 50px;background-color: #ffffff;padding-left: 15px;padding-right: 15px;padding-top: 50px;margin-bottom: 20px;",
          class = "graphwrapper",
          tags$span(
            style="display:inline-block; width:49%",
            h2("Summary of Participant Characterisitcs in the RCT"),
            h5("The statistics match the information presented in the above plot"),
            DT::dataTableOutput("summary_ct")
          ),
          tags$span(
            style="display:inline-block; width:49%",
            h2("Summary of Participant Characterisitcs in the Target Population"),
            h5("The statistics match the information presented in the above plot"),
            DT::dataTableOutput("summary_br")
          )
        )
      )
    }
    else if (taskType() =="researcher_metric_evaluation"){
      column(12,
             tags$div(
               style="padding-bottom: 50px;background-color: #ffffff;padding-left: 15px;padding-right: 15px;padding-top: 50px;margin-bottom: 20px;",
               class = "graphwrapper",
               tags$div(
                 class="flex-container",
                 tags$div(
                   class="flex-div column",
                h2("Representativeness Levels for Subgroups"),
                h5("By clicking the subgroup region on the figure, the corresponding metric figure for the subgroup will show on the right.
                   "),
                h5("Note: Reordering the variables shown on the side bar will change inner subgroup results"),
                
                use_bs_popover(),
                plotlyOutput("sun_ldi",height= "600px"),
               ),
               tags$div(
                 class="flex-div column",
                 h2("Metric Function for the Selected Subgroup"),
                 actionButton("reset_di", "resize plot"),
                 h5("Please click 'resize plot' if you want to go back to the original plot after zoom"),
                 h4("Brush and double-click to zoom"),
                 h5(paste0("Selected Subgroup: ",generate_click_subgroup()$ids)),
                 plotOutput("sun_plot_di",
                            dblclick = "sun_plot_di_dblclick",
                            brush = brushOpts(
                              id = "sun_plot_di_brush",
                              resetOnNew = TRUE )
                 )
               )
               )
            ),
            tags$div(
              style="padding-bottom: 50px;background-color: #ffffff;padding-left: 15px;padding-right: 15px;padding-top: 50px;margin-bottom: 20px;",
              htmlOutput("sunburst_text")
            )
      )
    }
    
    else if (taskType() == "researcher_comparison_trial"){
      column(12,
             tags$div(
               style="padding-bottom: 50px;background-color: #ffffff;padding-left: 15px;padding-right: 15px;padding-top: 50px;margin-bottom: 20px;",
               h2("Representativeness of Demographic Characterisitcs in the RCTs"),
               htmlOutput("study_comparison_demo")
             ),
             tags$div(
               style="padding-bottom: 50px;background-color: #ffffff;padding-left: 15px;padding-right: 15px;padding-top: 50px;margin-bottom: 20px;",
               h2("Representativeness of Clinical Characterisitcs in the RCTs"),
               htmlOutput("study_comparison_clinical")
             )
      )
    }

  })
  
  
  
 
  ############## Observed Events ###################### 
 
  observeEvent(input$closeStartModal, {
    removeModal()
  })

  
  

  #######  BACK TO PREVIOUS PAGE ######
  
  # back button functionality for the background uploader
  observeEvent(input$back2Background, {
    updateTabItems(session, "tabs", "profiler")
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    showBackgroundUploader(TRUE)
  })
  

 
  #######  CONTINUOUS TO NEXT PAGE  ########
  observeEvent(input$continue2main,{
    updateTabItems(session, "tabs", "main")
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    showBackgroundUploader(FALSE)
    
  })

  
  
  observeEvent(input$tasks_researcher, {
    if (identical(input$tasks_researcher, "distribution_p")) {taskType("physician_comparison_distribution")}
    else if (identical(input$tasks_researcher, "metric_r")) {taskType("researcher_metric_evaluation")}
    else if (identical(input$tasks_researcher, "comparison_r")) {taskType("researcher_comparison_trial")}
    
  })
  

  
  observeEvent(input$equity_metrics, {
    if (identical(input$equity_metrics, "DI_metric")) {metricType("LDI")}
    else if (identical(input$equity_metrics, "AEO_metric")) {metricType("AEO")}
  })
  
  
  observeEvent(input$equity_cutoff1, {
    lowerThreshold(input$equity_cutoff1)
  })
  
  observeEvent(input$equity_cutoff2, {
    upperThreshold(input$equity_cutoff2)
  })
  observeEvent(input$significance_cutoff, {
    significanceThreshold(input$significance_cutoff)
  })
  

  


  ######################################## Upload Datasets ########################################################
  
  ###########################################  STEP 1 ###############################################
  
  df_file<- reactive({
    name_list<-list()
    path_of_files<-list()
    name_list[1]<- "ACCORD"
    path_of_files[1]<- "./ACCORD.csv"
    name_list[2]<- "SPRINT"
    path_of_files[2]<- "./SPRINT.csv"
    name_list[3]<-"ALLHAT"
    path_of_files[3]<- "./ALLHAT.csv"
    num<-length(name_list)
    new_df <- data.frame("new_name" = as.character(name_list), "old_name" = as.character(name_list), "datapath" = as.character(path_of_files))
    fctr.cols <- sapply(new_df, is.factor)
    new_df[, fctr.cols] <- sapply(new_df[, fctr.cols], as.character)
    
    return(new_df)
    
  })
  
  df_file_names<- reactive({
    new_df<-df_file()
    return(as.list(new_df$new_name))
  })
  
  
  which_file<-function(selected_file, file_group){
    my_row<- file_group[which(file_group$new_name == selected_file), ]
    this_file_path<-as.character(my_row$datapath)
    return(this_file_path)
  }
  
  
  
  output$user_selected_file_comparative <- renderUI({

    if (diseaseType() == "diabetes"){
      files_names<- c("ACCORD")
    }
    else{
      files_names<- c("ALLHAT","SPRINT")
    }

    selectInput("upload_this_file_researcher_compare", "Step 1: Choose RCT(s) for Comparative Analysis", files_names, multiple = TRUE, selected = trialName())
  })
  
  output$user_selected_file_researcher <- renderUI({
    # Get the data set with the appropriate name
    
    if(taskType() == "researcher_comparison_trial"){
       column(12, class = "side_sub",
             uiOutput("user_selected_file_comparative")
    )}
    else{
      files_names<-df_file_names()
      column(12, class = "side_sub",
             selectInput("upload_this_file_researcher", "Step 1: Choose an RCT for Analysis",files_names)
      )
    }   
  }) 
  
  
  observeEvent(input$upload_this_file_researcher, {
    file_groups<- df_file()
    d_path<-which_file(as.character(input$upload_this_file_researcher), file_groups)
    trialName(as.character(input$upload_this_file_researcher))
    
    if (trialName() == "ACCORD"){
      diseaseType("diabetes")
      targetName("Target Population (type-II diabetes)")
    }
    else {
      diseaseType("hypertension")
      targetName("Target Population (hypertension)")
    }
    
    trialData(read.csv(d_path,
                       header = TRUE,
                       sep = ","))
    
    
  })
  
  output$user_selected_file_researcher_background <- renderUI({
    # Get the data set with the appropriate name

    if(is.null(input$backgroundUploader)) {
      files_names<-c("Target Population (type-II diabetes)", "Target Population (hypertension)")
      
    }
    else{
      files_names<-c("Target Population (type-II diabetes)", "Target Population (hypertension)", "HADatAC Target Population")
    }
    
    selectInput("upload_this_file_researcher_background", "Step 2: Select the Target Population",files_names, selected = NULL)
  })
  
  
  df_upload_files<-function(){
    file_groups<- df_file()
    upload <- list()
    selected_files<-as.list(input$upload_this_file_researcher_compare)
    num_files<-length(selected_files)
    for (i in 1:num_files){
      d_path<-which_file(selected_files[i], file_groups)
      current_df<-read.csv(d_path,
                           header = TRUE,
                           sep = ",")
      upload[[i]] <- current_df
    }
    return(upload)
  }
  
  


  
  
  ###########################################  STEP 2 ###############################################
  observeEvent(input$upload_this_file_researcher_background, {
     if ((diseaseType() == "hypertension" & input$upload_this_file_researcher_background == "Target Population (type-II diabetes)") | (diseaseType() == "diabetes" & input$upload_this_file_researcher_background == "Target Population (hypertension)")){
       showFeedbackWarning(
         inputId = "upload_this_file_researcher_background",
         text = "Wrong Target Population!"
       )  
     }
     else {
       hideFeedback("upload_this_file_researcher_background")
       if (input$upload_this_file_researcher_background == "HADatAC Target Population"){
         name_list <- input$backgroundUploader$name
         path_of_file<-input$backgroundUploader$datapath
       }
       else if (diseaseType() == "hypertension"){
         path_of_file<-"./modules/background_data/nhanes_hypertension_combined.csv"
         }
      else{
        path_of_file<-"./modules/background_data/nhanes_diabetes_combined.csv"
        }
       
       backgroundData(read.csv(path_of_file,
                               header = TRUE, 
                               sep = ","))
     }
       
    
     
   })

   



  ######## Variabes Selection & Ordering ##################
  #User pick the protected attributes that they want to analyze (univariate, bivariate, or multivariate)
  output$user_var_display <- renderUI({
    # Get the data set with the appropriate name
    if (is.null(trialData())){
      return()
    }
    dat <- trialData()
    colnames<-names(dat[1:ncol(dat)-1])
    selectInput("vars_display_sundb", "3B: Choose Attributes for Evaluation", 
                choices  = colnames,
                selected = colnames[1:4],
                multiple= TRUE
                )
  })
  
  
  
  
  #User pick the protected attributes that they want to analyze (univariate, bivariate, or multivariate)
  output$user_var_order <- renderUI({
    # Get the data set with the appropriate name
    colnames <- input$vars_display_sundb
    orderInput(inputId = "vars_sundb_order", label= "Step 3C: Variable Order: Inner - Outer (Drag and Drop the Attributes to Reorder)", items  = colnames)
  })

  #########################################################Distribution########################################################
  df_user_compare<- reactive({
    if (is.null(trialData())){
      return()
    }
    df_compare <- trialData()
    
    if("FPG" %in% colnames(df_compare))
    {
      levels(df_compare$FPG)[levels(df_compare$FPG)=="Glucose<100"] <- "Glucose<5.6"
      levels(df_compare$FPG)[levels(df_compare$FPG)=="Glucose 100-125"] <- "Glucose 5.6-6.9"
      levels(df_compare$FPG)[levels(df_compare$FPG)=="Glucose>=126"] <- "Glucose>=7"
    }
    
    
    df_group<- df_compare %>%
      group_by(.dots = input$vars_display_sundb) %>% 
      summarise(user_n = sum(user_n))
    df_cleaned<-na.omit(df_group)
    total_n<- sum(df_cleaned$user_n)
    df_cleaned$Percentage<-df_cleaned$user_n/total_n
    df_cleaned$user_n<-NULL
    return(df_cleaned)
  })
  
  #corresponding background subgroup selection
  df_background_compare <- reactive({
    if (is.null(backgroundData())){
      return()
    }
    
    df_background<-backgroundData()
    
    if("FPG" %in% colnames(df_background))
    {
      levels(df_background$FPG)[levels(df_background$FPG)=="Glucose<100"] <- "Glucose<5.6"
      levels(df_background$FPG)[levels(df_background$FPG)=="Glucose 100-125"] <- "Glucose 5.6-6.9"
      levels(df_background$FPG)[levels(df_background$FPG)=="Glucose>=126"] <- "Glucose>=7"
    }
    
    df_new<- df_background %>%
      group_by(.dots = input$vars_display_sundb) %>% 
      summarise(background_n = sum(background_n))
    df_cleaned<-na.omit(df_new)
    total_n<- sum(df_cleaned$background_n)
    df_cleaned$Percentage<-df_cleaned$background_n/total_n
    df_cleaned$background_n<-NULL
    return(df_cleaned)
  })
  
  df_match_two_df<-reactive({
    if (is.null(input$vars_display_sundb)){
      return()
    }
    if (is.null(backgroundData())){
      return()
    }
    
    if (is.null(trialData())){
      return()
    }
    df_compare_user<-df_user_compare()
    df_compare_base<-df_background_compare()
    df_merged<-merge(df_compare_user, df_compare_base, by=1:(ncol(df_compare_base)-1), all=TRUE)
    df_merged[is.na(df_merged)] <- 0
    return(df_merged)
  })
  
  #user input: subgroup summary table
  output$summary_ct <- DT::renderDataTable({
    df_compare_merged<-df_match_two_df()
    n_v<-ncol(df_compare_merged)
    if (is.null(df_compare_merged)){return()}
    df_user<-df_compare_merged %>%
      dplyr::select(1:(n_v-1))
    
    names(df_user)[n_v-1] <-"Percentage"
    return(datatable(df_user,class = 'cell-border stripe',extensions = 'Scroller', options = list(
      deferRender = TRUE,
      scrollY = 200,
      scrollX = TRUE,
      scroller = TRUE,  
      autoWidth = TRUE,
      columnDefs = list(list(width = '10px', targets = "_all"))
    ))%>%formatPercentage('Percentage', 2)  )
    
  })
  
  #background: corresponding subgroup summary table
  output$summary_br <- DT::renderDataTable({
    df_compare_merged<-df_match_two_df()
    if (is.null(df_compare_merged)){return()}
    n_v<-ncol(df_compare_merged)
    df_base<-df_compare_merged %>%
      dplyr::select(-(n_v-1))
    
    names(df_base)[n_v-1] <-"Percentage"
    return(datatable(df_base,class = 'cell-border stripe',extensions = 'Scroller', options = list(
      deferRender = TRUE,
      scrollY = 200,
      scrollX = TRUE,
      scroller = TRUE,
      columnDefs = list(list(width = '10px', targets = "_all"))
    ))%>%formatPercentage('Percentage', 2) )
  })
  
  
  #comparison barplot 
  output$comparePlot <- renderPlotly({
    if (is.null(input$vars_display_sundb)){
      return()
    }
    df_user<-df_user_compare()
    df_background<- df_background_compare()

    n_used<-ncol(df_background)-1
    
    df_user$Category<-as.factor(rep("Observed",nrow(df_user)))
    df_background$Category<-as.factor(rep("Ideal",nrow(df_background)))
    
    
    df_merged<-rbind(df_background,df_user)
    if (n_used>1){
      df_merged$Subgroup <- apply( df_merged[ , 1:n_used] , 1 , paste , collapse = "-" )
    }
    else{
      names(df_merged)[1]<-"Subgroup"
    }
    
    
    p<-ggplot(df_merged, aes(Subgroup, Percentage, fill= Category, width = 0.9))+ 
      geom_col(position = "dodge") +
      scale_fill_manual(values=c("#94c0c6","#c35442"))+
      theme(axis.text.x = element_text(size=8, face = "bold",angle = 75, hjust = 1),
            legend.position="right",
            legend.title = element_blank(), 
            legend.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"),
            axis.text=element_text(size=8),
            plot.margin = margin(1, 2, 2, 1, "cm"),
            plot.title = element_text(size = 14, face = "bold"),
            text=element_text(size=8))+
      labs(
        title="U.S. Target Population Distribution vs Clinical Trial Population Distribution",
        x="Subgroup Characteristics",
        y="Percentage of Subgroup"
      )
    
    
    ggplotly(p, hoverlabel = label)%>%
      layout(
        hoverlabel = list(font=list(color='white'))
      )
  })
  
  
  output$compare_distribution_text <- renderUI({
    HTML(paste(
      "The ideal rates are estimated from ", tags$a(href="https://www.cdc.gov/nchs/nhanes/index.htm", "National Health and Nutrition Examination Survey Data (NHANES)", target="_blank"), "<br>",
      "", sep=""
    ))
  })
  
  
  
  ######################################################### SUNBURST ########################################################
  df_generate_sunburst <- reactive({
    user_data <- trialData()
    
    if("FPG" %in% colnames(user_data))
    {
      levels(user_data$FPG)[levels(user_data$FPG)=="Glucose<100"] <- "Glucose<5.6"
      levels(user_data$FPG)[levels(user_data$FPG)=="Glucose 100-125"] <- "Glucose 5.6-6.9"
      levels(user_data$FPG)[levels(user_data$FPG)=="Glucose>=126"] <- "Glucose>=7"
    }
    
    background_data <- backgroundData()
    
    if("FPG" %in% colnames(background_data))
    {
      levels(background_data$FPG)[levels(background_data$FPG)=="Glucose<100"] <- "Glucose<5.6"
      levels(background_data$FPG)[levels(background_data$FPG)=="Glucose 100-125"] <- "Glucose 5.6-6.9"
      levels(background_data$FPG)[levels(background_data$FPG)=="Glucose>=126"] <- "Glucose>=7"
    }

    if (metricType() == "LDI"){
      df_processed <-generate_sunburst_df(background_data,as.list(input$vars_sundb_order),user_data,significanceThreshold(),-log(1-lowerThreshold()),-log(1-upperThreshold()),Log_Disparate_Impact, FALSE)
    }
    else if (metricType() == "AEO"){
      df_processed <-generate_sunburst_df(background_data,as.list(input$vars_sundb_order),user_data,significanceThreshold(),lowerThreshold(),upperThreshold(),Adjusted_Equal_Opportunity, FALSE)
      
    }
    
    return(df_processed)
  })
  
  output$sun_ldi <- renderPlotly({
    if (is.null(input$vars_display_sundb)){
      return()
    }
    df_returned<-df_generate_sunburst()
    generate_sunburst_plotly(df_returned, given_width = input$zoom_w)
  })
  
  
  generate_click_subgroup<- reactive({
    # if there is no click data, render nothing!
    clickData<-event_data(event="plotly_click", source = "sunSource")
    if (is.null(clickData)) {
      #print("Click a sunbgroup on the sunburst figure")
      return(NULL)
    } 
    else {
      df_returned<-df_generate_sunburst()
      index_row<-as.integer(clickData[["pointNumber"]])+1
      sungroup_info<-df_returned[index_row,]
      return(sungroup_info)
    }
  })
  
  ranges <- reactiveValues(x = c(0,1), y = c(0,0))
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  
  observeEvent(input$reset_di, {
    ranges$x <- c(0,1)
    ranges$y <- c(0,0)
  })
  
  observeEvent(input$sun_plot_di_dblclick, {
    brush <- input$sun_plot_di_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- c(0,1)
      ranges$y <- c(0,0)
    }
  })

  output$sun_plot_di <-renderPlot({
    if (is.null(input$vars_display_sundb)){
      return()
    }
    returned_info_click<-generate_click_subgroup()
    
    if (is.null(returned_info_click)){
      return(NULL)
    }
    
    
    else{
      if (metricType() == "LDI"){
        sun_plot_process("Log Disparity",returned_info_click$ids,Log_Disparate_Impact,log(1-upperThreshold()) ,log(1-lowerThreshold()),
                       -log(1-lowerThreshold()),-log(1-upperThreshold()),significanceThreshold(),returned_info_click$Ideal_Rate,returned_info_click$Observed_Rate,returned_info_click$EquityLable,seg_length=0.0001,TRUE,ranges$x, ranges$y)
      }
      else if (metricType() == "AEO"){
        sun_plot_process("Normalized Parity",returned_info_click$ids, Adjusted_Equal_Opportunity,
                         -input$equity_cutoff2,-input$equity_cutoff1 ,input$equity_cutoff1,input$equity_cutoff2,input$significance_cutoff,returned_info_click$Ideal_Rate,returned_info_click$Observed_Rate,returned_info_click$EquityLable,seg_length=0.0001,ylimit=FALSE,ranges$x, ranges$y)
      }
      
    }
  })
  

  output$sunburst_text <- renderUI({
    if (is.null(input$vars_display_sundb)){
      return()
    }
    returned_info_click<-generate_click_subgroup()
    
    if (is.null(returned_info_click)){
      return(NULL)
    }
    else{
      if (metricType() == "LDI"){
        metric_used<- "Log Disparity"
      }
      else if (metricType() == "AEO"){
        metric_used<- "Normalized Parity"
      }
      
    }
    
    my_file_name<- as.character(input$upload_this_file_researcher)
    
    ideal_num<- ceiling(returned_info_click$Ideal_Rate * returned_info_click$Trial_Number)
    observed_num<-returned_info_click$Observed_Number
    trial_participants<- returned_info_click$Trial_Number
    if ( returned_info_click$EquityLable=="Representative(p)"){
      my_word<- "equitably represented"
      my_transition<- paste("Since your trial contains a subgroup population of ", observed_num, " subjects, which is not significantly differnet from the target population rate, ")
    }
    else if (returned_info_click$EquityLable=="Representative"){
      my_word<- "equitably represented in this RCT"
      my_transition<- paste("Since your trial contains a subgroup population of ", observed_num, " subjects, which is whithin the metric lower threshold.")
    }
    else if (returned_info_click$EquityLable=="Highly Underrepresented"){
      my_word<- "highly underrepresented in this RCT"
      my_transition<- paste("However your trial contains only a subgroup population of ", 
                            observed_num, " subjects, which is even lower than the negative upper threshold.",sep="")
    }
    else if (returned_info_click$EquityLable=="Underrepresented"){
      my_word<- "underrepresented in this RCT"
      my_transition<- paste("However your trial contains a subgroup population of ", 
                            observed_num, " subjects, which is less than the range for equitble representation but more than the highly 
                            underrepresented threshold.",sep="")
    }
    else if (returned_info_click$EquityLable=="Highly Overrepresented"){
      my_word<- "highly overrepresented in this RCT"
      my_transition<- paste("Your trial includes ", 
                            observed_num, " subjects in the RCT, which is higher than the upper threshold.",sep="")
    }
    
    else if (returned_info_click$EquityLable=="Overrepresented"){
      my_word<- "overrepresented in this RCT"
      my_transition<- paste("However your trial includes ", 
                            observed_num, " subjects, which is over the range for equitable representation but under the highly 
                            overrepresented threshold.",sep="")
    }
    else if (returned_info_click$EquityLable=="Absent"){
      my_word<- "absent in this RCT"
      my_transition<- paste("This means that the whole group is missing from your group.",sep="")
    }
    else if (returned_info_click$EquityLable=="No Info"){
      my_word<- "not included in the both RCT and the target population"
      my_transition<- paste("Unfortunately, we are not able to evaluate it now. The evaluation can be performed if the smoothing method is allowed to
                            be applied to the target population data and the subgroup population is not absent.", sep="")
    }
    
    else if (returned_info_click$EquityLable=="No Base Data"){
      my_word<- "not available in the target population estimated from the reference data"
      my_transition<- paste("Unfortunately, we are not able to evaluate it now; we provide other smoothing methods that can be selected to perform the evaluation.", sep="")
    }

    old_name<-returned_info_click$ids
    group_name<-str_replace(old_name, c("<"), "&lt")

    
   
    HTML(paste(
      "REPORT of the RCT  ", my_file_name,  "<br>",
      "Subgroup Name :  ", group_name, "<br>",
      "Its value of  ", group_name, " using the metric ", metric_used, " is ",  returned_info_click$EquityValue,
      " , which means that this subgroup is ", my_word, ".","<br>",
      "The ideal rate of the subgroup is " ,returned_info_click$Ideal_Rate, " and the observed rate of the subgroup is ", returned_info_click$Observed_Rate, ".","<br>",
      "In an RCT of ", trial_participants, " participants targeting the population of ", diseaseType(), " , we expect a subgroup population of " ,returned_info_click$ids, " to be around ", ideal_num, ".","<br>",
       my_transition,sep=""
    ))
    
  })
  
  
  ######################################################### Multiple Files Comparison########################################################
  
  
  colorpicker <- function(z,cut1 = log(1-upperThreshold()),cut2=log(1-lowerThreshold()),cut3=-log(1-lowerThreshold()),cut4=-log(1-upperThreshold())){
    if(is.na(z)){return("white")}
    #else if(z>cut4){return("white")}
    else if(z == -Inf){return("white")}
    else {return("black")}
  }
  
  bgpicker <- function(z,cut1 = log(1-upperThreshold()),cut2=log(1-lowerThreshold()),cut3=-log(1-lowerThreshold()),cut4=-log(1-upperThreshold())){
    if(is.na(z)){return("#9ea2a2")}
    else if(z == -Inf){return("#ab2328")}
    else if(z <= cut1){return("#d58570")}
    else if( z > cut1 & z <= cut2){return("#eabcad")}
    else if( z > cut2 & z <= cut3){return("#d4e6e8")}
    else if( z > cut3 & z <= cut4){return("#a5b0cb")}
    else if (z>cut4){return("#667ba2")}
  }
  
  
  
  comparison_formatter<-reactive({
    formatter("span",
              style = x ~ style(display = "block",
                                "border-radius" = "4px",
                                "padding-right" = "4px",
                                color = sapply(x,colorpicker),
                                "background-color" = sapply(x,bgpicker)),
              x ~ sprintf("%.2f", x))
  })
  
  output$study_comparison_demo <- renderPrint({
    req(input$upload_this_file_researcher_compare)
    df_list<-df_upload_files()
    df_demo_comparison<-preprocess_comparison_df(as.numeric(significanceThreshold()),as.numeric(lowerThreshold()),as.numeric(upperThreshold()), metricType(), diseaseType(),df_list)
    colnames(df_demo_comparison)<-c("Demographic Characteristics",input$upload_this_file_researcher_compare)

    
    if (diseaseType() == "diabetes"){
      df_demo_comparison<-df_demo_comparison[c(1:13),]
      n<-9
    }
    else if (diseaseType() == "hypertension"){
      df_demo_comparison<-df_demo_comparison[c(1:14),]
      n<-10
    }
    rownames(df_demo_comparison) <- NULL
    
    df_demo_comparison[,2:ncol(df_demo_comparison)] <- sapply(df_demo_comparison[,2:ncol(df_demo_comparison)],as.numeric)
    demo_result<-format_table (df_demo_comparison, 
                  align =c("l","c","c","c"), 
                  lapply(2:ncol(df_demo_comparison), function(col) {
                    area(row=1:nrow(df_demo_comparison), col) ~ comparison_formatter()
                  }
                  
                  ))%>%
      kable_styling("striped", full_width = TRUE,fixed_thead = TRUE) %>%
      pack_rows("Gender", 1,2) %>%
      pack_rows("Age group (years)", 3,5)%>%
      pack_rows("Race/Ethnicity", 6,n) %>%
      pack_rows("Education", n+1,n+4)
    
    gt::html(demo_result)
    
  })
  
  
  
  
  output$study_comparison_clinical <- renderPrint({
    req(input$upload_this_file_researcher_compare)
    df_list<-df_upload_files() 
    df_clinical_comparison<-preprocess_comparison_df(as.numeric(significanceThreshold()),as.numeric(lowerThreshold()),as.numeric(upperThreshold()), metricType(), diseaseType(),df_list)
    colnames(df_clinical_comparison)<-c("Clinical Characteristics",input$upload_this_file_researcher_compare)
      
    if (diseaseType() == "diabetes"){
      df_clinical_comparison<-df_clinical_comparison[c(14:28),]
    }
    else if (diseaseType() == "hypertension"){
      df_clinical_comparison<-df_clinical_comparison[c(15:29),]
    }
    rownames(df_clinical_comparison) <- NULL
    
    df_clinical_comparison[,2:ncol(df_clinical_comparison)] <- sapply(df_clinical_comparison[,2:ncol(df_clinical_comparison)],as.numeric)
    
    
    clinical_result<-format_table (df_clinical_comparison, 
                  align =c("l","c","c","c"), 
                  lapply(2:ncol(df_clinical_comparison), function(col) {
                    area(row=1:nrow(df_clinical_comparison), col) ~ comparison_formatter()
                  }
                  ))%>%
      kable_styling("striped", full_width = TRUE,fixed_thead = TRUE) %>%
      pack_rows("Cigarette-smoking status", 1,2)%>%
      pack_rows("Body-mass index group", 3,6)%>%
      pack_rows("Systolic blood pressure (mm Hg)", 7,10)%>%
      pack_rows("Total cholesterol", 11,12)%>%
      pack_rows("Fasting glucose (mmol/L) ", 13,15)
    
    gt::html(clinical_result)
    
  })
  

  
  
}

# Run the application
shinyApp(ui = ui, server = server)