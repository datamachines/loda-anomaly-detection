# these three packages must be installed on system prior to running the app
library(shiny)
library(shinybusy)
library(shinythemes)
# the loda.R fild must be located in the same directory as app.R
source("loda.R", TRUE)

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("LODA Tool for Anomaly Detection"),

  sidebarLayout(
    sidebarPanel(
      tags$h3("Import data"),
      fileInput("file1", "Choose CSV File (data requirements: csv, single sheet data, no missing values)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      tags$hr(),
      tags$h3("Feature Exploration"),
      selectInput('var', 'Single Variable Boxplot', ""),
      tags$hr(),
      tags$h3("Anomaly Detection"),
      uiOutput("checkbox"),
      checkboxInput('selectall', "Select All"),
      helpText("The selected variables will be used to generate the model. DO NOT SELECT VARIABLES WITH MISSING VALUES.
             All non-numeric variables will be dropped for computation. 
             If the user wants to include categorical variables in model, one-hot encode prior to uploading. "),
      numericInput('kprojs', 'Number of projections used to generate detection model:', 100, min = 30, max = 10000),
      helpText("Generally recommend 100 projections to start. The generated projections are used to model the feature space
      of the data. If there are only a few features, even fewer than 100 projections can adequately model 
      the data. A higher number of projections creates the most stable 
               results but will noticeably increase run time. (Minimum limit is set to 30, Maximum = 10,000)"),
      numericInput('outliers', 'Number of outliers for the model to return:', 3, min = 1, max = 10000),
      helpText("When finished press RUN."),
      actionButton(inputId = "run", label = "RUN", 
                   style="color: #FFFFFF; background-color: #ff7f00; border-color: #FFA500")
      
    ),
    mainPanel(
      add_busy_spinner(spin = "fading-circle", color = "white"),
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Recommended Workflow", 
                           tags$h4("1. Upload a dataset"),
                           tags$h4("2. Explore features"),
                           tags$h4("3. Run anomaly detection (this typically takes a few minutes to run, 
                                   and each time RUN is pressed a new model is generated)"),
                           tags$h4("4. Explore the detected anomlies, important features, and 
                                   distribution of anomaly scores"),
                           tags$h4("5. Alter parameters and re-run anomaly detection")),
                  tabPanel("Data Table", 
                           tags$h5("First 10 rows of the uploaded data:"),
                           tableOutput("table")),
                  tabPanel("Feature Exploration",
                           plotOutput("boxplot"),
                           tags$h5("Summary of all features in dataset:"),
                           verbatimTextOutput("summary"),
                           tags$h5("Data dimensions: "),
                           verbatimTextOutput("dims")),
                  tabPanel("Anomaly Detection", 
                           fluidRow(tags$h4("Press RUN to generate a model using the selected features."),
                                    tags$h5("The output generated returns: "),
                                    tags$h5(" - the top most distinct anomalies
                                             (their scores and associated indexes in the data), "),
                                    tags$h5(" - the range of anomalies scores (higher scores = more anomalous, 
                                             range is dependent on the spread of the data), "),
                                    tags$h5(" - the data rows of the anomalies, "),
                                    tags$h5(" - and feature importances (higher positive feature importance score generate greater anomalies,
                                             negative feature importance scores reduce anomalous behavior. "),
                                    verbatimTextOutput("anom"),
                                    tags$hr(),
                                    tags$h5("Scatterplot (anomalies get marked as Xs on graphs):"),
                                    helpText("Upon RUN the variables selected in the option boxes populate a 
                                    scatterplot of the data to visually identify anomalies."),
                                    plotOutput("compare"),
                                    selectInput('xcol', 'Choose X variable to plot', ""),
                                    selectInput('ycol', 'Choose Y variable to plot', "", selected = "")
                           )
                  ),
                  # tabPanel("Table of Selected Features (populated upon RUN)", tableOutput("loda")),
                  tabPanel("Summary of Anomaly Scores", 
                           tags$h5("After RUN, this generates a histogram of the anomaly scores: "),
                           plotOutput("scores"), 
                           numericInput('bins', 'Number of bins:', 20, min = 2, max = 1000)),
                  tabPanel("Documentation",
                           tags$h3("Loda: Lightweight on-line detector of anomalies"),
                           tags$a(href="https://link.springer.com/article/10.1007/s10994-015-5521-0", "Link to paper"),
                           tags$hr(),
                           tags$h4("Methodology:"),
                           tags$h4("1. Represent the feature space using a set of k random projections."),
                           tags$h4("2. Construct k one-dimensional histograms each approximating the probability density 
                                   of input data projected onto a single projection vector."),
                           tags$h4("3. Generate anomaly scores for each data observation based on the joint probability 
                                   of the collection of histograms. "),
                           tags$hr(),
                           tags$h4("Determining Feature Importance:"),
                           tags$h4("Sparse projections (each represents only a subset of features) offer the capability to 
                                   do feature selection. This method is used to rank features according to their contribution
                                   to the degree of anomalousness in the sample data. For instance,
                                   if anomaly scores with a feature being present tend to be much higher than that of without,
                                   the feature is important for identification of outliers. "))
                  
      )

    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2) # increase maximum file input size to 50MB
  
  df <- reactive({ 
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame. 
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
    
    updateSelectInput(session, inputId = 'xcol', label = 'Plot X Variable',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'ycol', label = 'Plot Y Variable',
                      choices = names(df), selected = names(df)[2])

    return(df)
    
  })
  
  # Dynamically generate UI input when data is uploaded 
  output$checkbox <- renderUI({
    checkboxGroupInput(inputId = "variables", 
                       label = "Select variables for Anomaly Detection (requires at least 1): ", 
                       choices = names(df()))
  })
  # select all feature
  observe({
    updateCheckboxGroupInput(session, 'variables', choices = names(df()), selected = if (input$selectall) names(df()))
  })
  
  # populate boxplot options with selected data variables
  observe({
    nums <- unlist(lapply(df(), is.numeric))
    numeric <- df()[ ,nums]
    updateSelectInput(session, inputId = 'var', choices = names(numeric), selected = names(numeric[1]))
  })
  
  ## MAIN TAB 1: FULL DATA TABLE
  output$table <- renderTable({
    head(df(), 10)
  })
  
  ## MAIN TAB 2:EXPLORATORY FEATURE ANALYSIS
  observeEvent(input$file1, {
    singleVar <- reactive({
      df()[, c(input$var)]
    })
    output$boxplot <- renderPlot({
      boxplot(singleVar(), horizontal = TRUE, main = "Single Variable Boxplot")
    })
    output$summary <- renderPrint({
      summary(df())
    })
    output$dims <- renderPrint({
      dim(df())
    })
    
  })
 
  ## MAIN TAB 3: ANOMALY DETECTION RESULTS USING THE LODA ALGORITHM
  # selected data filled by variable checkboxes
  selectedLoda <- eventReactive(input$run, {
    if(is.null(input$variables))
      return()
    df()[input$variables]
  })
  
  loda_output <- eventReactive(input$run, {
    full_loda(selectedLoda(), input$kprojs, input$outliers)
  })
  
  # choose data to plot, make reactive to the inputs
  selectedLodaPlot <- reactive({
    df()[, c(input$xcol, input$ycol)]
  })
  
  
  # observeEvent() triggers code to run on server
  observeEvent(input$run, {
    
    output$anom <- renderPrint({
      print(loda_output()$loda_output)
      anomaly_rows_summary <- print_anomaly_rows(selectedLoda(), loda_output()$loda_output)
      print(anomaly_rows_summary)
      feature_importances <- feature_importances(selectedLoda(), loda_output()$w)
      print(feature_importances)
    })
    
    output$compare <- renderPlot({
      plot(selectedLodaPlot())
      points(selectedLodaPlot()[loda_output()$loda_output$anomaly_indexes,], pch = 4, cex = 4, lwd = 4)
      abline(v=mean(selectedLodaPlot()[,input$xcol]), col="red", lwd=3, lty=2)
      abline(h=mean(selectedLodaPlot()[,input$ycol]), col="blue", lwd=3, lty=2)
    })
  })
  
  ## MAIN TAB 4: TABLE OF VARIABLES USER SELECTED FOR ANOMLY DETECTION
  output$loda <- renderTable({
    head(selectedLoda(),10)
  })
  
  ## MAIN TAB 4: SUMMARY OF ANOMALY SCORES
  bins <- reactive({
    bins <- input$bins
  })
  
  output$scores <- renderPlot({
    input$run
    hist(loda_output()$scores, n=bins(), main = "Histogram of anomaly scores (each data point is assigned a score)", 
         xlab = "scores", labels = TRUE)
  })
  

}

shinyApp(ui, server)