library(shiny)
library(ggplot2)
library(shinyalert)

ui <- fluidPage(

  titlePanel(h1("LivTransplantEvolution Shiny App")),

  sidebarLayout(

    # Sidebar Panel for user inputs
    sidebarPanel(

      tags$p("The Shiny App for the LivTransplant Evolution R package.
             It displays the cleaned and processed dataset containing Liver
             transplantation relevant biomarker and clinical
             patient data taken from a csv file that the user uploads. It uses
             this data set to Calculate the MELD, MELD-na and MELD3.0 risk
             score values for each patient entry in the dataset and creates a
             MELD dataset containing the computed MELD, MELD-na and MELD3.0 risk
             score values for every data entry in the cleaned up dataset.
             It plot the logitudinal trajectories for the specified patient's
             MELD, MELD-na and MELD3.0 risk score values. The specified patient's
             patient ID is the other user input"),

      br(),

      # Link for Sample Dataset
      shinyalert::useShinyalert(force = TRUE),  # Set up shiny alert
      uiOutput("tab1"),
      actionButton(inputId = "data",
                   label = "Sample Dataset Details"),
      br(),
      br(),
      br(),

      tags$p("Upload a Valid csv file dataset containing patient data"),
      # Input for raw patient data csv file
      fileInput("file", "Choose CSV File:",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),


      tags$p("Enter a Valid patient ID corresponding to a patient present in your input dataset"),
      # input to take patient ID to plot MELD for
      numericInput("patient_id", "Enter Patient ID:", value = 153090),

      br(),
      br(),

      # Run button
      actionButton("runButton", "Run")
    ),

    # Main panel for displaying of outputs
    mainPanel(

      # Outputs
      tabsetPanel(

        tabPanel("Cleaned Up Patient Data Table",
                 h3("Instructions: Please Upload a valid patient data CSV file
                    and press 'Run'", style = "font-weight: bold;"),
                 br(),
                 br(),
                 h3("Cleaned up and Processed DataSet containing Liver Transplantation
                    relevant patient data", style = "font-weight: bold;"),
                 tableOutput("cleanedData")),
        tabPanel("Patient MELD Scores Table",
                 h3("Instructions: Please Upload a valid patient data CSV file
                    and press 'Run'", style = "font-weight: bold;"),
                 br(),
                 br(),
                 h3("Calculated MELD, MELDna and MELD3.0 risk scores from patient
                    info data set", style = "font-weight: bold;"),
                 tableOutput("meldScores")),
        tabPanel("Plot of Longitudinal Trajectory of Patient MELD scores",
                 h3("Instructions: Please Upload a valid patient data CSV file
                 and valid patient ID present in your uploaded data and
                    press 'Run'", style = "font-weight: bold;"),
                 br(),
                 br(),
                 h3("Plot of Patient's MELD, MELDna and MELD3.0 Longitudinal
                    Trajectories", style = "font-weight: bold;"),
                 plotOutput("meldPlot"))
      )
    )
  )
)

server <- function(input, output) {

  # Function to process data
  processData <- eventReactive(input$runButton, {
    req(input$file)  # Ensure that a file is uploaded
    inFile <- input$file
    preparedData <- LivTransplantEvolution::prepareData(inFile$datapath)
    meldData <- LivTransplantEvolution::createMELDdf(preparedData)
    list(prepared = preparedData, meld = meldData)
  })

  # Display cleaned data
  output$cleanedData <- renderTable({
    processData()$prepared
  })

  # Display MELD scores data
  output$meldScores <- renderTable({
    processData()$meld
  })

  # Display MELD plot
  output$meldPlot <- renderPlot({
    req(processData())
    LivTransplantEvolution::visualizeMELD(processData()$meld, input$patient_id)
  })

  # URL for downloading Sample data set
  url <- a("Example Sample Dataset", href="https://raw.githubusercontent.com/Mouaid-ALim/LivTransplantEvolution/master/inst/extdata/updated_meld_patient_data.csv")
  output$tab1 <- renderUI({
    tagList("Download:", url)
  })

  observeEvent(input$data, {
    # Show data set detail when button is pressed
    shinyalert(title = "Sample Dataset",
               text = "This is a Sample dataset in the csv file format that can be used to run the LivTransplantEvolution shiny app. It contains the same column headers found in the SRTR database stathiist_liin datafile. All data was created by Mouaid Alim manually. The data set contains sample patient data for 3 patients, one to showcase increasing MELD risk scores, one to showcase fluctuating MELD risk scores and one to show decreasing MELD risk scores.
               Citation: Alim, M. (2023) LivTransplantEvolution: Visualize MELD score trajectories Unpublished. URL https://github.com/Mouaid-Alim/LivTransplantEvolution.",
               type = "info")
  })
}

shinyApp(ui = ui, server = server)

