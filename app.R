

library(shiny)
library(shinythemes)
library(readxl)
library(MFO)

# Define UI
ui <-
    navbarPage(
        theme = shinytheme("cerulean"),
        "Fast Maximal Fat Oxidation & Kinetics estimation",
        tabPanel("Read before use the app", includeHTML("introduction.html")),
        tabPanel("App",
                 sidebarLayout(
                     sidebarPanel(
                         width = 4,
                         h4("Import data"),
                         fileInput(inputId = "fileBasal",
                                   label = "Basal metabolism",
                                   buttonLabel = "Upload",
                                   accept = c(".csv", ".xlsx")),
                         fileInput(inputId = "fileMFO",
                                   label = "MFO test",
                                   buttonLabel = "Upload",
                                   accept = c(".csv", ".xlsx")),
                         fileInput(inputId = "fileGraded",
                                   label = "Graded exercise test",
                                   buttonLabel = "Upload",
                                   accept = c(".csv", ".xlsx")),
                         br(),
                         h4("Calculate"),
                         actionButton(inputId = "calculateMFO",
                                      label = "MFO & FatMax",
                                      class = "btn-success"),
                         br(),
                         br(),
                         actionButton(inputId = "calculateKinetics",
                                      label = "Kinetics",
                                      class = "btn-success")
                     ),
                     mainPanel(

                         tabsetPanel(
                             tabPanel("MFO & FatMax results",
                                      plotOutput(outputId = "MFOplot1"),
                                      DT::dataTableOutput("MFOtable1")),
                             tabPanel("Kinetics results",
                                      plotOutput(outputId = "MFOplot2"),
                                      DT::dataTableOutput("MFOtable2"))

                         )
                     )

                 )
        )
    )

server <- function(input, output, session) {

    basal_df <- reactive({
        req(input$fileBasal)

        basal_ext <- tools::file_ext(input$fileBasal$name)
        switch(basal_ext,
               csv = vroom::vroom(input$fileBasal$datapath, delim = ","),
               xlsx = readxl::read_excel(input$fileBasal$datapath),
               validate("Invalid file; Please upload a .csv or .xlsx file")
        )
        }
    )

    MFO_df <- reactive({
        req(input$fileMFO)

        MFO_ext <- tools::file_ext(input$fileMFO$name)
        switch(MFO_ext,
               csv = vroom::vroom(input$fileMFO$datapath, delim = ","),
               xlsx = readxl::read_excel(input$fileMFO$datapath),
               validate("Invalid file; Please upload a .csv or .xlsx file")
        )
    }
    )

    graded_df <- reactive({
        req(input$fileGraded)

        graded_ext <- tools::file_ext(input$fileGraded$name)
        switch(graded_ext,
               csv = vroom::vroom(input$fileGraded$datapath, delim = ","),
               xlsx = readxl::read_excel(input$fileGraded$datapath),
               validate("Invalid file; Please upload a .csv or .xlsx file")
        )
    }
    )


    MFOresults <- eventReactive(input$calculateMFO,{

                             MFO::MFO(
                                 step_time = 20,
                                 db_MFO = MFO_df(),
                                 db_basal = basal_df(),
                                 db_graded = graded_df(),
                                 cv_var = "RER",
                                 author = "Frayn",
                                 VO2max = NULL)

                             })

    kineticsResults <- eventReactive(input$calculateKinetics,{

        res_MFO <- MFO::MFO(
            step_time = 20,
            db_MFO = MFO_df(),
            db_basal = basal_df(),
            db_graded = graded_df(),
            cv_var = "RER",
            author = "Frayn",
            VO2max = NULL)

        MFO_kinetics(res_MFO$MFO_db)

    })

    output$MFOplot1 <- renderPlot({MFOresults()$MFO_plot})
    output$MFOtable1 <- DT::renderDataTable(
        DT::formatRound(
        DT::datatable({
        MFOresults()$MFO_db
    },
    options = list(lengthChange = FALSE,
                   searching = FALSE,
                   paging = FALSE)),
    columns = c("VO2", "FAT", "porc_VO2"),
    digits = 2
        )
    )



    output$MFOplot2 <- renderPlot({kineticsResults()$MFO_kinetics_plot})
    output$MFOtable2 <- DT::renderDataTable(
        DT::formatRound(
        table = DT::datatable({
        kineticsResults()$MFO_kinetics_data
        },
        options = list(lengthChange = FALSE,
                       searching = FALSE,
                       paging = FALSE)),
        columns = c("VO2", "FAT", "porc_VO2", "porc_MFO"),
        digits = 2
        )
        )



}

# Run the application
shinyApp(ui = ui, server = server)
