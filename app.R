library(shiny)
library(DT)
devtools::load_all("flies", reset=T)

## Frontend
ui = fluidPage(
  titlePanel("Flies Survival Analysis"),

  fluidRow(
    column(
      4,
      wellPanel(
        fileInput("inputfile", "Input File:", placeholder = "test/tests.xlsx"),
        selectInput("worksheet", "Worksheet:", c()),
        radioButtons("finished", "Experiment finished:",
                     c("Auto" = "auto",
                       "Yes" = "yes",
                       "No" = "no")),
        htmlOutput("warning", class="error.shiny-output-error")
      ),
      uiOutput("downloadPrismCond")
    ),

    column(
      8,
      plotOutput("plot")
    )
  ),

  hr(),


  fluidRow(
    column(
      12,
      dataTableOutput("table")
    )
  )
)

## Backend
server = function(input, output, session) {
  observe({
    req(input$inputfile)
    updateSelectInput(session, "worksheet",
                      choices = readxl::excel_sheets(input$inputfile$datapath)
                      )
  })

  surv = reactive({
    req(input$inputfile, input$worksheet)
    flies::process(input$inputfile$datapath, input$worksheet, input$finished)
  })

  output$table = renderDataTable({
    surv()$tally
  })

  output$plot = renderPlot({
    flies::plot(surv())$plot
  })

  output$downloadPrismCond = renderUI({
    req(input$inputfile, input$worksheet)
    wellPanel(
        downloadButton("downloadPrism", "Download Prism CSV")
    )
  })

  output$downloadPrism = downloadHandler(
    filename = function() {
      paste(
        paste(
          tools::file_path_sans_ext(input$inputfile$name),
          input$worksheet, "prism", sep="_"),
        ".csv", sep="")
    },
    content = function(file) {
      flies::export_prism(surv()$events, surv()$format, file)
    }
  )

  output$warning = renderText({
    surv()$warning
  })
}

## Run the application
shinyApp(ui = ui, server = server)
