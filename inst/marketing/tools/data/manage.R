# used in ui.R. Structure relevant for (future) modularization
output$ui_manage <- renderUI({
  ui_manage()
})

ui_manage <- function() {
  list(wellPanel(
      radioButtons(inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", ".xls" = "xls"), selected = ".rda"),
      conditionalPanel(condition = "input.dataType != 'xls'",
        conditionalPanel(condition = "input.dataType == 'csv'",
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', '', c(Comma=',', Semicolon=';', Tab='\t'), 'Comma')
        ),
        fileInput('uploadfile', '')
      ),
      conditionalPanel(condition = "input.dataType == 'xls'",
        HTML("<label>Copy-and-paste data from Excel</label>"),
        tags$textarea(id="xls_paste", rows=3, cols=40, "")
      )
      # selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
    ),
    wellPanel(
      radioButtons(inputId = "saveAs", label = "Save data:", c(".rda" = "rda", ".csv" = "csv"), selected = ".rda"),
      downloadButton('downloadData', 'Save data')
    ),
    wellPanel(
      uiOutput("removeDataset"),
      actionButton('removeDataButton', 'Remove data')
    )
  )
}

observe({
  if(is.null(input$removeDataButton) || input$removeDataButton == 0) return()
  isolate({
    for(rem in input$removeDataset) {
      values[[rem]] <- NULL
    }
    # datasets <<- datasets[-which(datasets == input$datasets)]
    datasets <- values[['datasetlist']]
    if(length(datasets) == length(input$removeDataset)) {
      datasets <- ""
    } else {
      datasets <- datasets[-which(datasets == input$removeDataset)]
    }

    values[['datasetlist']] <- datasets
  })
})
