ui_Manage <- function() {
  list(wellPanel(
      # radioButtons(inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard"), selected = ".rda"),
      radioButtons(inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard", "examples" = "examples"), selected = ".rda"),
      conditionalPanel(condition = "input.dataType != 'clipboard' && input.dataType != 'examples'",
        conditionalPanel(condition = "input.dataType == 'csv'",
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', '', c(Comma=',', Semicolon=';', Tab='\t'), 'Comma')
        ),
        fileInput('uploadfile', '')
      ),
      conditionalPanel(condition = "input.dataType == 'clipboard'",
        actionButton('loadClipData', 'Paste data')
      ),
      conditionalPanel(condition = "input.dataType == 'examples'",
        actionButton('loadExampleData', 'Load examples')
      )
    ),
    wellPanel(
      radioButtons(inputId = "saveAs", label = "Save data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard"), selected = ".rda"),
      conditionalPanel(condition = "input.saveAs == 'clipboard'",
        actionButton('saveClipData', 'Copy data')
      ),
      conditionalPanel(condition = "input.saveAs != 'clipboard'",
        downloadButton('downloadData', 'Save data')
      )
    ),
    wellPanel(
      uiOutput("removeDataset"),
      actionButton('removeDataButton', 'Remove data')
    ),
    helpModal('Manage','manage',includeMarkdown("tools/help/manage.md"))
  )
}

observe({
  # 'reading' data to clipboard
  if(is.null(input$loadClipData) || input$loadClipData == 0) return()
  isolate({
    os_type <- .Platform$OS.type
    if (os_type == 'windows') {
      dat <- read.table("clipboard", header = TRUE, sep = '\t')
    } else { 
      dat <- read.table(pipe("pbpaste"), header = TRUE, sep = '\t')
    }

    values[['xls_data']] <- as.data.frame(dat)
    values[['datasetlist']] <- unique(c('xls_data',values[['datasetlist']]))
    updateRadioButtons(session = session, inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard", "examples" = "examples"), selected = ".rda")
  })
})

observe({
  # loading all examples files (linked to helpfiles)
  if(is.null(input$loadExampleData) || input$loadExampleData == 0) return()
  isolate({
    path <- "www/examples/"
    examples <- list.files(path)

    for(ex in examples) {

      ext <- file_ext(ex)
      robjname <- sub(paste0(".",ext),"",ex)
      robj <- load(paste0(path,ex))
      values[[robjname]] <- data.frame(get(robj))  # only work with data.frames
      values[['datasetlist']] <- unique(c(robjname,values[['datasetlist']]))
    }

    updateRadioButtons(session = session, inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard", "examples" = "examples"), selected = ".rda")
  })
})

observe({
  # 'saving' data to clipboard
  if(is.null(input$saveClipData) || input$saveClipData == 0) return()
  isolate({
    os_type <- .Platform$OS.type
    if (os_type == 'windows') {
      write.table(getdata(), "clipboard", sep="\t", row.names=FALSE)
    } else { 
      write.table(getdata(), file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
    }
    updateRadioButtons(session = session, inputId = "saveAs", label = "Save data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard"), selected = ".rda")
  })
})

output$downloadData <- downloadHandler(
  filename = function() { paste(input$datasets,'.',input$saveAs, sep='') },
  content = function(file) {

    ext <- input$saveAs
    robj <- input$datasets

    # only save selected columns
    # assign(robj, getdata()[,input$columns])
    assign(robj, getdata())

    if(ext == 'rda') {
      save(list = robj, file = file)
    } else if(ext == 'csv') {
      write.csv(get(robj), file)
    }
  }
)

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
