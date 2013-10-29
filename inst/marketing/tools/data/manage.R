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
      checkboxInput("man_add_descr","Add/edit data description", FALSE),
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
      
      dat <- try(read.table("clipboard", header = TRUE, sep = '\t'), silent = TRUE)
      if(is(dat, 'try-error')) dat <- c("Data from clipboard was not well formatted. Try exporting the data to csv format.")
    } else { 

      dat <- try(read.table(pipe("pbpaste"), header = TRUE, sep = '\t'), silent = TRUE)
      if(is(dat, 'try-error')) dat <- c("Data from clipboard was not well formatted. Try exporting the data to csv format.")
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


loadUserData <- function(filename, uFile, type) {

  ext <- file_ext(filename)
  # objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(filename))
  objname <- sub(paste(".",ext,sep = ""),"",basename(filename))
  ext <- tolower(ext)

  if(ext == 'rda' || ext == 'rdata') {
    # objname will hold the name of the object(s) inside the R datafile
    robjname <- load(uFile)

    if(length(robjname) > 1) {

      values[[objname]] <- data.frame(get(robjname[1]))
      values[[paste0(objname,"_descr")]] <- get(robjname[2])
    }

    values[[objname]] <- data.frame(get(robjname))  # only work with data.frames
  }

  if(values[['datasetlist']][1] == '') {
    values[['datasetlist']] <- c(objname)
  } else {
    values[['datasetlist']] <- unique(c(objname,values[['datasetlist']]))
  }

  if(ext == 'sav') {
    values[[objname]] <- as.data.frame(as.data.set(spss.system.file(uFile)))
  } else if(ext == 'dta') {
    values[[objname]] <- read.dta(uFile)
  } else if(ext == 'csv') {
    values[[objname]] <- read.csv(uFile, header=input$header, sep=input$sep)
  }
}

loadPackData <- function(pFile) {

  robjname <- data(list = pFile)
  dat <- get(robjname)

  if(pFile != robjname) return("R-object not found. Please choose another dataset")

  if(is.null(ncol(dat))) {
    return()
  }

  values[[robjname]] <- dat

  if(values[['datasetlist']][1] == '') {
    values[['datasetlist']] <- c(robjname)
  } else {
    values[['datasetlist']] <- unique(c(robjname,values[['datasetlist']]))
  }
}

output$datasets <- renderUI({

  inFile <- input$uploadfile
  if(!is.null(inFile)) loadUserData(inFile$name, inFile$datapath, input$dataType)

  # if(input$xls_paste != '') {
 #  if(!is.null(input$xls_paste) && input$xls_paste != '') {
  #   values[['xls_data']] <- as.data.frame(read.table(header=T, text=input$xls_paste, sep="\t"))
 #    values[['datasetlist']] <- unique(c('xls_data',values[['datasetlist']]))
  # }

  # clean out the copy-and-paste box once the data has been stored
  # updateTextInput(session = session, inputId = "xls_paste", label = "", '')

  # # loading package data
  # if(input$packData != "") {
  #   if(input$packData != lastLoaded) {
  #     loadPackData(input$packData)
  #     lastLoaded <<- input$packData 
  #   }
  # }

  # Drop-down selection of data set
  # selectInput(inputId = "datasets", label = "Datasets:", choices = datasets, selected = datasets[1], multiple = FALSE)
  selectInput(inputId = "datasets", label = "Datasets:", choices = values$datasetlist, selected = values$datasetlist[1], multiple = FALSE)
})

output$removeDataset <- renderUI({
  # Drop-down selection of data set
  selectInput(inputId = "removeDataset", label = "Remove data from memory:", choices = values$datasetlist, selected = NULL, multiple = TRUE)
})

output$packData <- renderUI({
  selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
})

output$downloadData <- downloadHandler(
  filename = function() { paste(input$datasets,'.',input$saveAs, sep='') },
  content = function(file) {

    ext <- input$saveAs
    robj <- input$datasets


    if(ext == 'rda') {
      if(input$man_data_descr != "") {

        # save data description
        assign(robj, getdata())
        description <- input$man_data_descr
        save(list = c(robj,"description"), file = file)

        # isolate({
        #   values[[paste0(input$datasets,"_descr")]] <- input$man_data_descr
        # })

      } else {

        assign(robj, getdata())
        save(list = robj, file = file)
      }
    } else if(ext == 'csv') {
      assign(robj, getdata())
      write.csv(get(robj), file)
    }
  }
)

output$datasets <- renderUI({

  inFile <- input$uploadfile
  if(!is.null(inFile)) loadUserData(inFile$name, inFile$datapath, input$dataType)

  # # loading package data
  # if(input$packData != "") {
  #   if(input$packData != lastLoaded) {
  #     loadPackData(input$packData)
  #     lastLoaded <<- input$packData 
  #   }
  # }

  # system(paste("rm", inFile$datapath))
  # inFile$datapath <- NULL
  # inFile <- NULL
  # file.remove(as.character(inFile$datapath))
  # inFile$datapath <- NULL
  # file.create(as.character(inFile$data))
  # file.create(inFile$data, showWarnings = FALSE)

  # if(file.exists(inFile$datapath)) unlink(inFile$datapath)
  # if(file.exists(as.character(inFile$datapath))) unlink(inFile$datapath)

  # Drop-down selection of data set
  # selectInput(inputId = "datasets", label = "Datasets:", choices = datasets, selected = datasets[1], multiple = FALSE)
  selectInput(inputId = "datasets", label = "Datasets:", choices = values$datasetlist, selected = values$datasetlist[1], multiple = FALSE)

})

output$removeDataset <- renderUI({
  # Drop-down selection of data set
  selectInput(inputId = "removeDataset", label = "Remove data from memory:", choices = values$datasetlist, selected = NULL, multiple = TRUE)
})

output$packData <- renderUI({
  selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
})

output$htmlDataExample <- reactive({
  if(is.null(input$datasets)) return()

  # dat <- date2character()
  dat <- getdata()

  # Show only the first 10 rows
  nr <- min(10,nrow(dat))
  dat <- data.frame(dat[1:nr,, drop = FALSE])

  dat <- date2character_dat(dat)

  html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  # Encoding(html) <- 'UTF-8'
  html

})
