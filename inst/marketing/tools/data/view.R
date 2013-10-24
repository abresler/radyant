output$view_order <- renderUI({
  if(is.null(input$columns)) return()
  selectInput(inputId = "view_order", label = "Order by:", choices = c("None",input$columns), selected = "None", multiple = FALSE)
})

ui_View <- function() {
  list(wellPanel(
      uiOutput("columns"), 
     	uiOutput("view_order"), checkboxInput("view_order_desc", "DESC", value = FALSE),
      textInput("dv_select", "Subset (e.g., mpg > 20 & vs == 1)", ''), 
      actionButton("sub_select", "Go"),
      uiOutput("nrRows")
    ),
    helpModal('View','view',includeMarkdown("tools/help/view.md"))
  )
}

# output$dataviewer <- renderTable({
output$dataviewer <- reactive({
  if(is.null(input$datasets) || is.null(input$columns)) return()

  # dat <- getdata()
  dat <- date2character()

  # not sure why this is needed when files change ... but it is
  # without it you will get errors the invalid columns have been
  # selected
  if(!all(input$columns %in% colnames(dat))) return()

  if(!is.null(input$sub_select) && !input$sub_select == 0) {
    isolate({
      if(input$dv_select != '') {
        selcom <- input$dv_select
        selcom <- gsub(" ", "", selcom)
        if(nchar(selcom) > 30) q()
        if(length(grep("system",selcom)) > 0) q()
        if(length(grep("rm\\(list",selcom)) > 0) q()
          
        # selcom is a valid expression to the subset command
        selcom <- try(parse(text = paste0("subset(dat,",selcom,")")), silent = TRUE)
        if(!is(selcom, 'try-error')) {
          seldat <- try(eval(selcom), silent = TRUE)
          if(is.data.frame(seldat)) {
            dat <- seldat
            seldat <- NULL
            # showing all rows that fit the condition
            updateSliderInput(session = session, "nrRows", "Rows to show:", value = c(1,nrow(dat)))
          }
        } 
      }
    })
  }

  # order data
  if(!is.null(input$view_order) && input$view_order != "None") {
    indx <- order(dat[,input$view_order], decreasing = input$view_order_desc)
    dat <- dat[indx,,drop = FALSE]
  }

  # Show only the selected columns and no more than 50 rows at a time
  nr <- min(input$nrRows[2],nrow(dat))
  dat <- data.frame(dat[input$nrRows[1]:nr, input$columns, drop = FALSE])

  html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)

})

output$nrRows <- renderUI({
  if(is.null(input$datasets)) return()
  dat <- getdata()

  # number of observations to show in dataview
  nr <- nrow(dat)
  sliderInput("nrRows", "Rows to show:", min = 1, max = nr, value = c(1,min(15,nr)), step = 1)
})

