output$view_order <- renderUI({
  if(is.null(input$columns)) return()
  selectInput(inputId = "view_order", label = "Order by:", choices = c("None",input$columns), selected = "None", multiple = FALSE)
})

ui_View <- function() {
  list(wellPanel(
      uiOutput("columns"), 
     	uiOutput("view_order"), checkboxInput("view_order_desc", "DESC", value = FALSE),
      returnTextInput("dv_select", "Subset (e.g., mpg > 20 & vs == 1)", ''), 
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

  if(!all(input$columns %in% colnames(dat))) return()

  if(input$dv_select != '') {
    selcom <- input$dv_select
    selcom <- gsub(" ", "", selcom)

    seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)

    if(!is(seldat, 'try-error')) {
      if(is.data.frame(seldat)) {
        dat <- seldat
        seldat <- NULL
        # showing all rows that fit the condition
        # updateSliderInput(session = session, "nrRows", "Rows to show:", value = c(1,nrow(dat)))
      }
    }
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
  html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  # Encoding(html) <- 'UTF-8'
  html

})

output$nrRows <- renderUI({
  if(is.null(input$datasets)) return()
  dat <- getdata()

  # number of observations to show in dataview
  nr <- nrow(dat)
  sliderInput("nrRows", "Rows to show:", min = 1, max = nr, value = c(1,min(15,nr)), step = 1)
})

