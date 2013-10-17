# UI-elements for explore
output$expl_columns <- renderUI({
	cols <- varnames()
	isFct <- sapply(getdata(), is.factor)
 	cols <- cols[!isFct]
  if(is.null(cols)) return()
	selectInput("expl_columns", "Select column(s):", choices  = as.list(cols), selected = NULL, multiple = TRUE)
})

output$expl_byvar <- renderUI({
	cols <- varnames()
  isFct <- sapply(getdata(), is.factor)
 	cols <- cols[isFct]
  if(is.null(cols)) return()
  selectInput(inputId = "expl_byvar", label = "Group by:", choices = cols, selected = NULL, multiple = TRUE)
})

# sq <<- function(x) x^2
# inv <<- function(x) 1/x
expl_functions <- list("Mean" = "mean", "Std. dev" = "sd", "N" = "length")

output$expl_function <- renderUI({
  if(is.null(input$expl_byvar)) return()
  selectInput(inputId = "expl_function", label = "Apply function(s):", choices = expl_functions, selected = "Mean", multiple = TRUE)
})

output$ui_explore <- renderUI({
	ui_explore()
})

ui_explore <- function() {
  wellPanel(
    uiOutput("expl_columns"),
    uiOutput("expl_byvar"),
    uiOutput("expl_function"),
	  checkboxInput('expl_show_viz', 'Show plot:', value = FALSE)

   	# radioButtons("changeType", "", c("Change" = "change", "Rename" = "rename", "Add" = "add", "Recode" = "recode"), selected = "Change"),
    # conditionalPanel(condition = "input.changeType == 'change'",
	   #  selectInput("tr_transfunction", "Change columns:", trans_options)
    # ),
    # conditionalPanel(condition = "input.changeType == 'rename'",
    # 	textInput("tr_rename", "Rename (separate by ','):", ''),
	   # 	tags$style(type='text/css', "#tr_rename { max-width: 185px; }")
    # ),
    # conditionalPanel(condition = "input.changeType == 'add'",
    # 	HTML("<label>Copy-and-paste from Excel:</label>"),
	   #  tags$textarea(id="tr_copyAndPaste", rows=3, cols=40, "")
    # ),
    # conditionalPanel(condition = "input.changeType == 'recode'",
	   #  textInput("tr_recode", "Recode (e.g., lo:20 = 1):", ''), 
  	 #  actionButton("tr_recode_sub", "Go")
    # ),
    # actionButton("expl_button", "Run quiry")
  )
}

explore <- reactive({
	if(input$datatabs != 'Explore') return()
	if(is.null(input$datasets) || is.null(input$expl_columns)) return(invisible())
	if(is.null(input$expl_byvar)) return(getdata()[,input$expl_columns])
	getdata()[,c(input$expl_byvar,input$expl_columns)]

})

output$expl_data <- renderPrint({
	if(is.null(input$datasets) || is.null(input$expl_columns)) return(invisible())

	dat <- explore()

	if(is.null(dat)) return(invisible()) 			# ...

	if(is.null(input$expl_byvar)) {
		isFct <- sapply(dat, is.factor)
		isNum <- sapply(dat, is.numeric)
		isDate <- sapply(dat, is.Date)

		if(sum(isNum) > 0) {
			cat("\nSummarize numeric variables:\n")
			print(describe(dat[isNum]))
		}
		if(sum(isFct) > 0) {
			cat("\nSummarize factors:\n")
			print(summary(dat[isFct]))
		}
		if(sum(isDate) > 0) {
			cat("\nSummarize date variables:\n")
			print(summary(dat[isDate]))
		}
	} else {

		for(func in input$expl_function) {
		
			cat("Results grouped by: ", input$expl_byvar, "\n")
			cat("Function used: ", names(which(expl_functions == func)), "\n")
			print(ddply(dat, c(input$expl_byvar), colwise(func)))
			cat("\n")
		}
	}
})

expl_plot_width <- function() {
 	# return(input$expl__plot_width)
 	650
}

expl_plot_height <- function() {
 	# return(input$expl_plot_height)
 	650
}

output$expl_viz <- renderPlot({

	# if(is.null(input$datasets) || is.null(input$expl_show_viz)) return()
	if(is.null(input$datasets) || is.null(input$expl_show_viz)) return()
	if(input$datatabs != 'Explore') return()

		# inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
		# dat <- getdata()
  #   print(p)

	if(input$expl_show_viz) plot(1:10)

}, width = expl_plot_width, height = expl_plot_height)

observe({
	if(is.null(input$expl_button) || input$expl_button == 0) return()
	isolate({

		# reset the values once the changes have been applied
	  # updateTextInput(session = session, inputId = "tr_recode", label = "Recode (e.g., lo:20 = 1):", '')
		# updateSelectInput(session = session, inputId = "tr_transfunction", choices = trans_options, selected = "None")
	})
})

### For when we can move to dplyr

# filter(hflights, Month == 1, DayofMonth == 1, Dest == "DFW")
# head(select(hflights, Year:DayOfWeek))
# summarise(hflights, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
 
# by_dest <- group_by(hflights, Dest)
# filter(by_dest, ArrDelay == max(ArrDelay))
 
# res <- summarise(group_by(hflights, Dest), arr = mean(ArrDelay, na.rm = TRUE))
 
# by_day <- group_by(hflights, Year, Month, DayofMonth)
# by_month <- summarise(by_day, delayed = sum(ArrDelay > 0, na.rm = TRUE))
# by_month
# summarise(summarise(by_month, delayed = sum(delayed)), delayed = sum(delayed))
# summarise(by_month, delayed = sum(delayed))
 
# by_dest <- group_by(hflights, Dest)
# filter(by_dest, ArrDelay == max(ArrDelay))
# summarise(group_by(hflights, Dest), arr = mean(ArrDelay, na.rm = TRUE))
