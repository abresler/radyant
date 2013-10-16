# UI-elements for transform
output$expl_columns <- renderUI({
	cols <- varnames()
	selectInput("expl_columns", "Select column(s):", choices  = as.list(cols), selected = NULL, multiple = TRUE)
})

output$ui_explore <- renderUI({
	ui_explore()
})

ui_explore <- function() {
  wellPanel(
    uiOutput("expl_columns"),

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
    actionButton("expl_button", "Run quiry")
  )
}

explore <- reactive({
	if(is.null(input$datasets)) return()
	if(input$datatabs != 'Explore') return()

	dat <- getdata()
	# if(!is.null(input$expl_button) && !input$expl_button == 0) {
	# 	isolate({
	# 	})
	# }

	dat
})

# output$expl_data <- renderTable({
output$expl_data <- renderPrint({
	if(is.null(input$datasets) || is.null(input$expl_columns)) return()

	dat <- explore()
	# if(is.null(dat)) return()

	# dat <- date2character(dat)
	# dat <- data.frame(dat)

  # if (!require(assertthat))
  # 	print("asserthat not installed.")
  # if (!require(dplyr))
  # 	print("dlyr not installed.")

  # print(dat)

	# summarise(dat, mean = mean(input$expl_columns, na.rm = TRUE))
	# summary(dat)
	describe(dat[,input$expl_columns])
})

observe({
	if(is.null(input$expl_button) || input$expl_button == 0) return()
	isolate({

		# reset the values once the changes have been applied
	 # 	updateTextInput(session = session, inputId = "tr_recode", label = "Recode (e.g., lo:20 = 1):", '')
		# updateSelectInput(session = session, inputId = "tr_transfunction", choices = trans_options, selected = "None")
	})
})


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
