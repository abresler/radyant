shinyServer(function(input, output, session) {

	# source base functions
	source('radyant.R', local = TRUE)

	# source data & analysis tools
	flist_analysis <- sourceDirectory('tools/analysis', recursive = TRUE)
	flist_data <- sourceDirectory('tools/data', recursive = TRUE)

	# analysis ui-element caller
	output$ui_analysis <- renderUI({
  	if(input$tool == "dataview") return()
	  get(paste('ui_',input$tool, sep=""))()
	})

	# data ui-element caller - not used yet
	# output$ui_dataview <- renderUI(function() {
 	#  	if(input$tool != "dataview") return()
	#   get(paste('ui_',input$datatabs, sep=""))()
	# })

	output$ui_analysis_tabs <- renderUI({

	  tabs <- try(get(paste('ui_',input$tool,'_tabs', sep=""))(), silent = TRUE)
  	if(is(tabs, 'try-error')) {
	    tabsetPanel(id = "analysistabs",
  	    tabPanel("Summary", verbatimTextOutput("summary")),
    	  tabPanel("Plots", plotOutput("plots", height = "100%"))
	    )
	  } else {

	  }
	})
})
