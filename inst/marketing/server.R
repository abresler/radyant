dbox_remote <- file.info(list.files(recursive = TRUE, include.dirs = TRUE))
save(dbox_remote, file = "dbox_remote.rda")

shinyServer(function(input, output, session) {

	# source base functions
	source('radyant.R', local = TRUE)

	# source data & analysis tools
	flist_analysis <- sourceDirectory('tools/analysis', recursive = TRUE)
	flist_data <- sourceDirectory('tools/data', recursive = TRUE)

	# data ui-element caller - not used yet
	# output$ui_data <- renderUI(function() {
 	#  	if(input$tool != "data") return()
	#   get(paste('ui_',input$datatabs, sep=""))()
	# })

	# analysis ui-element caller
	output$ui_analysis <- renderUI({
  	if(input$tool == "data") return()
	  get(paste('ui_',input$tool, sep=""))()
	})

	#analysis output tabs can be customized in the tools files
	output$ui_analysis_tabs <- renderUI({
	  tabs <- try(get(paste('ui_',input$tool,'_tabs', sep=""))(), silent = TRUE)
  	if(is(tabs, 'try-error')) {
  		return(tabsetPanel(id = "analysistabs",
	  	  tabPanel("Summary", verbatimTextOutput("summary")),
	    	tabPanel("Plots", plotOutput("plots", height = "100%")))
		  )
	  } else {
  		return(tabs)
	  }
	})

})
