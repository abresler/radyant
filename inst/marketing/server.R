dbox_remote <- file.info(list.files(recursive = TRUE, include.dirs = TRUE))
save(dbox_remote, file = "dbox_remote.rda")

shinyServer(function(input, output, session) {

	# source base functions
	source('radyant.R', local = TRUE)

	# source data & analysis tools
	flist_analysis <- sourceDirectory('tools/analysis', recursive = TRUE)
	flist_data <- sourceDirectory('tools/data', recursive = TRUE)

	# find the appropriate UI
	output$ui_finder <- renderUI({
  	if(input$tool == "data") {
  		if(!is.null(input$datatabs)) get(paste('ui_',input$datatabs, sep=""))()
		} else {
		  if(!is.null(input$tool)) get(paste('ui_',input$tool, sep=""))()
		}
	})

	# data tabs
	output$ui_data_tabs <- renderUI({
    tabsetPanel(id = "datatabs",
      tabPanel("Manage", HTML('<h4>20 (max) rows shown. See View-tab for details.</h4>'), tableOutput("dataexample")),
      tabPanel("View", tableOutput("dataviewer")),
      tabPanel("Visualize", plotOutput("visualize", width = "100%", height = "100%")),
      tabPanel("Explore", verbatimTextOutput("expl_data"), plotOutput("expl_viz", width = "100%", height = "100%")),
      # tabPanel("Merge", #   HTML('<label>Merge data.<br>In progress. Check back soon.</label>') # ),
      tabPanel("Transform", tableOutput("transform_data"), br(), verbatimTextOutput("transform_summary")),
      tabPanel("About", includeRmd("about.Rmd"))
    )
	})

	# analysis output tabs can be customized in the tools files
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
