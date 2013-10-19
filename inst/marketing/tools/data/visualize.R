# variable selection in the datatabs views
output$vizvars1 <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	selectInput(inputId = "vizvars1", label = "X-variable", choices = as.list(cols), selected = NULL, multiple = FALSE)
})

# variable selection
output$vizvars2 <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	if(is.null(input$vizvars1)) return()

	dat <- getdata()
	if(!input$vizvars1 %in% colnames(dat)) return()
	if(is.Date(dat[,input$vizvars1])) {
		selectInput(inputId = "vizvars2", label = "Y-variable", choices = as.list(cols[-which(cols == input$vizvars1)]), selected = "", multiple = TRUE)
	} else {
		selectInput(inputId = "vizvars2", label = "Y-variable", choices = c("None" = "",as.list(cols[-which(cols == input$vizvars1)])), selected = "", multiple = FALSE)
	}
})

output$viz_color <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	if(length(input$vizvars2) > 1) return()
	selectInput('viz_color', 'Color', c('None'="", as.list(cols)))
})

output$viz_facet_row <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	if(length(input$vizvars2) > 1) return()
	# isFct <- sapply(getdata(), is.factor || is.integer)
	isFct <- sapply(getdata(), is.factor)
 	cols <- cols[isFct]
	selectInput('viz_facet_row', 'Facet row', c(None='.', as.list(cols)))
})

output$viz_facet_col <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	if(length(input$vizvars2) > 1) return()
	# isFct <- sapply(getdata(), is.factor || is.integer)
	isFct <- sapply(getdata(), is.factor)
 	cols <- cols[isFct]
	selectInput('viz_facet_col', 'Facet col', c(None='.', as.list(cols)))
})

ui_Visualize <- function() {
	list(wellPanel(
			uiOutput("vizvars1"),
			uiOutput("vizvars2"),
			conditionalPanel(condition = "input.vizvars2 != ''",
			  uiOutput("viz_color"),
			  uiOutput("viz_facet_row"),
			  uiOutput("viz_facet_col"),
			  checkboxInput('viz_smooth', 'Smooth', value = FALSE),
			  checkboxInput('viz_jitter', 'Jitter', value = FALSE)
			),
			div(class="row-fluid",
	    	div(class="span6",numericInput("viz_plot_height", label = "Plot height:", min = 100, step = 50, value = 650)),
	      div(class="span6", numericInput("viz_plot_width", label = "Plot width:", min = 100, step = 50, value = 650))
	    )
		),
	  helpModal('Visualize','visualize',includeMarkdown("tools/help/visualize.md"))
  )
}

viz_plot_width <- function() {
	if(is.null(input$viz_plot_width)) return(650)
 	return(input$viz_plot_width)
}

viz_plot_height <- function() {
	if(is.null(input$viz_plot_height)) return(650)
 	return(input$viz_plot_height)
}

output$visualize <- renderPlot({
	if(is.null(input$datasets) || is.null(input$vizvars1) || is.null(input$vizvars2)) return()
	if(input$datatabs != 'Visualize') return()

		# inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
		dat <- getdata()
		if(!input$vizvars1 %in% colnames(dat)) return()

		if(input$vizvars2 == "") {
			p <- ggplot(dat, aes_string(x=input$vizvars1)) + geom_histogram(fill = 'blue', alpha=.3) 
			return(print(p))
		} else {

			if(is.Date(dat[,input$vizvars1])) {

				if(length(input$vizvars2) > 1) {
					mdat <- melt(dat[,c(input$vizvars1,input$vizvars2)],id=input$vizvars1)
					p <- ggplot(mdat,aes_string(x=input$vizvars1,y="value",colour="variable",group="variable")) + geom_line()
				} else {
				  p <- ggplot(dat, aes_string(x=input$vizvars1, y=input$vizvars2)) + geom_line()
				}
			} else {

				if(is.factor(dat[,input$vizvars1])) {
				 	# updateCheckboxInput(session = session, inputId = "viz_jitter", label = "Jitter", value = TRUE)
				 	updateCheckboxInput(session = session, inputId = "viz_smooth", label = "Smooth", value = FALSE)
				  p <- ggplot(dat, aes_string(x=input$vizvars1, y=input$vizvars2, fill=input$vizvars1)) + geom_boxplot(alpha = .3)
			  } else {
			  	p <- ggplot(dat, aes_string(x=input$vizvars1, y=input$vizvars2)) + geom_point()
			  }
			}
		}

    if (input$viz_color != '') {
    	p <- p + aes_string(color=input$viz_color) + scale_fill_brewer()
    }

    facets <- paste(input$viz_facet_row, '~', input$viz_facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$viz_jitter) p <- p + geom_jitter()
    if (input$viz_smooth) p <- p + geom_smooth(method = "lm", size = .75, linetype = "dotdash")
    
    print(p)

}, width = viz_plot_width, height = viz_plot_height)

