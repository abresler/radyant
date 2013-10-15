# variable selection - factor analysis
output$pmap_brand <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  isCar <- sapply(getdata(), is.character)
 	vars <- vars[isCar]
 	if(length(vars) > 0) selectInput(inputId = "pmap_brand", label = "Brand:", choices = vars, selected = NULL, multiple = FALSE)
})

output$pmap_attr <- renderUI({
  vars <- varnames()
  if(is.null(input$pmap_brand)) return()
  selectInput(inputId = "pmap_attr", label = "Attributes:", choices = vars[-which(vars == input$pmap_brand)], selected = NULL, multiple = TRUE)
})

output$pmap_pref <- renderUI({
  vars <- varnames()
  if(is.null(input$pmap_brand)) return()
  selectInput(inputId = "pmap_pref", label = "Preferences:", choices = vars[-which(vars %in% c(input$pmap_brand,input$pmap_attr))], selected = NULL, multiple = TRUE)
})

output$pmap_dim_number <- renderUI({
  radioButtons(inputId = "pmap_dim_number", label = "", c("2-dims" = 2, "3-dims" = 3), selected = "2-dims")
})

output$pmap_plot <- renderUI({
	plot_list <- c("Brands" = "brand", "Attributes" = "attr")
  if(!is.null(input$pmap_pref)) plot_list <- c(plot_list, c("Preferences" = "pref"))
	checkboxGroupInput("pmap_plot", "Plot:", plot_list)
})

ui_pmap <- function() {
  wellPanel(
  	uiOutput("pmap_brand"),
  	uiOutput("pmap_attr"),
  	uiOutput("pmap_pref"),
  	uiOutput("pmap_dim_number"),
 	 	conditionalPanel(condition = "input.analysistabs == 'Plots'",
	  	uiOutput("pmap_plot"),
 	    numericInput("pmap_scaling", "Arrow scaling factor:", 2.4, .5, 4, .1)
    )
 	)
}

plot.pmap <- function(result) {

	out <- result$out

	lbf <- 1.2
	pbf <- 1.4

	std.pc <- input$pmap_scaling * out$pref.cor
	std.m <- input$pmap_scaling * out$loadings
	std.scores <- out$scores
	max.max <- max(abs(std.m),abs(std.scores),abs(std.pc)) * pbf	# adding a buffer so the labels don't move off the screen

	if(out$nr.dim == 3) {
		op <- par(mfrow=c(3,1))
	} else {
		op <- par(mfrow=c(1,1))
	}

	for(i in 1:(out$nr.dim-1)) {
			for(j in (i+1):out$nr.dim) {
				plot(c(-max.max,max.max),type = "n",xlab='', ylab='', axes = F, asp = 1, yaxt = 'n', xaxt = 'n', ylim=c(-max.max, max.max), xlim=c(-max.max,max.max))
				title(paste("Perceptual Map for",input$datasets,"data\nDimension",i,"vs Dimsension",j))
				abline(v=0, h=0)

				if("brand" %in% input$pmap_plot) {
					points(std.scores[,i], std.scores[,j], col="darkgreen", pch = 16)
					text(std.scores[,i], std.scores[,j], out$brand.names, col="darkgreen", cex = 1.2, adj = c(-.1,-.2))
				}
			
				if("attr" %in% input$pmap_plot) {
					text(lbf*std.m[,i], std.m[,j], input$pmap_attr, col="darkblue", cex = 1.2, adj=c(0.5,-.3))
					# add arrows
					for (k in input$pmap_attr) 
						arrows(0,0, x1=std.m[k,i], y1=std.m[k,j], col="orange", cex=1, length=.1)

					# http://sape.inf.usi.ch/quick-reference/ggplot2/geom_segment
					# http://docs.ggplot2.org/0.9.3.1/geom_abline.html
				}

				if("pref" %in% input$pmap_plot) {
					text(lbf*std.pc[,i], std.pc[,j], input$pmap_pref, col="darkred", cex = 1.2,adj=c(0.5,-.3))
					for (l in input$pmap_pref) 
						arrows(0,0, x1=std.pc[l,i], y1=std.pc[l,j], col="red", length=.1)
				}
			}
		}

	par(op)

}

pmap <- reactive({

	# if(is.null(input$pmap_brand) || is.null(input$pmap_attr)) return("Please select a brand variable and two or more attribute variables")
	if(is.null(input$pmap_brand) || (length(input$pmap_attr) < 2)) return("Please select a brand variable and two or more attribute variables")

	dat <- getdata()

	nr.dim <- as.numeric(input$pmap_dim_number)
	nr.attr <- length(input$pmap_attr)
	# sf <- as.numeric(state$sf)
	# pbf <- as.numeric(state$pbf)
	# lbf <- as.numeric(state$lbf)

	f.data <- dat[,input$pmap_attr]
	brands <- dat[,input$pmap_brand]

	f.res <- suppressWarnings(principal(f.data, nfactors=nr.dim, rotate='varimax', scores=FALSE, oblique.scores=FALSE))
	df <- as.data.frame(f.res$loadings[,colnames(f.res$loadings)])
	m <- as.matrix(df)
	mPm <- (t(m) %*% m)
	smPm <- solve(mPm)
	cscm <- m %*% smPm
	mcar <- as.matrix(f.data)
	scores <- scale(mcar, center = TRUE, scale = TRUE) %*% cscm
	rownames(scores) <- brands

	pc <- std.pc <- 0
	if(!is.null(input$pmap_pref)) {
		pc <- cor(dat[,input$pmap_pref],scores)
		rownames(pc) <- input$pmap_pref
	}

	out <- list()
	out$nr.dim <- nr.dim
	out$scores <- scores
	out$loadings <- m
	out$pref.cor <- pc 
	out$brand.names <- brands
	out$brand.attr <- input$pmap_attr
	if(!is.null(input$pmap_pref))
		out$pref.names <- input$pmap_pref
	
	nr.plots <- factorial(c(nr.dim,2))
	plotHeight = 650 * (nr.plots[1] / nr.plots[2])

	return(list('f.res' = f.res, 'out' = out, 'plotHeight' = plotHeight))
})

summary.pmap <- function(result) {

	cat("\n======== Attribute based perceptual map ==========\n")
	cat("\n--- Factor analysis output --- \n")

	f.res <- result$f.res

	communalities <- as.data.frame(f.res$communality)
	colnames(communalities) <- "communalities"
	cat("\nAttribute communalities:\n")
	print(communalities, digits = 3)

	cat("\nAttribute - Factor loadings:\n")
	print(f.res$loadings, cutoff = 0)
	### print(f.res$loadings, cutoff = .5)

	out <- result$out

	cat("\nBrand - Factor scores:\n")
	print(out$scores, digits = 3)

	if(!is.null(input$pmap_pref)) {
		cat("\nPreference correlations:\n")
		print(out$pref.cor, digits = 3)
	}
}

