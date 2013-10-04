# variable selection - factor analysis
output$mds_id1 <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "mds_id1", label = "ID 1:", choices = vars, selected = NULL, multiple = FALSE)
})

output$mds_id2 <- renderUI({
  vars <- varnames()
  if(is.null(input$mds_id1)) return()
  selectInput(inputId = "mds_id2", label = "ID 2:", choices = vars[-which(vars == input$mds_id1)], selected = NULL, multiple = FALSE)
})

output$mds_dis <- renderUI({
  vars <- varnames()
  if(is.null(input$mds_id2)) return()
  selectInput(inputId = "mds_dis", label = "Dissimilarity:", choices = vars[-which(vars %in% c(input$mds_id1,input$mds_id2))], selected = NULL, multiple = FALSE)
})

ui_mds <- function() {
  wellPanel(
  	uiOutput("mds_id1"),
  	uiOutput("mds_id2"),
  	uiOutput("mds_dis"),
    radioButtons(inputId = "mds_dim_number", label = "", c("2-dims" = 2, "3-dims" = 3), selected = "2-dims")
 	)
}

summary.mds <- function(result) {

	cat("\n======== MDS output ==========\n")
	print(result$co.dist.mat, digits = 3)
	cat("\n")
	print(result$co.mds, digits = 3)

}

plot.mds <- function(result) {
	# prefac <- result$prefac
	# ev <- prefac$Eigenvalues[,'0']
	# plot(ev, type = 'b', col = 'blue', main = "Screeplot of Eigenvalues", ylab = "Eigenvalues", xlab = "# of factors")
	# abline(1,0, col = 'red')

	out <- result$out

	if(out$nr.dim == 3) {
		op <- par(mfrow=c(3,1))
	} else {
		op <- par(mfrow=c(1,1))
	}


	for(i in 1:(out$nr.dim-1)) {
		for(j in (i+1):out$nr.dim) {
			plot(c(-out$lim,out$lim),type = "n",xlab=paste("Dimension",i), ylab=paste("Dimension",j), axes = F, asp = 1, yaxt = 'n', xaxt = 'n', ylim=c(-out$lim, out$lim), xlim=c(-out$lim,out$lim))
			title(paste("Perceptual Map for",input$datasets,"data\nDimension",i,"vs Dimsension",j))
			points(out$points[,i], out$points[,j], col="darkgreen", pch = 16)
			text(out$points[,i], out$points[,j], out$labels, col=rainbow(out$nr.lev,start=.6,end=.1), cex = out$fsize, adj = c(0.4,-.4))
			abline(v=0, h=0)
		}
	}
	par(op)

	# plots <- list()
	# for(var in input$km_vars) {
	# 	plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=1.5, alpha=.3) 
	# }

	# dat <- data.frame(out$points)
	# colnames(dat) <- paste0('Dimension',1:out$nr.dim)
	# dim1 <- 'Dimension1'
	# dim2 <- 'Dimension2'
	# p <- ggplot(dat, aes_string(x=dim1, y=dim2)) + geom_point(shape = 1) + 
	# 			theme(axis.text.x = element_blank(), axis.text.y = element_blank())
	# print(p)

  # p <- ggplot(dat, aes_string(x=input$vizvars1, y=input$vizvars2)) + geom_point()
 	# p <- p + aes_string(color=input$viz_color) + scale_fill_brewer()
	# print(do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2)))))
}

mds <- reactive({

	if(is.null(input$mds_id1) || is.null(input$mds_id2) || is.null(input$mds_dis)) return("Please select two id-variables and a measure of dissimilarity")

	dat <- getdata()

	nr.dim <- as.numeric(input$mds_dim_number)

	dis <- dat[,input$mds_dis]
	id1 <- dat[,input$mds_id1]
	id2 <- dat[,input$mds_id2]

	lab <- unique(c(id1,id2))
	nr.lev <- length(lab)

	lower <- (nr.lev * (nr.lev -1)) / 2
	nr.obs <- length(dis)

	co.dist <- diag(length(lab))
	if(lower == nr.obs) { 
		co.dist[lower.tri(co.dist, diag = FALSE)] <- dis 
	} else if((lower + nr.lev) == nr.obs) { 
		co.dist[lower.tri(co.dist, diag = TRUE)] <- dis 
	} else { 
		return("Number of observations and unique id's does not match.")
	}

	rownames(co.dist) <- lab
	colnames(co.dist) <- lab
	co.dist.mat <- as.dist(co.dist)

	set.seed(1234)

	# co.mds <- suppressWarnings(metaMDS(co.dist.mat, k = nr.dim, trymax = 500))
	# if(co.mds$converged == FALSE) return("The MDS algorithm did not converge. Please try again.")

	co.mds <- isoMDS(co.dist.mat, k = nr.dim)

	pbf <- 1
	fsize <- 1.5

	out <- list()
	out$nr.dim <- nr.dim
	out$data <- co.dist.mat
	out$points <- co.mds$points
	out$labels <- lab
	out$nr.levels <- nr.lev
	out$lim <- max(abs(out$points)) * pbf
	out$fsize <- fsize

	nr.plots <- factorial(c(nr.dim,2))
	plotHeight = 650 * (nr.plots[1] / nr.plots[2])

	return(list('co.mds' = co.mds, 'co.dist.mat' = co.dist.mat, 'out' = out, 'plotHeight' = plotHeight))
})
