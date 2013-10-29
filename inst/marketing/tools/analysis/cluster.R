################################################################
# Hierarchical clustering
################################################################

output$hc_vars <- renderUI({

  varCls <- getdata_class()
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()

  selectInput(inputId = "hc_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

hc_method <- list("Ward's" = "ward", "Single" = "single", "Complete" = "complete", "Average" = "average", 
	"McQuitty" =  "mcquitty", "Median" = "median", "Centroid" = "centroid")

hc_dist_method <- c("sq.euclidian", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

ui_hclustering <- function() {
  list(wellPanel(
    uiOutput("hc_vars"), 
    selectInput("hc_dist", label = "Distance measure:", choices = hc_dist_method, selected = hc_dist_method[1], multiple = FALSE),
    selectInput("hc_meth", label = "Method:", choices = hc_method, selected = hc_method[1], multiple = FALSE),

    conditionalPanel(condition = "input.analysistabs == 'Plots'",
	    radioButtons(inputId = "hc_plots", label = "", c("Dendrogram" = "dendo", "Scree plot" = "scree"), selected = "Dendrogram"),
    	# sliderInput("hc_cutoff", "Cluster cutoff:", min = 0, max = 1, value = c(0,1), step = .05)
    	numericInput("hc_cutoff", "Cluster cutoff:", min = 0, max = 1, value = 0, step = .05)
	  )

    # Add nr of clusters backin it you do a cut-off
    # selectInput("hc_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    # actionButton("hc_saveclus", "Save cluster membership")
  	),
		helpModal('Hierarchical cluster analysis','hcclustering',includeMarkdown("tools/help/hcclustering.md"))
	)
}

# main functions called from radyant.R
summary.hclustering <- function(result) {
	cat("Variables used:\n",input$hc_vars,"\n")
	print(result)
	cat("Note: The main output from this analysis is shown in the Plots tab.")
}

plot.hclustering <- function(result) {

	max_height <- max(result$height)
	result$height <- result$height / max_height

	if(input$hc_plots == "dendo") {

		dend <- as.dendrogram(result)
		if(input$hc_cutoff == 0) {
			plot(dend, main = "Dendrogram", xlab = '', ylab = 'Heterogeneity')
		} else {
			plot(dend, ylim = c(input$hc_cutoff,1), leaflab='none', main = "Cutoff Dendrogram", ylab = 'Heterogeneity')
		}

	} else {
		height = rev(result$height[result$height > input$hc_cutoff])
		nr_of_clusters = 1:length(height)
		plot(nr_of_clusters,height, main = "Scree plot", xlab = "Nr of clusters", ylab = "Cluster heterogeneity", type = 'b')
	}
}

# analysis reactive
hclustering <- reactive({

	if(is.null(input$hc_vars)) return("Please select one or more variables of type numeric or integer.")

	ret_text <- "This analysis requires variables of type numeric or integer. Please select another dataset."
	if(is.null(inChecker(c(input$hc_vars)))) return(ret_text)

	dat <- scale( getdata()[,input$hc_vars] ) 					# standardizing the data
	if(input$hc_dist == "sq.euclidian") {
		dist.data <- dist(dat, method = "euclidean")^2
	} else {
		dist.data <- dist(dat, method = input$hc_dist)
	}

	res <- hclust(d = dist.data, method= input$hc_meth)
	res$plotHeight = 650
	res
})

# Could use ggplot2 for dendrogram
# library(ggplot2)
# library(ggdendro)
# hc <- hclust(dist(USArrests))
# hcdata <- dendro_data(hc, type="rectangle")
# ggplot() + 
#     geom_segment(data=segment(hcdata), aes(x=x, y=y, xend=xend, yend=yend)) +
#     geom_text(data=label(hcdata), aes(x=x, y=y, label=label, hjust=0), size=3) +
#     coord_flip() + scale_y_reverse(expand=c(0.2, 0))

# ### demonstrate plotting directly from object class hclust
# ggdendrogram(hc, rotate=FALSE)
# ggdendrogram(hc, rotate=TRUE)
# ### demonstrate converting hclust to dendro using dendro_data first
# hcdata <- dendro_data(hc)
# ggdendrogram(hcdata, rotate=TRUE) + labs(title="Dendrogram in ggplot2")


observe({
	if(is.null(input$hc_saveclus) || input$hc_saveclus == 0) return()
	isolate({
		if(is.character(hclustering())) return()
		clusmem <- cutree(hclustering(), k = input$hc_nrClus)
		changedata(data.frame(as.factor(clusmem)), paste("hclus",input$hc_nrClus,sep=""))
	})
})

################################################################
# Kmeans clustering
################################################################

output$km_vars <- renderUI({

  varCls <- getdata_class()
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()

  selectInput(inputId = "km_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})
  
ui_kmeansClustering <- function() {
  list(wellPanel(
    uiOutput("km_vars"), 
	  checkboxInput(inputId = "km_hcinit", label = "Initial centers from HC", value = TRUE),
  	conditionalPanel(condition = "input.km_hcinit == true",
  		wellPanel(
	  		selectInput("km_dist", label = "Distance measure:", choices = hc_dist_method, selected = hc_dist_method[1], multiple = FALSE),
  			selectInput("km_meth", label = "Method:", choices = hc_method, selected = hc_method[1], multiple = FALSE)
  		)
  	),
  	conditionalPanel(condition = "input.km_hcinit == false", 
	    numericInput("km_seed", "Set random seed:", 1234, min = 0)
	  ),
    numericInput("km_nrClus", "Number of clusters", 2, min = 2),
    actionButton("km_saveclus", "Save cluster membership")
  	),
		helpModal('Kmeans cluster analysis','kmeansClustering',includeMarkdown("tools/help/kmeansClustering.md"))
	)
}

summary.kmeansClustering <- function(result) {
	nrClus <- input$km_nrClus

	cat("Kmeans clustering with", nrClus, "clusters of sizes", paste0(result$size, collapse=", "),"\n\n")
	cat("Cluster means:\n")
	
	# print(result$centers, digits = 3)

	dat <- getdata()[,input$km_vars, drop = FALSE]
	cvar <- as.factor(result$cluster)
	dat <- cbind(cvar,dat)
	cnt <- ddply(dat, c("cvar"), colwise(mean))
	cnt <- cnt[,-1, drop = FALSE]
	colnames(cnt) <- input$km_vars
	clusNames <- paste("Cluster",1:nrClus)
	rownames(cnt) <- clusNames
	print(cnt, digits = 3)

	# percentage of within cluster variance accounted for by
	# each cluster
	perc_within <- 100 * (result$withinss / result$tot.withinss)
	perc_within <- data.frame(paste0(sprintf("%.2f",perc_within),"%"))
	rownames(perc_within) <- clusNames
	colnames(perc_within) <- ""
	cat("\nPercentage of within cluster variance accounted for by each cluster:\n")
	print(perc_within, digits = 1)

	# percentage of between cluster variance versus the total
	# higher is better
	perc_between <- 100*(result$betweenss / result$totss)
	cat(paste0("\nBetween cluster variance accounts for ", sprintf("%.2f",perc_between), "% of the\ntotal variance in the data (higher is better)."))	

}

plot.kmeansClustering <- function(result) {
	# several things to work on here to clean-up the plots
	dat <- getdata()[,input$km_vars, drop = FALSE]
	dat$clusvar <- as.factor(result$cluster)

	plots <- list()
	for(var in input$km_vars) {
		plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=1.5, alpha=.3) +
				labs(y = "") + theme(axis.text.y = element_blank())
	}
	print(do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2)))))
}

# analysis reactives
kmeansClustering <- reactive({

	if(is.null(input$km_vars)) return("Please select one or more variables of type numeric or integer.")

	ret_text <- "This analysis requires variables of type numeric or integer. Please select another dataset."
	if(is.null(inChecker(c(input$km_vars)))) return(ret_text)

	set.seed(input$km_seed)
	dat <- scale( getdata()[,input$km_vars] )
	# dat <- getdata()[,input$km_vars]

	if(input$km_hcinit) {
		clusmem <- cutree(hinitclustering(), k = input$km_nrClus)
		cluscenters <- as.matrix(aggregate(dat,list(clusmem),mean)[-1])
		km_res <- kmeans(na.omit(object = data.frame(dat)), centers = cluscenters, iter.max = 500)
	} else {
		km_res <- kmeans(na.omit(object = data.frame(dat)), centers = input$km_nrClus, nstart = 10, iter.max = 500)
	}

	nrVars <- length(input$km_vars)
	km_res$plotHeight <- 325 * (1 + floor((nrVars - 1) / 2))
	km_res$plotWidth <- 325 * min(nrVars,2)

	km_res
})
 
hinitclustering <- reactive({
	if(is.null(input$km_vars)) return("Please select one or more variables")
	dat <- getdata()[,input$km_vars]
	if(input$km_dist == "sq.euclidian") {
		dist.data <- dist(dat, method = "euclidean")^2
	} else {
		dist.data <- dist(dat, method = input$km_dist)
	}
	hclust(d = dist.data, method= input$km_meth)
})

# save cluster membership when action button is pressed
observe({
	if(is.null(input$km_saveclus) || input$km_saveclus == 0) return()
	isolate({
		if(is.character(kmeansClustering())) return()
		clusmem <- kmeansClustering()$cluster
		changedata(data.frame(as.factor(clusmem)), paste("kclus",input$km_nrClus,sep=""))
	})
})
