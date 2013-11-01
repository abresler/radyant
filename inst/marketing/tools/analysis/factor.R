# variable selection - factor analysis
output$preFactor_vars <- renderUI({

  varCls <- getdata_class()
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()

  selectInput(inputId = "preFactor_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

ui_preFactor <- function() {
  list(wellPanel(
  	uiOutput("preFactor_vars")),
		helpModal('Pre-factor analysis','preFactor',includeHTML("tools/help/preFactor.html"))
	)
}


summary.preFactor <- function(result) {

	btest <- result$btest
	prefac <- result$prefac

	cat("\n\nPre-factor analysis diagnostics:\n\n")
	cat("Bartlett test of sphericity\n")
	cat("Chi-square: ", round(btest$chisq,3), "\n")
	cat("Degrees of freedom: ", btest$df, "\n")
	cat("p-value: ", round(btest$p.value,3), "\n")
	cat("H0: Correlation Matrix = Identity Matrix, i.e., variables are not correlated\n")

	cat("\nKaiser-Meyer-Olkin measure of sampling adequacy\nKMO: ", round(prefac$KMO,3), "\n")
	cat("\nMeasures of sampling adequacy:\n")
	print(prefac$MSA, digits = 3)
	cat("\n")

	ev <- prefac$Eigenvalues[,'0']
	ev.var <- ev/sum(ev)
	ev.cvar <- cumsum(ev.var)
	df <- data.frame(1:length(ev),ev,ev.var,ev.cvar)
	colnames(df) <- c("Factor","Eigen Values","% of variance","Cumulative %") 
	# print(df, digits = 3, row.names = FALSE)
	df
}

plot.preFactor <- function(result) {

	prefac <- result$prefac
	ev <- prefac$Eigenvalues[,'0']
	plot(ev, type = 'b', col = 'blue', main = "Screeplot of Eigenvalues", ylab = "Eigenvalues", xlab = "# of factors")
	abline(1,0, col = 'red')
}

preFactor <- reactive({

	if(is.null(input$preFactor_vars) || length(input$preFactor_vars) < 2) return("Please select two or more variables")

	ret_text <- "This analysis requires a multiple variables of type numeric or integer. Please select another dataset."
	if(is.null(inChecker(c(input$preFactor_vars)))) return(ret_text)

	dat <- getdata()[,input$preFactor_vars]
	if(nrow(dat) < ncol(dat)) return("Data has more variables than observations. Please reduce the number of variables.")

	btest <- cortest.bartlett(cor(dat), nrow(dat))
	prefac <- paf(as.matrix(dat))

	return(list(btest = btest, prefac = prefac))
})

# variable selection - factor analysis
output$factor_vars <- renderUI({

  varCls <- getdata_class()
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()

  # No memory when you want it :)
  # sel <- NULL
  # if(!is.null(input$preFactor_vars) || !is.null(inChecker(c(input$preFactor_vars)))) sel <- input$preFactor_vars

  selectInput(inputId = "factor_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

fac_method <- c('Principal components' = 'PCA', 'Maximum Likelihood' = "maxlik")
fac_rotation <- c('Varimax' = 'varimax', 'None' = 'none')

ui_fullFactor <- function() {
  list(wellPanel(
    uiOutput("factor_vars"), 
    selectInput("fac_method", label = "Method:", choices = fac_method, selected = fac_method[1], multiple = FALSE),
    numericInput("fac_number", label = "Number of factors:", min = 1, value = 1),

    HTML("<label>Format loadings matrix:</label>"),
		div(class="row-fluid",
    	div(class="span6", numericInput("fac_cutoff", label = "", min = 0, max = 1, value = 0, step = .05) ),
      div(class="span6", checkboxInput("fac_condFormat", "Conditional", value = FALSE) )
    ),

    radioButtons("fac_rotation", label = "Rotation:", fac_rotation, selected = 'Varimax'),
    actionButton("fac_savescores", "Save scores")
  	),
		helpModal('Factor analysis','fullFactor',includeHTML("tools/help/fullFactor.html"))
	)
}

summary.fullFactor <- function(result) {

	cat("Factor loadings matrix:\n")
	print(result$loadings, cutoff = input$fac_cutoff, digits = 3)

	communalities <- data.frame(1 - result$uniqueness)
	colnames(communalities) <- ""
	cat("\nAttribute communalities:\n")
	print(communalities, digits = 3)

	cat("\nFactor scores (max 30 shown):\n")
	scores <- as.data.frame(result$scores)
	print(scores[1:min(nrow(scores),30),, drop = FALSE], digits = 3)

}

fullFactor_fancy_tab <- function() {

	if(input$fac_condFormat) {

		res <- fullFactor()
		if(!is.null(res)) {

			res <- round(res$loading[],3)
			abs_res <- abs(res)
			row_max <- apply(abs_res, 1, max)

			ind_max <- which(abs_res == row_max , arr.ind = TRUE)
			ind_not_max <- which(abs_res < row_max, arr.ind = TRUE)
			emphasize.strong.cells(ind_max)
			emphasize.cells(ind_not_max)
			pander.return(res, style='rmarkdown', split.tables=Inf)
		} else {
			NULL
		}
	} else {
		NULL
	}
}

plot.fullFactor <- function(result) {

	if(result$factors < 2) return()

	df <- round(as.data.frame(result$loadings[]),3)
	rnames <- rownames(df)
	cnames <- colnames(df)
	plots <- list()
	pnr <- 1
	ab_df <- data.frame(a=c(0,0), b=c(1, 0))

	for(i in 1:(length(cnames)-1)) {
		for(j in (i+1):length(cnames)) {

			i_name <- cnames[i]
			j_name <- cnames[j]

		  df2 <- cbind(df[, c(i_name,j_name)],rnames)
  		plots[[pnr]] <- ggplot(df2, aes_string(x = i_name, y = j_name, color = 'rnames', label = 'rnames')) + geom_text() + theme(legend.position = "none") +
  			xlim(-1,1) + ylim(-1,1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
  		pnr <- pnr + 1
  	}
	}

	print(do.call(grid.arrange, c(plots, list(ncol = 1))))
}

fullFactor <- reactive({

	if(is.null(input$factor_vars) || length(input$factor_vars) < 2) return("Please select two or more variables")
	if(is.null(input$fac_number)) return("Number of factors should be > 1.")

	ret_text <- "This analysis requires a multiple variables of type numeric or integer. Please select another dataset."
	if(is.null(inChecker(c(input$factor_vars)))) return(ret_text)

	dat <- getdata()[,input$factor_vars]
	if(nrow(dat) < ncol(dat)) return("Data has more variables than observations. Please reduce the number of variables.")

	nrFac <- max(1,as.numeric(input$fac_number), na.rm = TRUE)
	if(nrFac > ncol(dat)) {
		cat("The number of factors cannot exceed the number of variables.\n")
		nrFac <- ncol(dat)
	}

	if(input$fac_method == 'PCA') {
		fres <- principal(dat, nfactors=nrFac, rotate=input$fac_rotation, scores=TRUE, oblique.scores=FALSE)
	} else {
		fres <- factanal(dat, nrFac, rotation=input$fac_rotation, scores='regression')
		fres$rotation <- input$fac_rotation
	}

	# nr.plots <- factorial(c(nrFac,2))
	# fres$plotHeight <- 650 * (nr.plots[1] / nr.plots[2])
	nr.plots <- (nrFac * (nrFac - 1)) / 2
	fres$plotHeight <- 400 * nr.plots
	fres$plotWidth <- 400
	fres
})

# save cluster membership when action button is pressed
observe({
	if(is.null(input$fac_savescores) || input$fac_savescores == 0) return()
	isolate({
		if(is.character(fullFactor())) return()
		facscores <- data.frame(fullFactor()$scores)
		changedata(facscores, paste0("fac",1:input$fac_number))
	})
})
