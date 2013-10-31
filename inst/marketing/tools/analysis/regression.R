###############################
# Correlation
###############################

output$cor_var <- renderUI({

  vars <- varnames()
  selectInput(inputId = "cor_var", label = "Select variables:", choices = vars, selected = NULL, multiple = TRUE)
})

ui_correlation <- function() {
  list(wellPanel(
	    uiOutput("cor_var"),
		  selectInput(inputId = "cor_type", label = "Method", choices = c("pearson", "spearman"), selected = "pearson")
	  ),
	 	helpModal('Correlation','correlation',includeHTML("tools/help/correlation.html"))
	)
}

summary.correlation <- function(dat) {

	# cmat <- cor(dat, use="complete.obs", method = input$cor_type)
	# print(as.dist(cmat), digits = 2)

	cmat <- Hmisc::rcorr(as.matrix(dat), type = input$cor_type)
	cat("Correlation matrix:\n")
	print(as.dist(round(cmat$r,2)))
	cat("\np-values:\n")
	print(as.dist(round(cmat$P,2)))
}

plot.correlation <- function(dat) {
	# based mostly on http://gallery.r-enthusiasts.com/RGraphGallery.php?graph=137
	panel.plot <- function(x, y) {
	    usr <- par("usr"); on.exit(par(usr))
	    par(usr = c(0, 1, 0, 1))
	    ct <- cor.test(x,y, method = input$cor_type)
	    sig <- symnum(ct$p.value, corr = FALSE, na = FALSE,
	                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
	                  symbols = c("***", "**", "*", ".", " "))
	    r <- ct$estimate
	    rt <- format(r, digits=2)[1]
	    cex <- 0.5/strwidth(rt)
	    
	    text(.5, .5, rt, cex=cex * abs(r))
	    text(.8, .8, sig, cex=cex, col='blue')
	}
	panel.smooth <- function (x, y) {
    points(x, y)
    abline(lm(y~x), col="red")
    lines(stats::lowess(y~x), col="blue")
	}
	pairs(dat, lower.panel=panel.smooth, upper.panel=panel.plot)
}

correlation <- reactive({
	vars <- input$cor_var
	if(is.null(vars) || (length(vars) < 2)) return("Please select two or more variables")
	if(sum(vars %in% varnames()) != length(vars))  return("")

	dat <- getdata()[,vars]
	data.frame(lapply(dat,as.numeric))
})


################################################################
# OLS
################################################################

output$reg_var1 <- renderUI({
  vars <- varnames()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "reg_var1", label = "Dependent variable:", choices = vars, selected = NULL, multiple = FALSE)
})

output$reg_var2 <- renderUI({

  if(is.null(input$reg_var1)) return()
  vars <- varnames()
 	vars <- vars[-which(vars == input$reg_var1)]
  if(length(vars) == 0) return()
  selectInput(inputId = "reg_var2", label = "Independent variables:", choices = vars, selected = NULL, multiple = TRUE)

})

output$reg_var3 <- renderUI({

  vars <- input$reg_var2
  if(is.null(vars)) return()

  # adding interaction terms as needed 
	if(!is.null(input$reg_intsel) && input$reg_interactions != 'none') vars <- c(vars,input$reg_intsel)

  selectInput(inputId = "reg_var3", label = "Variables to test:", choices = vars, selected = NULL, multiple = TRUE)
})

output$reg_intsel <- renderUI({

  vars <- input$reg_var2
  if(is.null(vars) || length(vars) < 2) return()

  choices <- ""
 	if(vars %in% varnames()) choices <- reg_int_vec(vars,input$reg_interactions)
 	
	selectInput("reg_intsel", label = "", choices = choices, selected = NULL, multiple = TRUE)
})

ui_regression <- function() {
  list(wellPanel(
    uiOutput("reg_var1"),
    uiOutput("reg_var2"),
  	checkboxInput(inputId = "reg_standardize", label = "Standardized coefficients", value = FALSE),
    radioButtons(inputId = "reg_interactions", label = "Interactions:", c("None" = "none", "All 2-way" = "2way", "All 3-way" = "3way"), selected = "None"),
    conditionalPanel(condition = "input.reg_interactions != 'none'",
  		uiOutput("reg_intsel")
  	),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
	    uiOutput("reg_var3"),
	    checkboxInput(inputId = "reg_vif", label = "Calculate VIF-values", value = FALSE),
  	  checkboxInput(inputId = "reg_stepwise", label = "Select variables step-wise", value = FALSE)
  	),
    conditionalPanel(condition = "input.analysistabs == 'Plots'",
      selectInput("reg_plots", "Regression plots:", choices = r_plots, selected = 'coef', multiple = FALSE)
    ),
    actionButton("saveres", "Save residuals")
	  ),
		helpModal('Regression','regression',includeHTML("tools/help/regression.html"))
	)
}



summary.regression <- function(result) {
	print(summary(result), digits = 3)
	if(input$reg_vif) {
		print(vif.regression(result))
		# cat("\n")
	} 
	if(!is.null(input$reg_var3)) {
		if(!input$reg_stepwise) {
			test.regression(result)
		} else {
	  	cat("Model comparisons are not conducted when Stepwise has been selected.\n")
	  }
	}
}

# main functions called from radyant.R
r_plots <- list("Histograms" = "histlist", "Correlations" = "correlations", "Scatter" = "scatterlist", "Dashboard" = "dashboard",
		"Residual vs predictor" = "resid_vs_predictorlist", "Leverage plots" = "leverage_plots", "Coefficient plot" = "coef")
	# "Actual vs Fitted" = 0, "Residuals vs Fitted" = 1, "Residual vs Row order" = 2, "Normal Q-Q" = 3)
# r_plots <- list("Coefficient plot" = "coef", "Actual vs Fitted" = 0, "Residuals vs Fitted" = 1, "Normal Q-Q" = 2, "Scale-Location" = 3,
# 	"Cook's distance" = 4, "Residuals vs Leverage" = 5, "Cook's distance vs Leverage" = 6
# )

plot.regression <- function(result) {

	mod <- fortify(result)

	# require(ggplot2)
	# require(gridExtra)
	# dat <- ideal
	# head(ideal)
	# result <- lm(y ~ x1 + x2 + x3, data = dat)

	# mod <- fortify(result)
	# str(result)
	# head(mod)
	# hist(mod$.hat)

	# input <- list()
	# input$reg_var1 <- "y"
	# input$reg_var2 <- c("x1","x2","x3")
	# vars <- c(input$reg_var1, input$reg_var2)
	vars <- c(input$reg_var1, input$reg_var2)
	dat <- mod[,vars]

	# input$reg_plots = "histlist"
	if(input$reg_plots == "histlist") {
		plots <- list()
		for(i in vars) plots[[i]] <- ggplot(dat, aes_string(x = i)) + geom_histogram()

		p <- suppressMessages(do.call(grid.arrange, c(plots, list(ncol = 2))))
	}

	# input$reg_plots = "correlations"
	if(input$reg_plots == "correlations") {
		# from the EDAT menu
		return(plot.correlation(dat))
	}

	# input$reg_plots = "dashboard"
	if(input$reg_plots == "dashboard") {

		plots <- list()

		df <- data.frame(cbind(mod$.fitted,mod[1]))
		colnames(df) <- c("x","y")
		plots[[1]] <- ggplot(df, aes(x=x, y=y)) + geom_point() + stat_smooth(method="lm", se=TRUE) +
			labs(list(title = "Actual vs Fitted", x = "Fitted values", y = "Actual values"))

		plots[[2]] <- qplot(.fitted, .resid, data = mod) + geom_hline(yintercept = 0) + geom_smooth(se = FALSE) +
			labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals"))

		plots[[3]] <- qplot(y=.resid, x=seq_along(.resid), data = mod) + geom_point() + geom_smooth(se = FALSE) +
			labs(list(title = "Residuals vs Row order", x = "Row order", y = "Residuals"))

		plots[[4]] <- qplot(sample =.stdresid, data = mod, stat = "qq") + geom_abline() +
			labs(list(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals"))

		p <- suppressMessages(do.call(grid.arrange, c(plots, list(ncol = 2))))

	}

	# input$reg_plots = "scatterlist"
	if(input$reg_plots == "scatterlist") {
		plots <- list()
		# for(i in input$reg_var2) plots[[i]] <- ggplot(dat, aes_string(x=i, y=input$reg_var1)) + geom_point() + geom_smooth(method = "lm", size = .75, linetype = "dotdash")
		for(i in input$reg_var2) plots[[i]] <- ggplot(dat, aes_string(x=i, y=input$reg_var1)) + geom_point() + geom_smooth(size = .75, linetype = "dotdash")
		p <- suppressMessages(do.call(grid.arrange, c(plots, list(ncol = 2))))
	}

	# input$reg_plots = "resid_vs_predictorlist"
	if(input$reg_plots == "resid_vs_predictorlist") {
		plots <- list()
		residuals <- mod$.resid
		rdat <- cbind(residuals,dat[,input$reg_var2])
		# for(i in input$reg_var2) plots[[i]] <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_point() + geom_smooth(method = "lm", size = .75, linetype = "dotdash")
		for(i in input$reg_var2) plots[[i]] <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_point() + geom_smooth(se = FALSE)
		p <- suppressMessages(do.call(grid.arrange, c(plots, list(ncol = 2))))
	}

	# input$reg_plots = "leverage_plots"
	if(input$reg_plots == "leverage_plots") {
		return(leveragePlots(result, main = "", ask=FALSE, layout = c(ceiling(length(input$reg_var2)/2),2)))
	}

	if(input$reg_plots == "coef") {
		return(coefplot(result, xlab="", ylab="", main="Coefficient plot", col.pts="blue", CI=2))
	} 

	if(input$reg_plots == 0) {
		df <- data.frame(cbind(mod$.fitted,mod[1]))
		colnames(df) <- c("x","y")
		p <- ggplot(df, aes(x=x, y=y)) + geom_point() + stat_smooth(method="lm", se=TRUE) +
			labs(list(title = "Actual vs Fitted", x = "Fitted values", y = "Actual values"))

	} else if(input$reg_plots == 1) {
		p <- qplot(.fitted, .resid, data = mod) + geom_hline(yintercept = 0) + geom_smooth(se = FALSE) +
			labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals"))

	} else if(input$reg_plots == 2) {
		p <- qplot(y=.resid, x=seq_along(.resid), data = mod) + geom_point() + geom_smooth(method = "lm", size = .75, linetype = "dotdash") +
			labs(list(title = "Residuals vs Row order", x = "Row order", y = "Residuals"))

	} else if(input$reg_plots == 3) {
		p <- qplot(sample =.stdresid, data = mod, stat = "qq") + geom_abline() +
			labs(list(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals"))

	} else if(input$reg_plots == 4) {
		p <- qplot(.fitted, sqrt(abs(.stdresid)), data = mod) + geom_smooth(se = FALSE) +
			labs(list(title = "Scale-Location", x = "Fitted values", y = "Sqrt. standardized residuals"))

	} else if(input$reg_plots == 5) {

		p <- qplot(seq_along(.cooksd), .cooksd, data = mod, geom = "bar", stat="identity") +
			labs(list(title = "Cook's distance", x = "Observation number", y = "Cook's distance"))

	} else if(input$reg_plots == 6) {
		p <- qplot(.hat, .stdresid, data = mod, size = .cooksd) + geom_smooth(se = FALSE, size = 0.5) +
			labs(list(title = "Residuals vs Leverage", x = "Leverage", y = "Standardized residuals", size = "Cook's distance"))

	} else if(input$reg_plots == 7) {
		p <- ggplot(mod, aes(.hat, .cooksd)) + geom_vline(xintercept = 0, colour = NA) +
			geom_abline(slope = seq(0, 3, by = 0.5), colour = "white") + geom_smooth(se = FALSE) +
			geom_point() + labs(list(title = "Cook's distance vs Leverage", x = "Leverage", y = "Cook's distance"))
		 	# p <- qplot(.hat, .cooksd, size = .cooksd / .hat, data = mod) + scale_area()
	}

	# suppressWarnings(print(p))
	suppressMessages(print(p))
}

# analysis reactive
regression <- reactive({

	mod <- regression.mod()

	if(class(mod) != 'lm') return(mod)

	# "Histograms" = "histlist", 
	# "Correlations" = "correlations", 
	# "Scatter" = "scatterlist", 
	# "Dashboard" = "dashboard",
	# "Residual vs predictor" = "resid_vs_predictorlist", 
	# "Leverage plots" = "leverage_plots", 
	# "Coefficient plot" = "coef"

	if(input$reg_plots == 'dashboard') {
		mod$plotHeight <- 650
		mod$plotWidth <- 650
	}

	if(input$reg_plots == 'coef') {
		mod$plotHeight <- 500
		mod$plotWidth <- 500
	}

	nrVars <- length(input$reg_var2) + 1
	if(input$reg_plots == 'histlist') {
		mod$plotHeight <- 325 * ceiling(nrVars / 2)
		mod$plotWidth <- 650
	}

	if(input$reg_plots == 'correlations') {
		mod$plotHeight <- 150 * nrVars
		mod$plotWidth <- 150 * nrVars
	}

	if(input$reg_plots %in% c('scatterlist','leverage_plots','resid_vs_predictorlist')) {
		mod$plotHeight <- 325 * ceiling((nrVars-1) / 2)
		mod$plotWidth <- 650
	}

	mod
})

regression.mod <- reactive({

	ret_text <- "This analysis requires a dependent variable of type integer or numeric and one or more independent variables. Please select another dataset."

	if(is.null(input$reg_var1)) return(ret_text)
	vars <- input$reg_var2
	if(is.null(vars)) return("Please select one or more independent variables.")
	if(is.null(inChecker(c(input$reg_var1, vars)))) return(ret_text)

	# adding interaction terms as needed
	if(input$reg_interactions != 'none') vars <- c(vars,input$reg_intsel)

	dat <- getdata()
	if(input$reg_standardize) dat <- data.frame(lapply(dat,rescale))

	formula <- paste(input$reg_var1, "~", paste(vars, collapse = " + "))
	if(input$reg_stepwise) {
		mod <- step(lm(as.formula(paste(input$reg_var1, "~ 1")), data = dat), scope = list(upper = formula), direction = 'forward')
	} else {
		mod <- lm(formula, data = dat)
	}

	mod
})

# save residuals
observe({
	if(is.null(input$saveres) || input$saveres == 0) return()
	isolate({
		if(is.character(regression())) return()
		changedata(data.frame(regression()$residuals), "residuals")
	})
})

################################################################
# Additional functions
################################################################
reg_int_vec <- function(reg_vars, nway) {
	n <- length(reg_vars)
	iway <- c()
	for(i in 1:(n-1)) {
		for(j in (i+1):n) {
			iway <- c(iway, paste(reg_vars[i],reg_vars[j],sep=":"))
		}
	}
	if(n >= 3 && nway == '3way') {
		for(i in 1:(n-2)) {
			for(j in (i+1):(n-1)) {
				for(k in (j+1):n) {
					iway <- c(iway, paste(reg_vars[i],reg_vars[j],reg_vars[k],sep=":"))
				}
			}
		}
	}
	iway
}

vif.regression <- function(result) {
	if(input$reg_vif) {
		if(length(input$reg_var2) > 1) {
	  	cat("Variance Inflation Factors\n")
	  	VIF <- as.matrix(vif(result))[,1]
	  	VIF <- sort(VIF, decreasing = TRUE)
			t(data.frame(VIF))
		} else {
	  	cat("Insufficient number of independent variables selected to calculate VIF scores\n")
		}
	}
}

test.regression <- function(result) {
	dat <- getdata()
	if(input$reg_standardize) dat <- data.frame(lapply(dat,rescale))

	sub_formula <- ". ~ 1"
	vars <- input$reg_var2
  if(!is.null(input$reg_intsel) && input$reg_interactions != 'none') vars <- c(vars,input$reg_intsel)
	not_selected <- setdiff(vars,input$reg_var3)
	if(length(not_selected) > 0) sub_formula <- paste(". ~", paste(not_selected, collapse = " + "))

	reg_sub <- update(result, sub_formula)
	anova(reg_sub, result, test='F')
}

