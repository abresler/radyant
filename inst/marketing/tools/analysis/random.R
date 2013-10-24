###############################
# sampling and assignment
###############################

output$rnd_var <- renderUI({
  vars <- varnames()
	isChar <- "character" == getdata_class()
 	vars <- vars[isChar]
  if(length(vars) == 0) return()
  selectInput(inputId = "rnd_var", label = "Variable (select one):", choices = vars, selected = NULL, multiple = FALSE)
})

output$rnd_block <- renderUI({
  vars <- varnames()
	isFct <- "factor" == getdata_class()
 	vars <- vars[isFct]
  if(length(vars) == 0) return()
  selectInput(inputId = "rnd_block", label = "Block variable (select one):", choices = c("None",vars), selected = NULL, multiple = FALSE)
})


ui_random <- function() {
  list(wellPanel(
 	 	uiOutput("rnd_var"),
	  radioButtons(inputId = "rnd_sample", label = "", c("Sample" = "sample", "Assign" = "assign"), selected = "Sample"),
	  conditionalPanel(condition = "input.rnd_sample == 'sample'",
	   numericInput("rnd_sample_size", "Sample size:", min = 1, value = 1)
  	),
	  conditionalPanel(condition = "input.rnd_sample != 'sample'",
	  	numericInput("rnd_nrCond", "Number of conditions:", min = 2, value = 2),
	 	 	uiOutput("rnd_block"),
	    actionButton("rnd_save_treatment", "Save treatment")
  	)
  	),
	 	helpModal('Random','random',includeHTML("tools/help/random.html"))
 	)
}

summary.random <- function(result) {
	if(input$rnd_sample == 'sample') {
		cat("Selected units:\n")
		print(result$sample)
		cat("\nAll units:\n")
		print(result$dat)
		} else {
			cat("Assigned:\n")
			print(result)
		}
}

plot.random <- function(result) {
	"Relevant output is in the Summary tab."
}

random <- reactive({

	ret_text <- "This analysis requires a variable of type character. Enteries should be unique (i.e., no duplicates). Please select another dataset."
	if(is.null(input$rnd_var)) return(ret_text)
	if(is.null(inChecker(c(input$rnd_var)))) return(ret_text)
	if(is.na(input$rnd_sample_size)) return("Please select a sample size of 1 or greater.")

	# example list of names obtained from http://listofrandomnames.com
	dat <- getdata()

	if(input$rnd_sample == 'sample') {

		selDat <- dat[sample(1:nrow(dat), input$rnd_sample_size),, drop = FALSE]
		return(list(sample = selDat, dat = dat))
	} else {

		dat$treatment <- 0

		# <<- needed else ddply doesn't know where to 'look'
		nrCond <<- 1:input$rnd_nrCond

		if(!is.null(input$rnd_block) && input$rnd_block != "None") {

			# dat <- rndnames
			# dat$treatment <- 0
			# input <- list()
			# input$rnd_var <- 'Names'
			# input$rnd_block <- 'Gender'
			# nrCond <- 1:5

			# adapted from http://stackoverflow.com/questions/5399773/how-to-generate-a-random-treatment-variable-by-factor
			 dat <- ddply(dat, c(input$rnd_block), transform, 
			 		treatment = replace(treatment, sample(seq_along(treatment)), nrCond))

		} else {

			dat$treatment <- replace(dat$treatment, sample(seq_along(dat$treatment)), nrCond)
		}

		nrCond <<- NULL
		dat
	}
})

observe({
	if(is.null(input$rnd_save_treatment) || input$rnd_save_treatment == 0) return()
	isolate({
		if(is.character(random())) return()
		changedata(data.frame(as.factor(random()$treatment)), "treatment")
	})
})

# levs <- levels(tulsa_age$age)
# levels(tulsa_age$age) <- levs[c(1,3,4,5,6,2)]
# levels(tulsa_age$age)

# levs <- levels(tulsa_age$rc.age)
# levels(tulsa_age$rc.age) <- levs[c(1,3,2)]
# levels(tulsa_age$rc.age)
