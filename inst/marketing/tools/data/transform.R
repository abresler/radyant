# UI-elements for transform
output$tr_columns <- renderUI({
	cols <- varnames()
	selectInput("tr_columns", "Select column(s):", choices  = as.list(cols), selected = NULL, multiple = TRUE)
})

revFactorOrder <- function(x) {
	x <- as.factor(x)
	factor(x, levels=rev(levels(x)))
}

standardize_1sd <- function(x) {
	if(is.factor(x)) return(rescale(x))
	if(is.numeric(x)) return(scale(x))
	x
}

centerVar <- function(x) {
	if(is.factor(x)) return(rescale(x))
	if(is.numeric(x)) return(x - mean(x, na.rm = TRUE))
	x
}

sq <<- function(x) x^2
inv <<- function(x) 1/x
st1 <<- standardize_1sd
st2 <<- rescale
cent <<- centerVar 
bin2 <<- function(x) cut(x,2)
bin10 <<- function(x) cut(x,10)
fct <<- as.factor
rfct <<- revFactorOrder
num <<- as.numeric
ch <<- as.character
# d <<- as.Date
d_mdy <<- function(x) as.Date(mdy(as.character(x)))
d_dmy <<- function(x) as.Date(dmy(as.character(x)))
d_ymd <<- function(x) as.Date(ymd(as.character(x)))

# trans_options <- list("None" = "none", "Remove" = "", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt", "Center" = "cent", "Standardize (1-sd)" = "st1", 
# trans_options <- list("None" = "", "Remove" = "remove", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt", "Center" = "cent", "Standardize (1-sd)" = "st1", 
trans_options <- list("None" = "", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt", "Center" = "cent", "Standardize (1-sd)" = "st1", 
	"Standardize (2-sd)" = "st2","Invert" = "inv", "Bin 2" = "bin2", "Bin10" = "bin10", "As factor" = "fct", "Rev factor order" = "rfct", "As number" = "num", "As character" = "ch", 
	"As date (mdy)" = "d_mdy", "As date (dmy)" = "d_dmy", "As date (ymd)" = "d_ymd")

ui_Transform <- function() {
	# Inspired by Ian Fellow's transform ui in JGR/Deducer
  list(wellPanel(
    uiOutput("tr_columns"),

   	radioButtons("tr_changeType", "", c("Change" = "change", "Create" = "create", "Paste" = "paste", "Recode" = "recode", "Rename" = "rename", "Remove" = "remove"), selected = "Change"),
   	# radioButtons("tr_changeType", "", c("Change" = "change", "Rename" = "rename", "Add" = "add", "Recode" = "recode", "Remove" = "remove"), selected = "Change"),
    conditionalPanel(condition = "input.tr_changeType == 'change'",
	    selectInput("tr_transfunction", "Change columns:", trans_options)
    ),
    conditionalPanel(condition = "input.tr_changeType == 'create'",
	    returnTextInput("tr_transform", "Create (e.g., x = y - z):", '')
	    # textInput("tr_transform", "Create (e.g., x = y - z):", ''), 
  	  # actionButton("tr_transform_sub", "Go")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'paste'",
    	HTML("<label>Paste from Excel:</label>"),
	    tags$textarea(id="tr_copyAndPaste", rows=3, cols=40, "")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'recode'",
	    returnTextInput("tr_recode", "Recode (e.g., lo:20 = 1):", '')
	    # textInput("tr_recode", "Recode (e.g., lo:20 = 1):", ''), 
  	  # actionButton("tr_recode_sub", "Go")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'rename'",
	   	returnTextInput("tr_rename", "Rename (separate by ','):", '')
	   	# textInput("tr_rename", "Rename (separate by ','):", '')
    ),

    # actionButton("transfix", "Edit variables in place") # using the 'fix(mtcars)' to edit the data 'inplace'. Looks great from R-ui, not so great from Rstudio
    actionButton("addtrans", "Save changes")
  	), 
		helpModal('Transform','transform',includeMarkdown("tools/help/transform.md"))
	)
}

transform_main <- reactive({

	if(input$datatabs != 'Transform') return()
	if(is.null(input$tr_copyAndPaste) || is.null(input$tr_transform)) return()
	if(is.null(input$datasets) || (is.null(input$tr_columns) && input$tr_copyAndPaste == '' && input$tr_transform == '')) return()

	dat <- getdata()
	if(!is.null(input$tr_columns)) {

		if(!all(input$tr_columns %in% colnames(dat))) return()
		dat <- data.frame(dat[, input$tr_columns, drop = FALSE])
		if(input$tr_transfunction != '' && input$tr_transfunction != 'remove') {
			cn <- c(colnames(dat),paste(input$tr_transfunction,colnames(dat), sep="."))
			dat <- cbind(dat,colwise(input$tr_transfunction)(dat))
			colnames(dat) <- cn
		}
	}

	if(input$tr_recode != '') {
		recom <- input$tr_recode
		recom <- gsub(" ", "", recom)
		recom <- gsub("\"","\'", recom)

		newvar <- try(do.call(car::recode, list(dat[,input$tr_columns[1]],parse(text = recom))), silent = TRUE)
		if(!is(newvar, 'try-error')) {

			cn <- c(colnames(dat),paste("rc",input$tr_columns[1], sep="."))
			dat <- cbind(dat,newvar)
			colnames(dat) <- cn
			return(dat)
		}
	}

	if(input$tr_copyAndPaste != '') {
		cpdat <- read.table(header=T, text=input$tr_copyAndPaste)
		cpname <- names(cpdat)
		if(sum(cpname %in% colnames(dat)) > 0) names(cpdat) <- paste('cp',cpname,sep = '.')
		if(is.null(input$tr_columns)) return(cpdat)
		if(nrow(cpdat) == nrow(dat)) dat <- cbind(dat,cpdat)
	}

	if(input$tr_rename != '') {
		rcom <- unlist(strsplit(gsub(" ","",input$tr_rename), ","))
		names(dat)[1:length(rcom)] <- rcom
	}

	if(input$tr_transform != '') {
		recom <- input$tr_transform
		recom <- gsub(" ", "", recom)
		recom <- gsub("\"","\'", recom)

		fullDat <- getdata()
		newvar <- try(do.call(within, list(fullDat,parse(text = recom))), silent = TRUE)

		if(!is(newvar, 'try-error')) { 
			nfull <- ncol(fullDat)
			nnew <- ncol(newvar)

			# this won't work properly if the transform command creates a new variable
			# and also overwrites an existing one
			if(nfull < nnew) newvar <- newvar[,(nfull+1):nnew, drop = FALSE]
			if(is.null(input$tr_columns)) return(newvar)
			cn <- c(colnames(dat),colnames(newvar))
			dat <- cbind(dat,newvar)
			colnames(dat) <- cn
		} else if(is.null(input$tr_columns)) {
			return()
		}
	}

	# dat <- mtcar
	# recom <- "t = mpg + cyl"
	# do.call(within, list(dat, parse(text = recom)))

	dat
})

output$transform_data <- renderTable({

	dat <- transform_main()
	if(is.null(dat)) return()

	dat <- data.frame(date2character_dat(dat))
	nr <- min(nrow(dat),10)
	dat[1:nr,, drop = FALSE]
})


output$transform_data <- reactive({

	dat <- transform_main()
	if(is.null(dat)) return()

	dat <- data.frame(date2character_dat(dat))
	nr <- min(nrow(dat),10)
	dat <- dat[1:nr,, drop = FALSE]

	html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
	html <- paste(html, '<label>10 rows shown. See View-tab for details.</label>') 
	sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)

})

output$transform_summary <- renderPrint({

	dat <- transform_main()
	if(is.null(dat)) return(invisible()) 			# ...

	isFct <- sapply(dat, is.factor)
	isNum <- sapply(dat, is.numeric)
	isDate <- sapply(dat, is.Date)
	# isChar <- sapply(getdata(), is.character)

	if(sum(isNum) > 0) {
		cat("\nSummarize numeric variables:\n")
		print(describe(dat[,isNum]))
	}
	if(sum(isFct) > 0) {
		cat("\nSummarize factors:\n")
		print(summary(dat[,isFct]))
	}
	if(sum(isDate) > 0) {
		cat("\nSummarize date variables:\n")
		print(summary(dat[,isDate]))
	}
})

observe({
	if(is.null(input$addtrans) || input$addtrans == 0) return()
	isolate({
		dat <- transform_main()
		if(input$tr_changeType == 'remove') {
			changedata(addColName = colnames(dat))
		} else {
			changedata(dat, colnames(dat))
		}

		# reset the values once the changes have been applied
	 	updateTextInput(session = session, inputId = "tr_transform", label = "Create (e.g., y = x - z):", '')
	 	updateTextInput(session = session, inputId = "tr_recode", label = "Recode (e.g., lo:20 = 1):", '')
	 	updateTextInput(session = session, inputId = "tr_rename", label = "Rename (separate by ','):", value = '')
	 	updateTextInput(session = session, inputId = "tr_copyAndPaste", label = "", '')
		updateSelectInput(session = session, inputId = "tr_transfunction", choices = trans_options, selected = "None")
	})
})
