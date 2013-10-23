################################################################
# non-reactive functions used in radyant
################################################################
changedata <- function(addCol = list(NULL), addColName = "") {
	# change data as specified
	if(addColName[1] == "") return()
  # isolate ensures no reactive dependencies are used
  isolate({
  	if(length(addCol) == 1 && is.null(addCol[[1]])) {
  		return(values[[input$datasets]][,addColName] <- addCol)
  	} else if(nrow(getdata()) == nrow(addCol)) {
	  	return(values[[input$datasets]][,addColName] <- addCol)
  	}
  })
}

inChecker <- function(tocheck) {

	# checking if variables are in the selected dataset
	for(var in tocheck) {
		if(!var %in% varnames()) return(NULL)
	}

	return('Good stuff')
}

getdata <- reactive({
	values[[input$datasets]]

	# if(is.null(input$data_filter)) {
	# 	return(values[[input$datasets]])
	# } else {
	# 	return(values[[input$datasets]])
		# dat <- values[[input$datasets]]
		# dat[dat[,input$data_filter], ]
	# }
})

getdata_class <- reactive({
	# don't use isolate here or values won't change when the dataset is changed
	cls <- sapply(getdata(), function(x) class(x)[1])
	gsub("ordered","factor", cls)
})

# varnames <- function() {
varnames <- reactive({
	# if(is.null(input$datasets)) return()
	dat <- getdata_class()
	vars <- names(dat)
	names(vars) <- paste(vars, " {", dat, "}", sep = "")
	vars
})

date2character <- reactive({

	dat <- getdata()
  isDate <- "Date" == getdata_class()
	if(sum(isDate) > 0) {
		# needed because xtable doesn't like dates
		dat[,isDate] <- sapply(dat[,isDate], as.character)
	}
	dat
})

date2character_dat <- function(dat) {

  isDate <- sapply(dat, is.Date)
	if(sum(isDate) > 0) {
		# needed because xtable doesn't like dates
		dat[,isDate] <- sapply(dat[,isDate], as.character)
	}
	dat
}

# xtable <- function(x, ...) {
#    for (i in which(sapply(x, function(y) !all(is.na(match(c("POSIXt","Date"),class(y))))))) x[[i]] <- as.character(x[[i]])
#    xtable::xtable(x, ...)
# }

loadUserData <- function(filename, uFile, type) {

	ext <- file_ext(filename)
	# objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(filename))
	objname <- sub(paste(".",ext,sep = ""),"",basename(filename))
	ext <- tolower(ext)

	if(ext == 'rda' || ext == 'rdata') {
		# objname will hold the name of the object inside the R datafile
	  # objname <- robjname <- load(uFile)
		# values[[robjname]] <- data.frame(get(robjname)) 	# only work with data.frames
	  robjname <- load(uFile)
		values[[objname]] <- data.frame(get(robjname)) 	# only work with data.frames
	}

	if(values[['datasetlist']][1] == '') {
    values[['datasetlist']] <- c(objname)
	} else {
    values[['datasetlist']] <- unique(c(objname,values[['datasetlist']]))
	}

	if(ext == 'sav') {
		values[[objname]] <- as.data.frame(as.data.set(spss.system.file(uFile)))
	} else if(ext == 'dta') {
		values[[objname]] <- read.dta(uFile)
	} else if(ext == 'csv') {
		values[[objname]] <- read.csv(uFile, header=input$header, sep=input$sep)
	}
}

loadPackData <- function(pFile) {

  robjname <- data(list = pFile)
	dat <- get(robjname)

	if(pFile != robjname) return("R-object not found. Please choose another dataset")

	if(is.null(ncol(dat))) {
		return()
	}

	values[[robjname]] <- dat

	if(values[['datasetlist']][1] == '') {
    values[['datasetlist']] <- c(robjname)
	} else {
    values[['datasetlist']] <- unique(c(robjname,values[['datasetlist']]))
	}
}

#################################################
# reactive functions used in radyant
#################################################

output$downloadData <- downloadHandler(
	filename = function() { paste(input$datasets,'.',input$saveAs, sep='') },
  content = function(file) {

	  ext <- input$saveAs
	  robj <- input$datasets

	  # only save selected columns
	  assign(robj, getdata())

		if(ext == 'rda') {
	    save(list = robj, file = file)
		} else if(ext == 'csv') {
			write.csv(get(robj), file)
		}
  }
)

output$datasets <- renderUI({

  inFile <- input$uploadfile
  if(!is.null(inFile)) loadUserData(inFile$name, inFile$datapath, input$dataType)

  # if(input$xls_paste != '') {
 #  if(!is.null(input$xls_paste) && input$xls_paste != '') {
	# 	values[['xls_data']] <- as.data.frame(read.table(header=T, text=input$xls_paste, sep="\t"))
 #    values[['datasetlist']] <- unique(c('xls_data',values[['datasetlist']]))
	# }

	# clean out the copy-and-paste box once the data has been stored
 	# updateTextInput(session = session, inputId = "xls_paste", label = "", '')

	# # loading package data
	# if(input$packData != "") {
	# 	if(input$packData != lastLoaded) {
	# 		loadPackData(input$packData)
	# 		lastLoaded <<- input$packData 
	# 	}
	# }

	# Drop-down selection of data set
	# selectInput(inputId = "datasets", label = "Datasets:", choices = datasets, selected = datasets[1], multiple = FALSE)
	selectInput(inputId = "datasets", label = "Datasets:", choices = values$datasetlist, selected = values$datasetlist[1], multiple = FALSE)
})

output$removeDataset <- renderUI({
	# Drop-down selection of data set
	selectInput(inputId = "removeDataset", label = "Remove data from memory:", choices = values$datasetlist, selected = NULL, multiple = TRUE)
})

output$packData <- renderUI({
	selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
})

output$columns <- renderUI({
	cols <- varnames()
	selectInput("columns", "Select columns to show:", choices  = as.list(cols), selected = names(cols), multiple = TRUE)
})

output$nrRows <- renderUI({
	if(is.null(input$datasets)) return()
	dat <- getdata()

	# number of observations to show in dataview
	nr <- nrow(dat)
	sliderInput("nrRows", "Rows to show:", min = 1, max = nr, value = c(1,min(15,nr)), step = 1)
})

################################################################
# Main reactives 
################################################################

output$htmlDataExample <- reactive({
	if(is.null(input$datasets)) return()

	# dat <- date2character()
	dat <- getdata()

	# Show only the first 20 rows
	nr <- min(15,nrow(dat))
	dat <- data.frame(dat[1:nr,, drop = FALSE])

	dat <- date2character_dat(dat)

	html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
	# sub("<TABLE border=1>","<table class='table table-condensed'>", html)
	sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)

})


# output$dataviewer <- renderTable({
output$dataviewer <- reactive({
	if(is.null(input$datasets) || is.null(input$columns)) return()

	# dat <- getdata()
	dat <- date2character()

	# not sure why this is needed when files change ... but it is
	# without it you will get errors the invalid columns have been
	# selected
	if(!all(input$columns %in% colnames(dat))) return()

	if(!is.null(input$sub_select) && !input$sub_select == 0) {
		isolate({
			if(input$dv_select != '') {
				selcom <- input$dv_select
				selcom <- gsub(" ", "", selcom)
				if(nchar(selcom) > 30) q()
				if(length(grep("system",selcom)) > 0) q()
				if(length(grep("rm\\(list",selcom)) > 0) q()
					
				# selcom is a valid expression to the subset command
				selcom <- try(parse(text = paste0("subset(dat,",selcom,")")), silent = TRUE)
				if(!is(selcom, 'try-error')) {
					seldat <- try(eval(selcom), silent = TRUE)
					if(is.data.frame(seldat)) {
						dat <- seldat
						seldat <- NULL
						# showing all rows that fit the condition
						updateSliderInput(session = session, "nrRows", "Rows to show:", value = c(1,nrow(dat)))
					}
				} 
			}
		})
	}

	# order data
	if(!is.null(input$view_order) && input$view_order != "None") {
		indx <- order(dat[,input$view_order], decreasing = input$view_order_desc)
		dat <- dat[indx,]
	}

	# Show only the selected columns and no more than 50 rows at a time
	nr <- min(input$nrRows[2],nrow(dat))
	dat <- data.frame(dat[input$nrRows[1]:nr, input$columns, drop = FALSE])

	html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
	sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)

})

update_radyant <- reactive({

	if(!is.null(input$update)) {
		isolate({
			source('update.R')
			# source('ui.R')
			# source('server.R')
			# source('global.R')
		})
	}

})

################################################################
# Output controls for the Summary and Plots tabs
# The tabs are re-used for various tools. Depending on the tool
# selected by the user the appropropriate analaysis function 
# is called.
#
# Naming conventions: The reactive function to be put in the
# code block above must be of the same name as the tool
# in the tools drop-down. See global.R for the current list
# of tools (and tool-names) 
################################################################

# Generate output for the summary tab
# output$summary <- renderText({
output$summary <- renderPrint({
	if(is.null(input$datasets) || input$tool == 'data') return()

	# get the summary function for currently selected tool and feed
	# it the output from the related analysis reactives 
	# get-function structure is used because there may be many
	# sets of tools that will have the same output structure

	# call analysis reactive
	result <- get(input$tool)()

	if(is.character(result)) {
		# used when no analysis is conducted (e.g., no variables selected yet)
		# ret <- cat(result,"\n")
		cat(result,"\n")
	} else {
		# pass analysis results to the summary function
		f <- get(paste("summary",input$tool,sep = '.'))
		# ret <- f(result)
		f(result)
	}

	# query <- parseQueryString(session$clientData$url_search)
  # print(query)
	# ret

})

plotHeight <- function(height = 650) {

 	height <- try(get(input$tool)()[['plotHeight']], silent = TRUE)
	if(is(height, 'try-error') || is.null(height)) {
		return(650)
	} else {
		return(height)
	}
}

plotWidth <- function(width = 650) {

 	width <- try(get(input$tool)()[['plotWidth']], silent = TRUE)
	if(is(width, 'try-error') || is.null(width)) {
		return(650)
	} else {
		return(width)
	}
}


# Generate output for the plots tab
output$plots <- renderPlot({

	# plotting could be expensive so only done when tab is being viewed
	if(input$tool == 'data' || input$analysistabs != 'Plots') return()

	# call analysis reactive
	result <- get(input$tool)()
	if(!is.character(result)) {
		# pass analysis results to the plotting function
		f <- get(paste("plot",input$tool,sep = '.'))
		f(result)
	} else {
		# used when no analysis is conducted (e.g., no variables selected yet)
		plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = "")
	}

}, width=plotWidth, height=plotHeight)
