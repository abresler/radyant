################################################################
# non-reactive functions used in radyant
################################################################
varnames <- function() {
	if(is.null(input$datasets)) return()

	dat <- getdata()
	cols <- colnames(dat)
	names(cols) <- paste(cols, " {", sapply(dat,class), "}", sep = "")
	cols
}

changedata <- function(addCol = list(NULL), addColName = "") {
	# change data as specified
	if(addColName[1] == "") return()
  # isolate ensures no reactive dependencies are used
  isolate({
  	if(length(addCol) == 1 && is.null(addCol[[1]])) return(values[[input$datasets]][,addColName] <- addCol)
  	if(nrow(getdata()) == nrow(addCol)) return(values[[input$datasets]][,addColName] <- addCol)
  })
}

# getdata <- function(dataset = input$datasets) {
#   values[[dataset]]
# }	

getdata <- reactive({
	if(is.null(input$data_filter)) {
		return(values[[input$datasets]])
	} else {
		return(values[[input$datasets]])
		# dat <- values[[input$datasets]]
		# dat[dat[,input$data_filter], ]
	}
})

getdata_class <- reactive({
	# sapply(values[[input$datasets]], class)
	c <- sapply(values[[input$datasets]], function(x) class(x)[1])
	gsub("ordered","factor", c)
})

# myfun <- function(data = mtcars) {
# 	c <- sapply(data, function(x) class(x)[1])
# 	gsub("ordered","factor", c)
# }

# myfun()
# t <- myfun(diamonds)
# 'factor' == t




date2character <- function(dat = NULL) {
	isDate <- sapply(dat, is.Date)
	if(sum(isDate) > 0) dat[,isDate] <- sapply(dat[,isDate], as.character)
	dat
}

loadUserData <- function(filename, uFile, type) {

	ext <- file_ext(filename)
	objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(filename))
	ext <- tolower(ext)

	if(ext == 'rda' || ext == 'rdata') {
		# objname will hold the name of the object inside the R datafile
	  objname <- robjname <- load(uFile)
		values[[robjname]] <- data.frame(get(robjname)) 	# only work with data.frames
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
	  # assign(robj, getdata()[,input$columns])
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
  if(!is.null(input$xls_paste) && input$xls_paste != '') {
		values[['xls_data']] <- as.data.frame(read.table(header=T, text=input$xls_paste, sep="\t"))
    values[['datasetlist']] <- unique(c('xls_data',values[['datasetlist']]))
	}

	# clean out the copy-and-paste box once the data has been stored
 	updateTextInput(session = session, inputId = "xls_paste", label = "", '')

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
# Data reactives - view, plot, transform data, and log your work
################################################################
output$dataexample <- renderTable({
	if(is.null(input$datasets)) return()

	dat <- date2character(getdata())

	# Show only the first 20 rows
	nr <- min(20,nrow(dat))
	data.frame(dat[1:nr,, drop = FALSE])
})

output$dataviewer <- renderTable({
	if(is.null(input$datasets) || is.null(input$columns)) return()

	# dat <- getdata()
	dat <- date2character(getdata())

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
				parse_selcom <- try(parse(text = selcom)[[1]], silent = TRUE)
				if(!is(parse_selcom, 'try-error')) {
					seldat <- try(eval(parse(text = paste("subset(dat,",selcom,")")[[1]])), silent = TRUE)
					if(is.data.frame(seldat)) {
						# return(seldat[, input$columns, drop = FALSE])
						dat <- seldat
						seldat <- NULL
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
	data.frame(dat[input$nrRows[1]:nr, input$columns, drop = FALSE])

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

}, width=650, height=plotHeight)
