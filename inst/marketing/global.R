# avoid breaks in R-output print and show JSON packets transferred
# over websockets
options(width = 150, shiny.trace=TRUE, digits = 3)

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)

options(repos = c("http://cran.rstudio.com/"))
libs <- c("shiny", "car", "AER", "Ecdat", "foreign", "tools", "ggplot2", "gridExtra", "markdown", "R.utils", "psych", "rela", "arm", "xts", "plyr", "reshape", "vegan")
available <- suppressWarnings(suppressPackageStartupMessages(sapply(libs, require, character.only=TRUE)))
inst.libs <- libs[available == FALSE]
if(length(inst.libs) != 0) {
	install.packages(inst.libs, dependencies = TRUE)
	suppressWarnings(suppressPackageStartupMessages(sapply(inst.libs, require, character.only=TRUE)))
}


# This is a wrapper for download.file and has the same interface.
# The only difference is that, if the protocol is https, it changes the
# download settings, depending on platform.
download <- function(url, ...) {
  # First, check protocol. If http or https, check platform:
  if (grepl('^https?://', url)) {
    
    # If Windows, call setInternet2, then use download.file with defaults.
    if (.Platform$OS.type == "windows") {
      # If we directly use setInternet2, R CMD CHECK gives a Note on Mac/Linux
      mySI2 <- `::`(utils, 'setInternet2')
      # Store initial settings
      internet2_start <- mySI2(NA)
      on.exit(mySI2(internet2_start))
      
      # Needed for https
      mySI2(TRUE)
      download.file(url, ...)
      
    } else {
      # If non-Windows, check for curl/wget/lynx, then call download.file with
      # appropriate method.
      
      if (nzchar(Sys.which("wget")[1])) {
        method <- "wget"
      } else if (nzchar(Sys.which("curl")[1])) {
        method <- "curl"
        
        # curl needs to add a -L option to follow redirects.
        # Save the original options and restore when we exit.
        orig_extra_options <- getOption("download.file.extra")
        on.exit(options(download.file.extra = orig_extra_options))
        
        options(download.file.extra = paste("-L", orig_extra_options))
        
      } else if (nzchar(Sys.which("lynx")[1])) {
        method <- "lynx"
      } else {
        stop("no download method found")
      }
      
      download.file(url, method = method, ...)
    }
    
  } else {
    download.file(url, ...)
  }
}

# setwd("/Users/vnijs/Dropbox/radyant/inst/marketing")

sync_shiny_app <- function() {

  try_remote <- try(download('https://raw.github.com/mostly-harmless/radyant/master/inst/marketing/dbox_remote.rda','dbox_remote.rda'), silent = TRUE)

  if(!is(try_remote, 'try-error')) {

    dbox_local <- file.info(list.files(recursive = TRUE))
    # dbox_remote <- dbox_local
    # save(dbox_remote, file = "dbox_remote.rda")
    load('dbox_remote.rda')

    files2get <- c()

    # new files
    rn_local <- rownames(dbox_local)
    rn_remote <- rownames(dbox_remote)

    new_files <- !(rn_remote %in% rn_local))
    if(sum(new_files) > 0) {
      files2get <- rn_remote[new_files]
    }

    existing_files <- rn_remote %in% rn_local

    # newer_files <- dbox_remote$mtime > dbox_local$mtime

  }
}



# setting up a few standard datasets to play with 
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)

# Our datasets can change over time (i.e. the changedata function). Therefore,
# these need to be reactive values; otherwise, the other reactive functions
# and outputs that depend on these datasets won't know that they are changed.

# Note that we never get or assign the "original" copies of mtcars, morley, 
# or rock. This way, all user sessions are independent from each other 

values <- reactiveValues()
values$mtcars <- mtcars
values$morley <- morley
values$rock <- rock

n <- nrow(diamonds)

values$diamonds <- diamonds[sample(1:n,3000),]

datasets <- c("mtcars", "diamonds", "rock")

# loading list of data.frame in the car package
# listPackData <- function(packs) {
# 	libnames <- c('')
# 	lib <- c('')
# 	for(ipack in packs) {
# 		ilib <- data(package = ipack)$results
# 		libnames <- c(libnames, paste(ilib[,'Package'],ilib[,'Item'], sep = '-'))
# 		lib <- c(lib,ilib[,'Item'])
# 	}
# 	names(lib) <- libnames
# 	as.list(lib)
# }

# packDataSets <- listPackData(c('AER','Ecdat'))

# pds <- list()
# for(pd in 2:length(packDataSets)) {
# 	data(list = packDataSets[[pd]])
# 	dat <- get(packDataSets[[pd]])
# 	if(!is.null(colnames(dat))) {
# 		pds <- c(pds,packDataSets[pd])
# 	}
# }
# # packDataSets <- packDataSets[-which(packDataSets == "DutchSales")]
# # packDataSets <- c(list(''=''),pds)
# packDataSets <- c('',pds)
# save(packDataSets, file = '~/Desktop/packDataSets.rda')

load('data/packDataSets.rda')

lastLoaded <- "" 		

getTool <- function(inputId) {
  tagList(
    tags$head(tags$script(src = "js/navbar.js")),
    tags$html(includeHTML('www/navbar.html'))
  )
}

# function to render .Rmd files into html on-the-fly
includeRmd <- function(path){
  if (!require(knitr))
    stop("knitr package is not installed")
  if (!require(markdown))
    stop("Markdown package is not installed")
  # shiny:::dependsOnFile(path)
  contents <- paste(readLines(path, warn = FALSE), collapse = '\n')
  html <- knitr::knit2html(text = contents, fragment.only = TRUE)
  Encoding(html) <- 'UTF-8'
  HTML(html)
}

helpPopup <- function(title, content, placement=c('right', 'top', 'left', 'bottom'), 
  trigger=c('click', 'hover', 'focus', 'manual')) {

  tagList(
    singleton(tags$head(tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })"))),
    tags$a(href = "#", `data-toggle` = "popover", title = title, `data-content` = content,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1], 
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1], tags$i(class="icon-question-sign"))
  )
}

helpModal <- function(title, link, content) {
  html <- sprintf("<div id='%s' class='modal hide fade in' style='display: none; '>
                   <div class='modal-header'><a class='close' data-dismiss='modal'>×</a>
                   <h3>%s</h3>
                   </div>
                   <div class='modal-body'>%s</div>
                   </div>
                   <a data-toggle='modal' href='#%s' class='icon-question-sign'></a>", link, title, content, link)
  HTML(html)
}

# Simulate a big data-file
# n <- 200000
# n.var <- 100
# bigsimdat <- data.frame(matrix(rnorm(n*n.var),nrow = n, ncol <- n.var))
# save(bigsimdat,file = "data/bigsimdat.rda")