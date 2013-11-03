download.file_win <- function(url,f) {
  download.file(paste0(url,f),f, quiet = TRUE)
}

download.file_mac <- function(url,f) {
  download.file(paste0(url,f),f, method = 'curl', quiet = TRUE, extra = '-L')
}

update_app <- function(url) {

  os_type <- .Platform$OS.type

  if (os_type == 'windows') {
    # copied from Shiny source code
    # mySI2 <- `::`(utils, 'setInternet2')
    # internet2_start <- mySI2(NA)
    # on.exit(mySI2(internet2_start))
    # mySI2(TRUE)
    download <- download.file_win
  } else { 
    download <- download.file_mac
    # try_remote <- try(download.file(paste0(url,f),f, method = 'curl', quiet = TRUE, extra = '-L'), silent = TRUE)
  }

  f <- 'dbox_remote.rda'
  try_remote <- try(download(url,f), silent = TRUE)

  if(!is(try_remote, 'try-error')) {

    dbox_local <- file.info(list.files(recursive = TRUE, include.dirs = TRUE))
    load(f)

    if(dim(dbox_remote)[1] == dim(dbox_local)[1]) {
      if(sum(dbox_remote$mtime == dbox_local$mtime) == dim(dbox_local)[1]) stop
    }

    # new dirs
    rn_local_dirs <- rownames(dbox_local[dbox_local$isdir == TRUE, ])
    rn_remote_dirs <- rownames(dbox_remote[dbox_remote$isdir == TRUE, ])
    new_dirs <- rn_remote_dirs[!(rn_remote_dirs %in% rn_local_dirs)]

    for(d in new_dirs) {
      dir.create(d)
    }

    # new files
    rn_local_files <- rownames(dbox_local[dbox_local$isdir == FALSE, ])
    rn_remote_files <- rownames(dbox_remote[dbox_remote$isdir == FALSE, ])
    new_files <- rn_remote_files[!(rn_remote_files %in% rn_local_files)]

    files2get <- new_files

    # existing files
    existing_files <- rn_remote_files[rn_remote_files %in% rn_local_files]
    files2get <- c(files2get, existing_files[dbox_remote[existing_files,]$mtime > dbox_local[existing_files,]$mtime])

    for(f in files2get) {
      cat(paste("Getting file",f,"\n"))
      download(url,f)
    }
  }
}

# installing the required packages 
options(repos = c(CRAN = "http://cran.rstudio.com"))
libs <- c("shiny", "knitr", "Hmisc", "car", "tools", "gridExtra", "markdown", "R.utils", "psych", "rela", "arm", "plyr", "reshape2", "vegan", "ggplot2", "lubridate", "pander")

if('shiny' %in% rownames(installed.packages()) || packageVersion('shiny') < '0.8') {
  # make sure they have .80 or greater installed
  install.packages('shiny')
}

available <- libs %in% rownames(installed.packages())
inst.libs <- libs[!available]
if(length(inst.libs) != 0) {
  install.packages(inst.libs, dependencies = TRUE)
}

# full install 
os_type <- .Platform$OS.type
if (os_type == 'windows') {
  dir.create('~/../Desktop/radyant/', showWarnings = FALSE)
  setwd('~/../Desktop/radyant/')
} else {
  dir.create('~/Desktop/radyant/', showWarnings = FALSE)
  setwd('~/Desktop/radyant/')
}

require(shiny)

# getting the Radyant files
suppressWarnings(update_app('http://vnijs.rady.ucsd.edu/site_media/R/radyant/inst/marketing/'))

q("ask")