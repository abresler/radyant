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


update_app <- function(url) {

  mtime_file <- 'dbox_remote.rda'
  try_remote <- try(download(paste0(url,mtime_file),mtime_file), silent = TRUE)

  if(!is(try_remote, 'try-error')) {

    dbox_local <- file.info(list.files(recursive = TRUE, include.dirs = TRUE))
    load(mtime_file)

    if(dim(dbox_remote)[1] == dim(dbox_local)[1]) {
      if(dbox_remote$mtime == dbox_local$mtime) stop
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
      try(download(paste0(url,f),f), silent = TRUE)
    }
  }
}

# url <- 'https://raw.github.com/mostly-harmless/radyant/master/inst/marketing/'
setwd('~/Desktop/test/')
update_app('https://raw.github.com/mostly-harmless/radyant/master/inst/marketing/')
shiny::runApp('.')

