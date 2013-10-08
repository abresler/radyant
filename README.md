## Data analytics using Shiny

Interactive data analytics using [R](http://www.r-project.org/) and [Shiny](http://www.rstudio.com/shiny/) by [Rstudio](http://www.rstudio.com/). 

### Todo:
- Add analysis-tools (e.g., perceptual maps, MDS)
- Use knitr to log analysis output
- Create help files
- etc. etc.

### Install the marketing app locally

- Required: [R](http://cran.rstudio.com/), version 3.0.2 or later
- Required: Shiny, version 0.7.0 or later
- Required: A modern browser (e.g., Chrome, Firefox, or Safari). Internet Explorer is not supported.
- Suggested: [Rstudio](http://www.rstudio.com/ide/download/)

Start R(studio) and copy-and-paste the commands below:

#### on windows

	install.packages('shiny', repos = "http://cran.rstudio.com/")
	dir.create('../Desktop/radyant/', showWarnings = FALSE)
	setwd('../Desktop/radyant/')
	source('http://vnijs.rady.ucsd.edu/site_media/R/update.R')
	shiny::runApp('../Desktop/radyant/')

#### on mac

	install.packages('shiny', repos = "http://cran.rstudio.com/")
	dir.create('~/Desktop/radyant/', showWarnings = FALSE)
	setwd('~/Desktop/radyant/')
	source('http://vnijs.rady.ucsd.edu/site_media/R/update.R')
	shiny::runApp('~/Desktop/radyant/')

### License
The radyant package is licensed under the GPLv3. See the files listed below for additional details.

- COPYING - radyant package license (GPLv3)
- NOTICE - Copyright notices for additional included software
