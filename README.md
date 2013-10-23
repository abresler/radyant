## Data analytics using Shiny

Interactive data analytics using [R](http://www.r-project.org/) and [Shiny](http://www.rstudio.com/shiny/) by [Rstudio](http://www.rstudio.com/). 

### Todo:
- Create help files
- Use dplyr to explore and summarize data
- Use knitr to log analysis output
- etc. etc.

### Install the marketing app to run locally

- Required: [R](http://cran.rstudio.com/), version 3.0.2 or later
- Required: Shiny, version 0.7.0 or later
- Required: A modern browser (e.g., Chrome, Firefox, or Safari). Internet Explorer is not supported.
- Suggested: [Rstudio](http://www.rstudio.com/ide/download/)

Start R(studio) and copy-and-paste the commands below to create a directory 'radyant' on your desktop:

	source('http://vnijs.rady.ucsd.edu/site_media/R/update.R')

Once the installation has completed use the command below to start the app:

	# on windows
	shiny::runApp('~/../Desktop/radyant/')

 	# on mac
	shiny::runApp('~/Desktop/radyant/')

### License
The radyant package is licensed under the GPLv3. See the files listed below for additional details.

- COPYING - radyant package license (GPLv3)
- NOTICE - Copyright notices for additional included software
