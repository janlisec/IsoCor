#load all libraries which are used within the package and stop if not sucessful
# stopifnot(all(
#   sapply(unique(packrat:::ns_imports("IsoCor")[[1]]), library, character.only = TRUE, logical.return=TRUE)
# ))
library('bsplus')
library('DT')
library('htmltools')
library('MALDIquant')
library('plyr')
library('pracma')
library('shiny')
library('shinyalert')
library('shinyjs')
#get package version and date for display
DESC <- readLines("DESCRIPTION")
status_line <- paste0("ver ", gsub("Version: ", "", DESC[grep("Version", DESC)]), 
                      " (", gsub("Date: ", "", DESC[grep("Date", DESC)]), ")",
                      " jan.lisec@bam.de")
rm(DESC)
#status_line <- paste0("ver ", "0.1.12", " (", "2021-10-18", ") jan.lisec@bam.de")
# set resource path for BAM icon and other static www data
shiny::addResourcePath(prefix = 'pics', directoryPath = 'inst/www')
# load testdata in an extra environment
tde <- new.env()
utils::data(testdata, envir = tde)
# set up an indicator object to tell the App that this is the shinyapp.io version
app_destination <- "shiny.io"