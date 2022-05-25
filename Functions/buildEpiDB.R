# Build the database - just run this program, nothing else needed
buildEpiDB <- function() {
  
# Load my dependancies
depends <- c("dplyr", "dbplyr", "DBI", "RSQLite", "RPostgres", "readxl", "tidyr", "jsonlite")
myPackages = data.frame(installed.packages())
need <- depends[!depends %in% myPackages$Package]
lapply(need, install.packages)
require(dplyr, quietly = T)
require(dbplyr, quietly = T)
require(DBI, quietly = T)
require(RSQLite, quietly = T)
require(RPostgres, quietly = T)
require(readxl, quietly = T)
require(tidyr, quietly = T)
require(jsonlite, quietly = T)
require(stringr, quietly = T)

# Load my functions - eventually this will be part of an R package and the namespace
source("Functions/mmsDataPrep.R")
source("Functions/jsonDBFun.R")


# First build the MMS side of things
mmsDataPrep()

# Then the json files - post-pipeline
# Note: right now this builds a seperate SQLite database due to github restrictions on file size
# This will need to be moved to Postgres whenever it is provisioned
jsonDBFun()
 

}
