# Run this script to update all the data on the EPi database
# You will need the following files and filenames.  If you don't include a particular file
# that is fine, the database already has a version and will continue to use the earlier versions.
# For example, if you ONLY want to input the priors data, just put the csv file in the Data folder and run the script
# It will update only the priors data and nothing else.

# The function works by searching for particular file name strings.  These files need to be in the "Data" directory
# I assume that the files I already have are representative of the files we will use, just with different time stamps
# SThey can have different time stamps, but need to otherwise look like this:




# conceptFile <-  "Concepts & Questions.xlsx"
# rfMarginalsFile <- "RF-D marginals_22FEB02.csv"
# ddMarginalsFile <- "D-D marginals_22FEB02.csv"
# priorsFile <- "priors_03FEB22.csv"
# menWeightFile <- "WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE"
# femWeightFile <- "WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE"
# frequencyLabelFile = "FrequencyLabels_14MAY21.csv"




# Run this script to update the epi databases
source("Functions/buildEpiDB.R")
buildEpiDB()


myDB <- dbConnect(SQLite(), "EpiDB.DB")
dbListTables(myDB)