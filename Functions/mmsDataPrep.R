mmsDataPrep <- function() {


# Connect to database
# Currently this is SQLite, but maybe we will have a postgres database soon
myDB <- dbConnect(SQLite(), "EpiDB.DB")



# Step 0 - rename the frequency label file
# I need the FL file to be named differently, without the word "Priors" in it
# It is messing with my grep and the priors file
lapply(list.files("Data", pattern = "All_concept_pipeline_priors"), function(x) {
  y = sub("All_concept_pipeline_priors", "FrequencyLabels", x, ignore.case = T)
  file.rename(file.path("Data", x),
               file.path("Data", y))
})


# Step 1 - get a list of data files and their last-update date
# I only want the most recent file, this section will identify the files I need
dataFiles = lapply(list.files("Data"), function(x) {
  file.info(file.path("Data", x)) %>% 
    data.frame() %>%
    select(mtime) %>%
    data.frame(File = x, mtime = .)
    }) %>%
  do.call("rbind", .)

recentFiles = function(grepName) {
   files = dataFiles[grep(grepName, dataFiles$File),] %>%
     arrange(desc(mtime))
   return(files$File[1])
}

# These are the files I'll be reading
conceptFile <- recentFiles("Concepts & Questions")
rfMarginalsFile <- recentFiles("RF-D")
ddMarginalsFile <- recentFiles("D-D")
priorsFile <- recentFiles("priors_")
menWeightFile <- recentFiles("ANNUAL_MALE")
femWeightFile <- recentFiles("ANNUAL_FEMALE")
frequencyLabelFile = recentFiles("FrequencyLabels")



#  Load my data

# 1. Concepts - 2 tabs
if (is.na(conceptFile)) {
  concepts <- tbl(myDB, "concepts") %>% data.frame()
  associations <- tbl(myDB, "associations") %>% data.frame()
  warning("Concepts & Questions file not found, using older data from the database")
} else {
  concepts = readxl::read_excel(file.path("Data", conceptFile), sheet = 1)
  associations = readxl::read_excel(file.path("Data", conceptFile), sheet = 7)
  dbWriteTable(myDB, "concepts", concepts, overwrite = T)
  dbWriteTable(myDB, "associations", associations, overwrite = T)
  message(paste0(conceptFile, " found and used to update the 'concepts' and 'associations' tables in the database"))
}


# 2.  marginals
if (is.na(ddMarginalsFile)) {
  ddMarginals <- tbl(myDB, "ddMarginals") %>% data.frame()
  warning("D-D Marginals CSV file not found, using older data from the database")
} else {
  ddMarginals <- read.csv(file.path("Data", ddMarginalsFile))
  dbWriteTable(myDB, "ddMarginals", ddMarginals, overwrite = T)
  message(paste0(ddMarginalsFile, " found and used to update the `ddMarginals` table in the database"))
}
if (is.na(rfMarginalsFile)) {
  rfMarginals <- tbl(myDB, "rfMarginals") %>% data.frame()
  warning("RF-D Marginals CSV file not found, using older data from the database")
} else {
  rfMarginals <- read.csv(file.path("Data", rfMarginalsFile))
  dbWriteTable(myDB, "rfMarginals", rfMarginals, overwrite = T)
  message(paste0(rfMarginalsFile, " found and used to update the `rfMarginals` table in the database"))
}
# Bind them to create a single marginals file
allMarginals <- rbind(
     tbl(myDB, "ddMarginals") %>% data.frame(),
     tbl(myDB, "rfMarginals") %>% data.frame()
   )
dbWriteTable(myDB, "marginals", allMarginals, overwrite = T)
rm(allMarginals)


# 3.  Priors - MMS incidence/prevalence
if (is.na(priorsFile)) {
  priors <- tbl(myDB, "priors") %>% data.frame()
  warning("Priors CSV file not found, using older data from the database")
} else {
  priors <- read.csv(file.path("Data", priorsFile))
    timestamp <- max(priors$Last_Updated_Time, na.rm=T) %>% as.Date(origin = "1970-01-01")
    priors$extractdate <- format(timestamp, "%d%b")
  dbWriteTable(myDB, "priors", priors, overwrite = T)
  message(paste0(priorsFile, " found and used to update the `priors` table in the database"))
}


# 4.  Frequency labels
if (is.na(frequencyLabelFile)) {
  freqLabels <- tbl(myDB, "freqLabels") %>% data.frame()
  warning("Frequency label CSV file not found, using older data from the database")
} else {
  freqLabels <- read.csv(file.path("Data", frequencyLabelFile))
    lab <- sub(".*_","", frequencyLabelFile) %>%
      sub(".csv", "", .)
    freqLabels$extractdate <- lab
  dbWriteTable(myDB, "freqLabels", freqLabels, overwrite = T)
  message(paste0(frequencyLabelFile, " found and used to update the `freqLabels` table in the database"))
}
   

# 5.  Population weights                          
getWeights <- function(file, sex) {
  dat <- readxl::read_excel(file.path("Data", file), sheet = 1, skip = 16)
  cols <- c("Index", "Variant", "Region", "Notes", "CountryCode", "Type", "ParentCode", "Date", paste0("Age_",0:100))
  keep <- c("Saudi Arabia", "Malaysia", "Singapore", "United Kingdom", "United States of America", "Rwanda")
  names(dat) <- cols
  
  dat = filter(dat, Region %in% keep) %>%
    filter(Date == "2020")
  
  long <- pivot_longer(data = dat, cols =Age_0:Age_100 , names_to = "Age", values_to = "Weight") %>%
    select(Region, Age, Weight) %>%
    mutate(Sex = sex)
  
  long$Age = sub("Age_", "", long$Age) %>% as.numeric()
  
  long$Region <- factor(long$Region, c("Saudi Arabia", "Malaysia", "Singapore", "United Kingdom", "United States of America", "Rwanda"),
                        labels = c("KSA", "MALAYSIA", "SINGAPORE", "UK", "US-DEEP-SOUTH-TO-GREAT-LAKES", "RWANDA"))
  
  return(long)
}

if (is.na(menWeightFile) | is.na(femWeightFile)) {
  popWeights <- tbl(myDB, "PopulationWeights") %>% data.frame()
  warning("Population weights excel files are missing for men or women. Using older data from the database")
} else {
  if (!is.na(menWeightFile)) {
    menWeights = getWeights(menWeightFile, "Males")
  } else {
    menWeights = tbl(myDB, "PopulationWeights") %>%
      filter(Sex == "Males") %>%
      data.frame()
  }
  if (!is.na(femWeightFile)) {
    femWeights = getWeights(femWeightFile, "Females")
  } else {
    femWeights = tbl(myDB, "PopulationWeights") %>%
      filter(Sex == "Females") %>%
      data.frame()
  }
  popWeights = rbind(menWeights, femWeights)
  dbWriteTable(myDB, "PopulationWeights", popWeights, overwrite = T)
}



# Cleanup my file path ----------------------------------------------------
# All the file used will be moved to a time stamped backup folder
# The user will only then need to update data as new files come in
# old data will be use

# Clean up the data folder but create a backup
suppressWarnings({
backup = file.path("Data", paste0("Backup - ", Sys.Date()))
dir.create(backup)
file.copy(file.path("Data", conceptFile), backup); file.remove(file.path("Data", conceptFile))
file.copy(file.path("Data", rfMarginalsFile), backup); file.remove(file.path("Data", rfMarginalsFile))
file.copy(file.path("Data", ddMarginalsFile), backup); file.remove(file.path("Data", ddMarginalsFile))
file.copy(file.path("Data", priorsFile), backup); file.remove(file.path("Data", priorsFile))
file.copy(file.path("Data", menWeightFile), backup); file.remove(file.path("Data", menWeightFile))
file.copy(file.path("Data", femWeightFile), backup); file.remove(file.path("Data", femWeightFile))
file.copy(file.path("Data", frequencyLabelFile), backup); file.remove(file.path("Data", frequencyLabelFile))

rm(conceptFile, rfMarginalsFile, ddMarginalsFile, priorsFile, menWeightFile, femWeightFile, frequencyLabelFile, 
   menWeights, femWeights, backup, lab, timestamp, getWeights, recentFiles)
})

# Clean up the priors---------------------------------------

# Matches Laura
p <- pivot_longer(
  data = priors,
  cols = Incidence_Value:Prevalence_Value,
  values_to = "data_points",
  names_to = "value_type"
) %>%
  filter(!is.na(data_points)) %>%
  count(Region, Name, Incidence.Prevalence, PGM_Code) %>%
  pivot_wider(names_from = Incidence.Prevalence, 
              values_from = n,
              values_fill = NA) %>%
  mutate(both_inci_prev = ifelse(!is.na(INCIDENCE + PREVALENCE), "both", "unique")) %>%
  select(Region, Name, both_inci_prev) %>%
  data.frame()
priors <- left_join(priors, p, c("Region", "Name"))
rm(p)

long <- pivot_longer(data = priors, Incidence_Value:Prevalence_Value, values_to = "data_points", names_to = "value_type") %>%
  filter(!is.na(data_points)) %>%
  mutate(dataper100000 = data_points*100000)

# General cleanup - mostly filling in missing values, renaming, etc
long[,c("Incidence.Prevalence", "Medical_Concept_Type")] <- lapply(long[,c("Incidence.Prevalence", "Medical_Concept_Type")], str_to_title)
long$Region <- toupper(long$Region)
long$Medical_Concept_Type[long$Medical_Concept_Type == "Risk_factor"] <- "Risk Factor"
long$measure_type <- with(long, paste(Medical_Concept_Type, Incidence.Prevalence))

long$Name[long$Name=="Alcohol intake above recommended limits (>14 units/week)"]<-"Alcohol above rec. limits"
long$Name<-gsub("H/O:", "HO ", long$Name)  
long$Sex <- with(long, ifelse(Sex == "M", "Males", ifelse(Sex == "F", "Females", "COMBINED")))
long$Age_Range[long$Age_Range == ""] <- "15/+"

long$Data_Status <- with(long, ifelse(Data_Status == "DATA_FOUND", "Epi data", NA))
long <- filter(long, !is.na(long$Data_Status))
long$varselect <- with(long, paste(measure_type, Name, sep = " - "))
long$Region[long$Region == "DEFAULT"] <- "UK"

keep <- c("PGM_Code", "Region", "Name", "Medical_Concept_Type","Incidence.Prevalence","Data_Status","Sex", "Age_Range","dataper100000","varselect", "extractdate")
long <- select(long, all_of(keep))
rm(keep)


# Frequency label data  -------------------------------------------
freqs <- tbl(myDB, "freqLabels") %>% 
  rename(PGM_Code = concept_id,
         Incidence.Prevalence = INCIDENCE.PREVALENCE,
         Sex = sex,
         data = Prior.value,
         Region = region,
         Name = concept_name,
         Medical_Concept_Type = Label) %>%
  filter(Sex != "unisex") %>%
  mutate(Sex = ifelse(Sex == "female_only", "Females", "Males"),
         max_age = ifelse(max_age == 100, "+", as.character(max_age)),  
         min_age = as.character(min_age),
         Age_Range = paste(min_age, max_age, sep="/"),
         dataper100000 = data*100000,
         Data_Status = "Frequency label") %>%
  select(-data) %>% 
  data.frame() 

freqs$Incidence.Prevalence<- str_to_title(freqs$Incidence.Prevalence, locale = "en")
freqs$Medical_Concept_Type[freqs$Medical_Concept_Type=="Risk"]<-"Risk Factor"
freqs$Name[freqs$Name=="Alcohol intake above recommended limits (>14 units/week)"]<-"Alcohol above rec. limits"
freqs$Name<-gsub("H/O:", "HO ", freqs$Name)  
freqs$Region[freqs$Region=="US-DSGL"]<-"US-DEEP-SOUTH-TO-GREAT-LAKES"
freqs$Region <- toupper(freqs$Region)
freqs$Name<-str_replace(freqs$Name, "UD ", "") 
freqs$Medical_Concept_Type[str_detect(freqs$Name, "DRF ")]<-"Disease"
freqs$Name<-str_replace(freqs$Name, "DRF ", "") 
freqs$varselect <- with(freqs, paste0(Medical_Concept_Type," ",Incidence.Prevalence," - ", Name))

# figure out who is FL only
longKey <- with(long, paste0(Region, "_", varselect)) %>% unique()
freqs$key <- with(freqs, paste0(Region, "_", varselect))
freqs <- filter(freqs, !key %in% longKey) %>%
  select(-key, -min_age, -max_age)
long$Data_Status <- "Epi data (MMS)"

# final dataset into the database
dbWriteTable(myDB, "ALL_REGIONS_FL_MMS_final", rbind(long, freqs), overwrite = T) # cleaned priors file

}

  