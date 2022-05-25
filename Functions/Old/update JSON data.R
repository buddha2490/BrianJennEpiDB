jsonDBFun <- function() {
  
  

# Data --------------------------------------------------------------------
myDB <- dbConnect(SQLite(), "EpiPackageR.DB")
tempDB <- dbConnect(SQLite(), "Data/jsonDB.DB") 
concepts <- tbl(myDB, "concepts") %>%
  select(id, Name = name, Medical_Concept_Type = `concept type`, original_node_id) %>%
  data.frame() 

# Functions ---------------------------------------------------------------
  
  # Reads the json files and cleans them up
  cleanJsonPriors <- function(json, concepts, sex, age, region) {
    dat <- jsonlite::fromJSON(json, flatten = T)
    version <- dat$metadata$version
    
    # Disease -----------------------------------------------------------------
    incidence <- dat$incidence_priors 
    code  <- names(incidence)
    p <- do.call("rbind", incidence)
    incidence <- data.frame(ID = code, P = p) %>%
      left_join(select(concepts, ID = original_node_id, Name), 
                "ID") %>%
      mutate(Sex = sex, Age_Range=age, Region = region) %>%
      rename(To_Concept_Name = Name, To_Concept_ID = ID, Incidence = P) %>%
      select(Sex, Age_Range, Region, To_Concept_ID, To_Concept_Name, Incidence)
    rm(code, p)
    
    
    # Risk factors --------------------------------------------------------------
    prevalence <- dat$prevalence_priors
    code  <- names(prevalence)
    p <- do.call("rbind", prevalence)
    prevalence <- data.frame(ID = code, P = p) %>%
      left_join(select(concepts, ID = id, Name),
                "ID") %>%
      mutate(Sex = sex, Age_Range=age, Region = region) %>%
      rename(From_Concept_Name = Name, From_Concept_ID = ID, Prevalence = P) %>%
      select(Sex, Age_Range, Region, From_Concept_ID, From_Concept_Name, Prevalence)
    rm(code, p)
    
    # Symptoms ----------------------------------------------------------------
    symptoms <- dat$symptom_disease_marginals 
    symptoms <- lapply(names(symptoms), function(i) {
      df <- do.call("rbind", symptoms[[i]]) %>% data.frame()
      df$To_Concept_ID <- row.names(df)
      df$From_Concept_ID <- i
      names(df) <- c("P", "To_Concept_ID", "From_Concept_ID")
      row.names(df) <- NULL
      return(df)
    }) 
    
    # Identify the duration of symptoms list
    col <- data.frame(i=as.numeric(), ncol = as.numeric())
    for (i in 1:length(symptoms)) {
      col[i, "i"] <- i
      col[i, "ncol"] <- ncol(symptoms[[i]])
    }
    i <- filter(col, ncol != 3)$i
    duration <- symptoms[[i]]
    names(duration) <- c("P1", "P2", "P3", "P4", "P5", "To_Concept_ID", "From_Concept_ID")
    duration <- left_join(duration,
                          filter(concepts, Medical_Concept_Type == "SYMPTOM") %>%
                            select(From_Concept_ID = id, From_Concept_Name = Name) %>%
                            filter(!duplicated(From_Concept_ID)),
                          "From_Concept_ID") %>%
      left_join(
        filter(concepts, Medical_Concept_Type == "DISEASE") %>%
          select(To_Concept_ID = original_node_id, To_Concept_Name = Name) %>%
          filter(!duplicated(To_Concept_ID)),
        "To_Concept_ID"
      )
    duration <- select(duration, From_Concept_ID, From_Concept_Name, To_Concept_ID,
                       To_Concept_Name, P1, P2, P3, P4, P5)
    
    
    symptoms <- symptoms[-i]
    symptoms <- do.call("rbind", symptoms) %>%
      left_join(
        filter(concepts, Medical_Concept_Type == "SYMPTOM") %>%
          select(From_Concept_ID = id, From_Concept_Name = Name) %>%
          filter(!duplicated(From_Concept_ID)),
        "From_Concept_ID") %>%
      left_join(
        filter(concepts, Medical_Concept_Type == "DISEASE") %>%
          select(To_Concept_ID = original_node_id, To_Concept_Name = Name) %>%
          filter(!duplicated(To_Concept_ID)),
        "To_Concept_ID"
      ) %>%
      mutate(Sex = sex, Age_Range=age, Region = region) %>%
      select(Sex, Age_Range, Region, From_Concept_ID, From_Concept_Name, To_Concept_ID,
             To_Concept_Name, P)
    
    
    
    # Relative risks ----------------------------------------------------------
    rr <- dat$relative_risks 
    # Each element of the RR is a disease
    # Within each sublist,  each is a risk factor
    
    rr <- lapply(names(rr), function(i) {
      df <- do.call("rbind", rr[[i]]) %>% data.frame()
      df$From_Concept_ID <- row.names(df) # risk factor - from_concept_id
      df$To_Concept_ID  <- i  # Disease
      names(df) <- c("RR", "From_Concept_ID", "To_Concept_ID")
      row.names(df) <- NULL
      return(df)
    }) %>%
      do.call("rbind", .) %>%
      left_join(
        select(concepts, To_Concept_ID = original_node_id, To_Concept_Name = Name),
        "To_Concept_ID") %>%
      left_join(
        select(concepts, From_Concept_ID = id, From_Concept_Name = Name),
        "From_Concept_ID")  %>%
      mutate(Sex = sex, Age_Range=age, Region = region) %>%
      select(Sex, Age_Range, Region, From_Concept_ID, From_Concept_Name, To_Concept_ID, To_Concept_Name, RR)
    
    
    list(version = version, incidence = incidence, prevalence = prevalence, symptoms = symptoms, duration = duration,
         marginals = rr)
    
  }
  
  # Quick function to merge the cleaned json files into something usable
  mergeJson <- function(dat) {
    
    incidence <- dat$incidence
    prevalence <- dat$prevalence
    symptoms <- dat$symptoms %>% mutate(Symptom = 1) %>% rename(P_RFD = P)
    marginals <- dat$marginals
    version <- dat$version
    
    # Merge symptoms and marginals together since they are structured the same
    allMarginals <- bind_rows(symptoms, marginals)
    
    
    merged <- full_join(
      select(incidence, To_Concept_ID, Incidence),
      allMarginals,
      "To_Concept_ID"
    )  %>%
      full_join(
        select(prevalence, From_Concept_ID, Prevalence),
        "From_Concept_ID"
      ) %>%
      filter(!is.na(To_Concept_ID))
    
    
    merged$Region <- incidence$Region[1]
    merged$Sex <- incidence$Sex[1]
    merged$Age_Range <- incidence$Age_Range[1]
    merged$Version <- version
    select(merged, Version,
           Region, Sex, Age_Range, Disease = To_Concept_Name, Risk_Factor = From_Concept_Name,
           Incidence, Prevalence, Symptom, RiskRatio = RR, P_RFD) 
    
  }
  
  # Update from the repo
  updateJson <- function() {
    
    # delete whatever old data I had in there
    system("rm -r diagnostic-engine-models")
    
    # This will do a fresh clone of the repo and copy the most recent UK and US files to the directory I need
    system("git clone git@github.com:babylonhealth/diagnostic-engine-models.git")
    src <- file.path("diagnostic-engine-models/components/inference_engine")
    drop <-c("us-deep-south-to-great-lakes-dolphin-8.3", "us-deep-south-to-great-lakes-dolphin-8.5",
             "us-deep-south-to-great-lakes-dolphin-8.6", "us-deep-south-to-great-lakes-dolphin-8.8",
             "us-deep-south-to-great-lakes-dolphin-8.9", "uk-dolphin-8.3", "uk-dolphin-8.9")
    models <- data.frame(files = list.files(src)) %>%
      filter(!files %in% drop)
    version <- list()
      version$uk = data.frame(files = models[grep("uk-dolphin", models$files),])
      version$us = data.frame(files = models[grep("us-deep-south-to-great-lakes-dolphin-", models$files),])
    version <- lapply(version, function(x) {
      x$version = sub("uk-dolphin-", "", x$files)
      x$version = sub("us-deep-south-to-great-lakes-dolphin-", "", x$version) %>% as.numeric()
      x <- arrange(x, version)
      return(x$files[nrow(x)])
    }) 
    lapply(c("US", "UK"), function(region) {
      
      if (region == "US") modelVersion = version$us
      if (region == "UK") modelVersion = version$uk
      
      newDir <- file.path(src, modelVersion, "perov")
      oldDir <- file.path("Data/json", region)
      newFiles <- list.files(newDir)
      oldFiles <- list.files(oldDir)
      
      # Create an archive of the old files
      dir.create(file.path("data/json/archive", Sys.Date())) # warnings: already exists
      archive <- file.path("data/json/archive", Sys.Date(), region)
      dir.create(archive)

      # Copy to archive
      lapply(oldFiles, function(x) {
        file.copy(
          file.path(oldDir, x),
          file.path(archive, x))

        file.remove(
          file.path(oldDir, x)
        )
      })

      # Copy new data
      lapply(newFiles, function(x) {
        file.copy(file.path(newDir, x),
                  file.path(oldDir))
      })
      
    })
  }
  

# Copy new files from PGM repo --------------------------------------------
message("Please wait while we clone the most recent version of git@github.com:babylonhealth/diagnostic-engine-models.git") 
suppressWarnings({
   updateJson()
 })
 


# Process the files and add to a database ---------------------------------
files <- lapply(c("UK", "US"), function(region) {
  src <- file.path("data/json",region)
  files <- data.frame(files = list.files(src))
  files$Region <- region
  files$Region <- factor(files$Region,
                         c("UK", "US"),
                         c("UK", "US-DEEP-SOUTH-TO-GREAT-LAKES"))
  # Parse the filenames
  files$Sex <- "Male"
  files$Sex[grep("model.female", files$files)] <- "Female"
  files$Age <- "15/24"
  files$Age[grep("25.39", files$files)] <- "25/39"
  files$Age[grep("40.59", files$files)] <- "40/59"
  files$Age[grep("60.74", files$files)] <- "60/74"
  files$Age[grep("75.100", files$files)] <- "75/+"
  
  files$filename <- paste0(tolower(files$Sex),files$Age) %>%
    sub("/", ".", .)
  files$filename <- sub(".+", "p", files$filename, fixed = T)
  return(files)
})
names(files) <- c("UK", "US")


# Process the UK files
ukDat <- file.path("Data/json/UK")
UKJson <- list()
df <- files$UK
for (i in 1:nrow(df)) {
  name <- df$filename[i]
  UKJson[[name]] <- cleanJsonPriors(json = file.path(ukDat, df$files[i]),
                    concepts = concepts,
                    sex = df$Sex[i],
                    age = df$Age[i],
                    region = df$Region[i])
}
rm(df)

# Process the US files
usDat <- file.path("Data/json/US")
USJson <- list()
df <- files$US
for (i in 1:nrow(df)) {
  name <- df$filename[i]
  USJson[[name]] <- cleanJsonPriors(json = file.path(usDat, df$files[i]),
                                    concepts = concepts,
                                    sex = df$Sex[i],
                                    age = df$Age[i],
                                    region = df$Region[i])
}
rm(df)

  # Merge the data
  UKJson <- lapply(UKJson, mergeJson) %>%
    do.call("rbind", .)
  USJson <- lapply(USJson, mergeJson) %>%
    do.call("rbind", .)
  
  dbWriteTable(tempDB, "UKJson", UKJson, overwrite = T)
  dbWriteTable(tempDB, "USAJson", USJson, overwrite = T)
  
  dbDisconnect(tempDB)
}

