# Running for the first time?
firsttimerun <- FALSE # IF THIS IS RUNNING FOR THE FIRST TIME, SET THIS TO TRUE AND RUN "main tests.R"
# CHANGE THE FILE LOCATION IF RUNNING THIS SCRIPT ON ANOTHER COMPUTER!!!
file_location <- "G:/My Drive/Studie/Uni/Medicin kandidat/10. semester/Speciale/Datasæt speciale.xlsx"
# Output folder for generated graphs:
output_folder <- "G:/My Drive/Studie/Uni/Medicin kandidat/10. semester/Speciale/Output"

# Remove any data based on variables?
removeimmunometry <- FALSE
removeLCMS <- TRUE

substudy <- TRUE





# Cutoffs
cutoff_macimorelin <- 2.8
cutoff_synacthen <- 420
SpotCortiCutoff <- 413

date_cutoff <- as.Date("2020-01-01") # should probably be around 2020

showplots <- FALSE
saveplots <- TRUE








# For LCMS vs immunometry:

#Use only data where there has been performed both LC/MS and immunometry at the same time?
onlyrunpaired <- TRUE # Needs both removeimmunometry and removeLCMS to be FALSE!




# Do not touch these:

# For Cortisol tests:
# Both should be TRUE
RemoveCortisolDuringMacimorelin <- TRUE
removeIDsWithSynacthen <- TRUE
  
# Remove excluded - ID 19 w/o Macimorelin or Synacthen
Remove19 <- TRUE
# Remove excluded - ID 20 w/o Macimorelin, but with Synacthen
Remove20 <- TRUE

createplots <- TRUE
#Remove synacthen-data over 60 min?
longsynacthen <- FALSE

# Only consider tests prior to the Macimorelin/Cortisol/IGF test when merging?
OnlyPreviousDates <- FALSE

# Input spot-cortisol data to synacthen_over_cutoff?
SpotCortiToSynacthen_over_cutoff <- TRUE