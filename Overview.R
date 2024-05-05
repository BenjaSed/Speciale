# Main dataframes creation
source("Main tests.R") 

# LCMS vs immunometry
source("LCMS vs immunometry synacthen.R") # Skal køres med removeLCMS og removeimmuno = FALSE!!!



# ANOVA tests
source("ANOVA.R") # Skal køres med removeLCMS, og én gang med removeimmuno+substudy, og én gang med removeLCMS+substudy
# Paired T-test
source("Paired T-test.R")

# IGF-1 plotting
source("IGF1.R")

# Cortisol during Macimorelin
source("Cortisol during Macimorelin.R")

# Demography
source("Demography.R") # Skal køres med removeLCMS, og én gang med removeimmuno+substudy, og én gang med removeLCMS+substudy
# Flowchart
source("Flowchart.R")
  
# Tests
source("Synacthen test.R") # Skal køres én gang med removeimmuno, og én gang med removeLCMS!!!





# Not needed
source("Cortisol test.R") # Skal køres med cutoffdate = NULL?
source("Macimorelin test.R") # Probably not needed


