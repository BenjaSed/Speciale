source("Main tests.R")

if(!removeimmunometry & !removeLCMS) {stop("Either removeimmunometry or removeLCMS must be TRUE!")}

demography <- read_excel(file_location, 
                         sheet = if(removeimmunometry) {"Patientdata LCMS"} else {if(removeLCMS) {"Patientdata Immuno"} else stop()}, 
                         col_types = c("text", "numeric", "text", 
                                       "text", "numeric", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "date", "date", "text", "numeric", 
                                       "numeric", "numeric", "numeric", "numeric", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "text","text","text","text"), n_max = 20)

if(substudy){
  demography <- demography[demography$ID %in% c(1, 5, 6, 9, 11, 13, 14, 16), ]
}



demography_table <- data.frame(Variables = c(
  "Patients",
  "     No. of events, % (n)",
  "     No. of unique patients, % (n)",
  "     Male/female %",
  "     Age mean (95% CI)",
  
  "Macimorelin test indication",
  "     Pituitary surgery, % (n)",
  "     TBI, % (n)",
  "     Brain proton therapy, % (n)",
  "     Congenital insufficiency, % (n)",
  "     N/A, % (n)",
  
  "Macimorelin tests per patient",
  "     2 test, % (n)",
  "     1 test, % (n)",
  
  "Days between Macimorelin-test and nearest other test",
  "     Synacthen test, mean (95% CI)",
  "     Spot-cortisol, mean (95% CI)",
  "     IGF-I test, mean (95% CI)",
  
  "Medications paused for Macimorelin test",
  "     GH substitutions, % (n)",
  "     Cortisol substitutions, % (n)",
  "     Sex hormone substitutions, % (n)",
  "     Thyroid hormone substitutions, % (n)",
  "     Antidiuretic hormone substitutions, % (n)",
  
  "Assessed functional HPA-axis",
  "     Synacthen-assessed, % (n)",
  "          Yes/no %",
  "     Spot-Cortisol assessed, % (n)",
  "          Yes/no %"
))

demography_table$Data <- c(
  "", #Patients
  npercent("Macimorelin udført","x",TRUE), #No. of events, % (n)
  npercent("Unique","x",TRUE), #No. of unique patients, % (n)
  abpercent("Køn","Mand","Kvinde"), #Male/female %
  meansd(demography, "Alder"), #Age mean (95% CI)
  
  "", #Cause
  npercent("Pituitary surgery","x",TRUE), #Pituitary surgery, % (n)
  npercent("TBI","x",TRUE), #TBI, % (n)
  npercent("Brain proton therapy", "x",TRUE), #Brain proton therapy, % (n)
  npercent("Congenital", "x",TRUE), #Congenital insufficiency, % (n)
  npercent("NA", "x",TRUE), #N/A, % (n)
  
  "", #Macimorelin tests per patient
  npercent("Antal macimorelin", "2", FALSE), #> 1 test, % (n)
  npercent("Antal macimorelin", "1", FALSE), #1 test, % (n)
  
  "", #Time between Macimorelin-test and nearest test
  meansd(demography, "Tid mllm maci/synac"), #Days between, mean (95% CI)
  meansd(demography, "Tid mllm maci/SpotCorti"), #Days between, mean (95% CI)
  meansd(demography, "Tid mllm maci/IGF"), #Days between, mean (95% CI)
  
  "", #Medications used at Macimorelin-test time*
  npercent("GH substitutions", "x", TRUE), #GH substitutions, % (n)
  npercent("Cortisol substitutions", "x", TRUE), #Cortisol substitutions, % (n)
  npercent("Sex hormone substitutions", "x", TRUE), #Sex hormone substitutions, % (n)
  npercent("Thyroid hormone substitutions", "x", TRUE), #T4, % (n)
  npercent("ADH substitutions", "x", TRUE), #ADH, % (n)
  
  "", #Assessed functional HPA-axis
  npercent("Synacthen","x",TRUE), # Synacthen
  abpercent("Synacthen rask", "Ja", "Nej", FALSE), #Yes/no %
  npercent("Spot Cortisol","x",TRUE), # Spot cortisol
  abpercent("Spot Cortisol rask", "Ja", "Nej", FALSE) #Yes/no %

)
if(saveplots){write_xlsx(demography_table, paste0(save_file_location,"/demography_table",if(removeLCMS) {"_OnlyImmuno"} else {"_OnlyLCMS"},".xlsx"))}
