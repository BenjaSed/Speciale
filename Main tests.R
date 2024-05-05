#Script by Benjamin Seddighi

#--------- Main variables ---------
source("Settings.R")
# If running for the first time, please check settings.R!
#--------- Libraries ---------
packages <- c(
  "readxl", # Reads excel
  "writexl", # Writes excel
  "dplyr", # Data manipulation
  "ggplot2", # Data plotting
  "DiagrammeR", # For creating flowchart
  "DiagrammeRsvg", # Needed for exporting flowchart
  "magrittr", # Also needed for exporting flowchart
  "xml2", # Also needed for exporting flowchart
  "glue", # Glue for flowchart text
  "ggpubr",
  "viridis", # Better colors for charts
  "lubridate", # Parse and manipulate dates
  "tidyverse",
  "broom",
  "survival", # Data manipulation
  "AICcmodavg", # Akaike information criterion
  "blandr", # Bland-Altman plot
  "gtools", # Enables better sorting capabilities
  "multcompView" # Enables cld for post-hoc analysis
)

if(firsttimerun) {
  lapply(packages, install.packages, character.only = TRUE) 
stop("All packages are installed, please change firsttimerun back to FALSE!")
  }
invisible(lapply(packages, library, character.only = TRUE))
rm(packages)
#--------- Functions ---------

folder <- if(removeimmunometry) {"/NoImmuno"} else if(removeLCMS) {"/NoLCMS"} else {""}
subfolder <- if(substudy) {"/substudy"} else {""}
save_file_location <- paste0(output_folder,subfolder,folder)


custompalette <- c("#0077b6","#FF6700","#e6194B","#3cb44b","#312853",
                   "#911eb4","#42d4f4","#f032e6","#bfef45","#fabed4",
                   "#469990","#dcbeff","#9A6324","#2B16E0","#800000",
                   "#aaffc3","#808000","#ffd8b1","#000075","#a9a9a9",
                   "#7C8FC7","#74492F","#335c67","#C3D613","#C2AC6E",
                   "#e5989b","#f15bb5","#76DB89","#ee9b00","#EF3054",
                   "#9e2a2b","#C2AC6E","#731DD8","#61B8FF","#ffe119"
)

demography <- data.frame()
# Function for calculating A/B %, with option to count NA
abpercent <- function(column, parameter_a, parameter_b, countNA = FALSE){
  
  countrowsa <- sum(demography[[column]]==parameter_a, na.rm = TRUE)
  countrowsb <- sum(demography[[column]]==parameter_b, na.rm = TRUE)
  totalrows <- if(!countNA){
    nrow(subset(demography,!is.na(demography[[column]])))
  } else {
    nrow(demography)
  }
  
  result <- paste0(
    round((countrowsa/totalrows)*100,1),
    "/",
    round((countrowsb/totalrows)*100,1))
  return(result)
}

# Function for calculating %, with option to count NA
apercent <- function(column, parameter, countNA = FALSE){
  
  countrows <- sum(demography[[column]]==parameter, na.rm = TRUE)
  totalrows <- if(!countNA){
    nrow(subset(demography,!is.na(demography[[column]])))
  } else {
    nrow(demography)
  }
  
  result <- round((countrows/totalrows)*100,1)
  return(result)
}

# Function for calculating % (n), with option to count NA
npercent <- function(column, parameter, countNA = FALSE){
  
  countrows <- sum(demography[[column]]==parameter, na.rm = TRUE)
  totalrows <- if(!countNA){
    nrow(subset(demography,!is.na(demography[[column]])))
  } else {
    nrow(demography)
  }
  
  result <- paste0(
    round((countrows/totalrows)*100,1),
    " (",countrows,")")
  return(result)
}


# Function for calculating mean and 95%-CI from standard error
meansd <- function(df, column, roundnumber = 1, plusminus = FALSE) {
  column_data <- df[[column]]
  column_data <- na.omit(column_data)
  standarddeviation <- sd(column_data)
  standarderror <- standarddeviation/sqrt(nrow(df))
  
  result <- tryCatch({
    paste0(format(round(mean(column_data, na.rm = TRUE), roundnumber), nsmall = 1), 
           " (",
           round(mean(column_data, na.rm = TRUE)-1.96*standarderror,roundnumber),
           "-",
           round(mean(column_data, na.rm = TRUE)+1.96*standarderror,roundnumber),
           ")"
    )
  }, error = function(e) "--")
  return(result)
}

# Function for calculating mean and standard error of mean

calculateSD <- function(df){ 
  
  if(UseLog){
    df$Cortisol <- exp(df$Cortisol)
  }
  
  test <- group_by(df, Time) %>%
    summarise(
      count = n(),
      mean = mean(Cortisol, na.rm = TRUE),
      sd = sd(Cortisol, na.rm = TRUE),
      se = sd/sqrt(n()),
      complete = paste0(round(mean,1),"±",round(se,1))
    )
  print(test$complete)
  print(nrow(df))
}


#Remove non-numeric values
remove_non_numeric <- function(dataframe_input, ..., columns = NULL) {
  # Check if columns argument is provided
  if (is.null(columns)) {
    # Single column handling
    columns <- list(...) %>% unlist()
  } else {
    # Multiple column handling
    columns <- as.character(columns)  # Ensure columns is a character vector
  }
  
  # Convert and replace each value individually
  for (col in columns) {
    dataframe_input[[col]] <- as.numeric(dataframe_input[[col]], errors = "coerce")
  }
  
  return(dataframe_input)
}

# Combine columns with data in only one column
combine_columns <- function(dataframe_input, ..., newcolname = "Combined") {
  # Capture column names from input
  columns <- as.character(list(...) %>% unlist())
  
  # Handle single column inouts as a special case
  if (length(columns) == 1) {
    dataframe_input[[newcolname]] <- dataframe_input[[columns[1]]]
    return(dataframe_input)
  }
  
  # For multiple columns with data
  dataframe_input[[newcolname]] <- sapply(1:nrow(dataframe_input), function(i) {
    # Find non-NA indices in specified columns
    non_na_index <- which(!is.na(dataframe_input[i, columns]))
    
    # Check if only one column has data
    if (length(non_na_index) == 1) {
      return(as.character(dataframe_input[i, columns[non_na_index[1]]]))  # Convert to string
    } else {
      return(NA_character_)  # Multiple or no populated columns, return NA
    }
  })
  
  return(dataframe_input)
}

# Create time_from baseline, calculate time difference relative to first bloodtest
calculate_relative_time <- function(group) {
  # Sort group by Datetime
  group$Time_from_baseline <- difftime(group$Datetime, group$Datetime[1])
  group$Time_from_baseline <- group$Time_from_baseline / 60  # Convert to minutes
  
  # Convert time difference to string without units
  group$Time_from_baseline <- as.character(round(group$Time_from_baseline, digits = 0))
  # Return the modified group
  return(group)
}

# Create numbering column
numbering <- function(dataframe_input, columnname, ...) {
  grouping <- enquos(...)
  edited_dataframe <- dataframe_input %>%
    group_by(!!!grouping) %>%
    mutate(!!columnname := cur_group_id())
  return(edited_dataframe)
}

# Create numbering column, but reset for each new data in first column
numberingforeachID <- function(dataframe_input, columnname, ...) {
  grouping <- enquos(...)
  edited_dataframe <- dataframe_input %>%
    group_by(!!!grouping) %>%
    mutate(!!columnname := row_number())
  return(edited_dataframe)
}

# Create numbering column, reset by first column, and only count different 
numberingforeachfactor <- function(dataframe_input, columnname, groupbycolumn, ...) {
  df <- dataframe_input
  othercolumns <- rlang::enquos(...)
  df <- df %>%
    group_by({{ groupbycolumn }}) %>%
    mutate({{ columnname }} := as.numeric(factor(interaction(!!! othercolumns))))
  
  return(df)
}

# Another numbering function
numberingforeach <- function(dataframe_input, columnname, ...) {
  # Group by the first column in ... and the remaining columns within each group
  grouped_data <- dataframe_input %>%
    group_by(across(c(...))) %>%
    group_by(across(1), add = TRUE)
  
  # Count each change within a group defined by ...
  dataframe_input <- grouped_data %>%
    mutate(!!columnname := cumsum(row_number() != lag(row_number(), default = 0))) %>%
    ungroup()
  
  return(dataframe_input)
}

# Save GGplots

saveggplot <- function(PrintID) {
  
  # Extracting name of dataframe
  name <- deparse(substitute(PrintID))
  
  # Running function
  lapply(names(PrintID), 
         function(x)ggsave(
           filename =paste0(name,"_",x,".png"), 
           plot = PrintID[[x]], 
           path = save_file_location,
           width = , 
           height = ))
  return("All saved!")
}

showggplot <- function(PrintID, ...) {
  # Capture all arguments as a character vector
  ids <- unlist(list(...))
  
  # Check for "all"
  if (all(tolower(ids) == "all")) {
    return(PrintID)
  }
  
  # Check for "null"
  if (any(tolower(ids) == "null")) {
    print(PrintID$NullID)
  }
  
  # Extract and validate numerical IDs
  valid_ids <- as.integer(ids[str_detect(ids, "\\d+")])
  if (any(is.na(valid_ids) | valid_ids < 1 | valid_ids > 20)) {
    return("Invalid IDs. Only use numbers between 1-20, 'all', or 'null'.")
  }
  
  # Print specified PrintIDs using vectorized approach
  print(PrintID[paste0("ID", valid_ids)])
}



# Create true/false column indicating if any values shares Date and ID with input column
testdateid <- function(dataframe_input, columnname, newcolumnname) {
  
  # Adding column with input column true
  df <- dataframe_input %>%
    mutate(Tempcolumn = case_when(!is.na(dataframe_input[[columnname]]) ~ TRUE))
  # Grouping by ID and date, creating column data for all dates for the ID if there is data in input column
  df <- df %>%
    group_by(ID, Date) %>%
    mutate(!!newcolumnname := any(Tempcolumn))
  
  # Moving data back into the original dataframe, caused problems when I returned df
  dataframe_input[[newcolumnname]] <- df[[newcolumnname]]
  
  return(dataframe_input)
}


# Creating ggplot
group_plot <- function(ID, input_dataframe, standardtest = NULL, group_by = NULL, group_by_legend = colnames(input_dataframe[group_by]), hidelegend = FALSE, x_axis = NULL, x_axis_name = colnames(input_dataframe[x_axis]), y_axis = NULL, y_axis_name = colnames(input_dataframe[y_axis]), second_y_axis = NULL, second_y_axis_name = if(is.null(y_axis)){colnames(input_dataframe[y_axis])} else NULL, second_y_axis_scale = NULL, title = "Title", subtitle = NULL, appendID = TRUE, autoXlimits = TRUE, xmin = 0, xmax = 250, autoYlimits = TRUE, ymin = 0, ymax = 900, meancurve = FALSE, showlines = TRUE, linear = FALSE, cutoff = NULL, agreementline = FALSE, saveasfile = FALSE, customcolors = TRUE, monocolor = FALSE, alpha = 1) {
  
  #Standardized tests
  if(!is.null(standardtest)) {
    
    # Standard Cortisol spot measurements
    if(standardtest == "Cortisol") {
      group_by = "Date"
      group_by_legend = "Date of measurement"
      x_axis = "Time_from_baseline"
      x_axis_name = "Time since baseline blood-test (min)"
      y_axis = "Combined"
      y_axis_name = "Cortisol (nmol/L)"
      title = "Cortisol measurements on a single day"
      autoXlimits = FALSE
      autoYlimits = FALSE
      ymin = 0
      ymax = 900
      xmin = 0
      xmax = 200
      cutoff = cutoff_synacthen}
    
    # Standard Synacthen test
    if(standardtest == "Synacthen") {
      group_by = "IDandTest"
      group_by_legend = "Test-ID and date"
      x_axis = "Time"
      x_axis_name = "Time since baseline blood-test (min)"
      y_axis = "Cortisol_P_Cortisol"
      y_axis_name = "Cortisol response (XX/XX)"
      title = "Synacthen-test"
      autoXlimits = FALSE
      autoYlimits = FALSE
      ymin = 0
      ymax = 900
      xmin = 0
      xmax = 30
      cutoff = cutoff_synacthen
    }
    
    # Standard Macimorelin test
    if(standardtest == "Macimorelin") {
      group_by = "Date"
      group_by_legend = "Date of test"
      x_axis = "Time_from_baseline"
      x_axis_name = "Time since baseline blood-test (min)"
      y_axis = "Macimorelin_GH_Result"
      y_axis_name = "Growth Hormone response (ug/L)"
      title = "Macimorelin-test"
      autoXlimits = FALSE
      autoYlimits = FALSE
      ymin = 0
      ymax = 20
      xmin = 0
      xmax = 100
      cutoff = cutoff_macimorelin
    }
    
    # Cortisol during macimorelin
    if(standardtest == "CortisolDuringMacimorelin"){
      group_by = "IDandTest"
      group_by_legend = "ID"
      x_axis = "Time"
      x_axis_name = "Time (min)"
      y_axis = "Cortisol"
      y_axis_name = "Cortisol level (nmol/L)"
      appendID = TRUE
      title = "Serum Cortisol during Macimorelin-test"
      cutoff = cutoff_synacthen
      autoYlimits = FALSE
      ymin = 100
      ymax = 600
    }
  }
  
  # Appending ID to name
  if(appendID == TRUE) {
    title <- paste0(title," - ID ",ID)
  }
  
  # If custom x- and y-axis limits
  if(autoXlimits == FALSE) {xlimits = c(xmin, xmax)} else {xlimits = NULL}
  if(autoYlimits == FALSE) {ylimits = c(ymin, ymax)} else {ylimits = NULL}
  
  # Shortening the dataframe name
  df <- input_dataframe
  
  # Make sure the x- and y-axis is numeric
  if(!is.numeric(df[[x_axis]])){
    df[[x_axis]] <- as.numeric(df[[x_axis]], na.rm = TRUE)
  }
  
  if(!is.numeric(df[[y_axis]])){
    df[[y_axis]] <- as.numeric(df[[y_axis]], na.rm = TRUE)
  }
  if(!is.null(second_y_axis)){
    if(!is.numeric(df[[y_axis]])){
      df[[second_y_axis]] <- as.numeric(df[[second_y_axis]], na.rm = TRUE)
    }
  }
  
  # Filter data for ID
  if(!is.null(ID)) {
    df_filtered <- df[df$ID == ID, ]
  } else {
    df_filtered <- df
  }
  
  if(FALSE) {
    # Remove empty axis values
    df_filtered <- subset(df_filtered,!is.na(df_filtered[[x_axis]]))
    df_filtered <- subset(df_filtered,!is.na(df_filtered[[y_axis]]))
    if(!is.null(second_y_axis)){df_filtered <- subset(df_filtered,!is.na(df_filtered[[second_y_axis]]))}
  }
  
  # If grouping is not wanted
  if(group_by == "" || is.null(group_by)) {
    nogrouping <- TRUE
    group_by <- "group"
    df_filtered$group <- "1"
  } else {nogrouping <- FALSE}
  
  # Sorting the order for the legend
  # Workaround to ggplot not playing nicely with my variables
  df_plot <- data.frame(grouping = df_filtered[[group_by]], 
                        x_axis = df_filtered[[x_axis]],
                        y_axis = df_filtered[[y_axis]]
  )
  if(!is.null(second_y_axis)){df_plot$second_y_axis = as.numeric(df_filtered[[second_y_axis]])}
  
  # Modifying groups with gtools' mixedsort
  df_plot$grouping <- factor(df_plot$grouping, levels=unique(mixedsort(df_plot$grouping)))
  
  if(!monocolor == FALSE) {custompalette <- replicate(50,monocolor)}
  #print(df_plot)
  #View(df_plot)
  #return(df_plot)
  
  
  plot <- ggplot(environment = environment()) +
    ggtitle(title, subtitle = subtitle) +
    xlab(x_axis_name) +
    ylab(y_axis_name) +
    {if(customcolors)scale_color_manual(values = custompalette)} +
    {if(!nogrouping)labs(color = group_by_legend)} +
    theme_classic2() +
    {if(!is.null(cutoff))geom_hline(yintercept= cutoff, linetype="dashed", color = "red", size=0.5)} +
    {if(agreementline)geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5)} +
    geom_point(data = subset(df_plot, !is.na(y_axis)), size = 0.9, aes(x = x_axis, y = y_axis, color = factor(grouping)), alpha = alpha) +
    {if(showlines == TRUE)if(linear == FALSE)geom_line(data = subset(df_plot, !is.na(y_axis)), size=0.5, aes(x = x_axis, y = y_axis, color = factor(grouping)), alpha = alpha)} + 
    {if(showlines == TRUE)if(linear == TRUE)geom_smooth(data = subset(df_plot, !is.na(y_axis)), method = "lm", se = FALSE, size=0.5, aes(x = x_axis, y = y_axis, color = factor(grouping)), alpha = alpha)} + 
    {if(meancurve == TRUE)geom_smooth(data = subset(df_plot, !is.na(y_axis)), aes(x = x_axis, y = y_axis), color = "black")} +
    {if(!is.null(second_y_axis))geom_point(data = subset(df_plot, !is.na(second_y_axis)), aes(x = x_axis, y = second_y_axis/second_y_axis_scale), alpha = alpha)} +
    {if(!is.null(second_y_axis))if(showlines == TRUE)if(linear == FALSE)geom_line(data = subset(df_plot, !is.na(second_y_axis)), aes(x = x_axis, y = second_y_axis/second_y_axis_scale), alpha = alpha)} +
    {if(!is.null(second_y_axis))if(showlines == TRUE)if(linear == TRUE)geom_smooth(data = subset(df_plot, !is.na(second_y_axis)), method = "lm", se = FALSE, size=0.3, aes(x = x_axis, y = second_y_axis/second_y_axis_scale), alpha = alpha)} +
    {if(!is.null(second_y_axis))scale_y_continuous(name = y_axis_name, sec.axis = sec_axis(~.*second_y_axis_scale, name=second_y_axis_name))} +
    {if(nogrouping)theme(legend.position = "none")} +
    {if(hidelegend)theme(legend.position = "none")} +
    coord_cartesian(xlim = xlimits, 
                    ylim = ylimits,
                    default = TRUE)
  
  if(saveasfile){ggsave(filename = paste0(title,".png"), plot = plot)}
  return(plot)
  
  # I can use print() here to get more outputs from the function before ending with return()
  
}


# Anova modelling and testing with AIC
anova_models <- function(input_dataframe, ..., oneway_only = FALSE) {
  
  # Testing for normality
  shapirotest <- as.numeric(shapiro.test(input_dataframe$Cortisol)[2])
  shapiro <- if(shapirotest >= 0.05) {paste0("Fail to reject normality - p-value: ",round(shapirotest,6))} else 
  {paste0("Not normally distributed! - p-value: ",round(shapirotest,6))}
  
  print(if(shapirotest >= 0.05) {"Normality test: Data can be used!"} else {"Normality test: Rejected!"})
  print(shapiro)
  
  # Creating empty lists
  models <- list()
  model_names <- c()
  summary <- c()
  
  # One-way
  models <- append(models, list(aov(Cortisol ~ Time, data = input_dataframe)))
  model_names <- append(model_names, paste0("(",deparse(substitute(input_dataframe)),") One-way ANOVA"))
  summary <- append(summary, list(paste0("aov(Cortisol ~ Time, data = ",deparse(substitute(input_dataframe)),")")))
  
  if(oneway_only == FALSE) {
    # Extract variable names from arguments
    variables <- list(...)  
    # Loop through each variable
    for (var in variables) {
      
      # AIC
      # Additive
      models <- append(models, list(aov(Cortisol ~ Time + input_dataframe[[var]], data = input_dataframe)))
      model_names <- append(model_names, paste0("(",deparse(substitute(input_dataframe)),") Two-way ANOVA, with additive effect from ",var))
      summary <- append(summary, list(paste0("aov(Cortisol ~ Time + ",deparse(substitute(input_dataframe$var)),", data = ",deparse(substitute(input_dataframe)),")")))
      # Interactive
      models <- append(models, list(aov(Cortisol ~ Time * input_dataframe[[var]], data = input_dataframe)))
      model_names <- append(model_names, paste0("(",deparse(substitute(input_dataframe)),") Two-way ANOVA, with interactive effect from ",var)) 
      summary <- append(summary, list(paste0("aov(Cortisol ~ Time * ",deparse(substitute(input_dataframe$var)),", data = ",deparse(substitute(input_dataframe)),")")))
    }
  }
  aic_best <- aictab(models, modnames = model_names)$Modnames[1]
  aic_bestnumb <- match(aic_best,model_names)
  aic_bestfunct <- eval(parse(text = summary[aic_bestnumb]))
  
  # Print the best models  
  print(paste0("The best model is ",aic_best))
  print(summary(aic_bestfunct))
  print(TukeyHSD(aic_bestfunct))
  
  # Unlist the models and model names into vectors
  return(data_frame(models = models, model_names = model_names, summary = summary))
}

#--------- Data import ---------

# Import the "datasæt" sheet from Excel file.
import_dataset <- read_excel(file_location, 
                             sheet = "Datasæt", skip = 5)


# Defining mapping data frame to translate to English, even if data is rearranged in excel
mapping <- data.frame(
  english_name = c("ID", "Birthdate", "Gender", "Date", "Bloodtest_time", "Time",
                   "Cortisol_NPU01787", "Cortisol_AAB00364", "Cortisol_AAB00260", "ACTH_NPU19744",
                   "IGF_1_NPU19829", "SD_Score_IGF_1_AAB00382", "Macimorelin_GH_Result",
                   "Cortisol_P_Cortisol", "Type", "GH_microg_L", "Glucose_mM", "Other_GH_Result", "Other_GH_Type"),
  danish_name = c("ID", "Fødselsdag", "Køn", "Dato", "BP klokkeslæt", "Tid",
                  "P-kortisol (NPU01787)", "P-kortisol (AAB00364)", "P-kortisol (AAB00260)", "ACTH (NPU19744)",
                  "IgF-I (NPU19829)", "SD score IGF-1 (AAB00382)",
                  "Macimorelin GH resultat", "Kortisol P-kortisol", "Type", "GH (microg/L)", "Glucose (mM)",
                  "Anden GH resultat", "Anden GH type")
)

# Use match() to find original positions based on English names
new_colnames <- mapping$english_name[match(colnames(import_dataset), mapping$danish_name)]

# Rename columns using the mapped positions
colnames(import_dataset) <- new_colnames
rm(new_colnames)
# Fix formatting for time and date from numeric

import_dataset$Bloodtest_time <- format(strptime(import_dataset$Bloodtest_time, "%Y-%m-%d %H:%M:%S"), format = "%H:%M")

# Remove data from before the date specified in Main Variables
import_dataset <- subset(import_dataset, import_dataset$Date > date_cutoff)

# Removing excluded data 
if(Remove19){import_dataset <- subset(import_dataset, !ID==19)}
if(Remove20){import_dataset <- subset(import_dataset, !ID==20)}

#--------- Creating dataframes ---------

# Create a cleaned main dataset with non-numeric values removed. 
# Uses the mapping-dataframe as a baseline, but skips the first 5 columns (date and time are non-numeric, but should be kept)
# SuppressWarnings suppresses warning of introduction of NA instead of non-numeric values, which is what is wanted.

suppressWarnings(main_dataset <- remove_non_numeric(import_dataset,mapping[-c(1:5), "english_name"]))
main_dataset <- main_dataset[, c("ID", "Gender", "Birthdate", "Date", "Bloodtest_time", "Time", "Cortisol_NPU01787", "Cortisol_AAB00364", "Cortisol_AAB00260", "Cortisol_P_Cortisol", "Type", "IGF_1_NPU19829", "SD_Score_IGF_1_AAB00382", "Macimorelin_GH_Result")]
#test_main_dataset <- main_dataset[rowSums(!is.na(main_dataset[, c("Cortisol_NPU01787", "Cortisol_AAB00364", "Cortisol_AAB00260", "Cortisol_P_Cortisol", "Type", "IGF_1_NPU19829", "SD_Score_IGF_1_AAB00382", "Macimorelin_GH_Result")])) != 0, ]
#reimport type-data
main_dataset$Type <- import_dataset$Type

rm(mapping)
rm(Remove19)
rm(Remove20)
rm(demography)

#------ Dataframe creation ------

# Importing columns from the main dataset
df_test <- main_dataset[, c("ID", "Birthdate", "Date", "Bloodtest_time", "Time", "Macimorelin_GH_Result",
                        "Cortisol_NPU01787", "Cortisol_AAB00364", "Cortisol_AAB00260", "Cortisol_P_Cortisol", "Type")]

# Checks the following columns, removes any that are empty in all
df_test <- df_test[rowSums(!is.na(df_test[, c("Macimorelin_GH_Result", "Cortisol_NPU01787", "Cortisol_AAB00364", "Cortisol_AAB00260", "Cortisol_P_Cortisol","Type")])) != 0, ]

# Creating a combined Cortisol column - However, all cortisol during macimorelin have data from NPU01787
df_test <- combine_columns(df_test, newcolname = "Cortisol", "Cortisol_NPU01787", "Cortisol_AAB00364", "Cortisol_AAB00260")

# Combine Date and Bloodtest_time using the lubridate package
df_test$Datetime <- paste0(df_test$Date, " ", df_test$Bloodtest_time)
df_test$Datetime <- ymd_hm(df_test$Datetime, tz = "")

df_test <- testdateid(df_test,"Macimorelin_GH_Result","Macimorelin_date")
df_test <- testdateid(df_test,"Cortisol","Cortisol_date")

# Finding tests where Cmax does not exceed the macimorelin GH-cutoff
df_test <- df_test %>%
  group_by(ID,Date) %>%
    mutate (
      GH_cmax = suppressWarnings(max(Macimorelin_GH_Result, na.rm = TRUE)),
      GH_over_cutoff = suppressWarnings(max(Macimorelin_GH_Result, na.rm = TRUE)) > cutoff_macimorelin
      )
df_test$GH_cmax[df_test$GH_cmax == "-Inf"] <- NA



# Removing all columns without macimorelin-test and cortisol measurement performed at that date
df_test <- subset(df_test, Macimorelin_date == TRUE & Cortisol_date == TRUE)

# Removing unnecessary columns
df_test <- df_test %>%
  select(ID, Datetime, Date, Birthdate, Time, Macimorelin_GH_Result, Cortisol, GH_over_cutoff)

# Combining rows if they are the same ID, date and time:
df_test <- df_test %>%
  group_by(ID,Datetime) %>%
  summarize(across(Date:GH_over_cutoff, ~toString(unique(na.omit(.x)))), .groups = 'drop')

# Replacing "" strings with NA for easy removal
df_test <- df_test %>%
  mutate(Macimorelin_GH_Result = replace(Macimorelin_GH_Result, Macimorelin_GH_Result == "", NA),
         Cortisol = replace(Cortisol, Cortisol == "", NA))

# Removing all columns without macimorelin-measurement and cortisol-measurement performed at the same time
df_test <- subset(df_test, !is.na(Macimorelin_GH_Result) & !is.na(Cortisol))

# Finding spot-synacthen during macimorelin cmax and start-value
df_test <- df_test %>%
  group_by(ID,Date) %>%
  mutate (
    maci_cortisol_cmax = suppressWarnings(max(Cortisol, na.rm = TRUE)),
    maci_cortisol_t0 = Cortisol[which(df_test$Time==0)[1]],
    maci_cortisol_over_cutoff = maci_cortisol_cmax > cutoff_synacthen
  ) %>%
  ungroup()

# Add numbering to multiple tests!

df_test <- numberingforeachfactor(df_test, columnname = "Testnumber", ID, Date)

df_test <- df_test %>%
  group_by(ID,Datetime) %>%
  mutate(IDandTest = as.numeric(paste0(ID,".",Testnumber)))

#----- Creating synacthen data ------
# Remove synacthen-data over 30 min?
longsynacthen <- FALSE

# Synachten test - Filtering for any kind of cortisol data

# Adding column with whether nearest synacthen-test is over cut-off, and dates between these.
# Need to figure out if it needs to be closest test, or if any tests in a given time-period is positive, due to uncertainty in synacthen-tests

# Creating synacthen dataframe
df_synacthen <- main_dataset[, c("ID", "Birthdate", "Date", "Bloodtest_time", "Time", "Type", "Cortisol_P_Cortisol")]
# Only keeping values where actually performed synacthen test
df_synacthen <- subset(df_synacthen, !is.na(df_synacthen$Cortisol_P_Cortisol))

# (Optional) remove synacthen 60min and 480 min
if(!longsynacthen){
  df_synacthen <- subset(df_synacthen, !df_synacthen$Time == "60")
  df_synacthen <- subset(df_synacthen, !df_synacthen$Time == "480")
}

# LC/MS or immunometry
df_synacthen <- df_synacthen %>%
  mutate(Variant = case_when(
    Type == "AAB00371" ~ "Immunometry",
    Type == "AAB00372" ~ "Immunometry",
    Type == "AAB00381" ~ "Immunometry",
    Type == "AAB00593" ~ "LC-MS",
    Type == "AAB00594" ~ "LC-MS",
    Type == "NPU04139" ~ "Immunometry",
    Type == "NPU04140" ~ "Immunometry",
    Type == "NPU04968" ~ "Immunometry",
    Type == "NPU04972" ~ "Immunometry",
    TRUE ~ NA,
  ))

# Numbering
df_synacthen <- numberingforeachfactor(df_synacthen, columnname = "Testnumber", ID, Date, Variant)
df_synacthen <- df_synacthen %>%
  group_by(ID,Date) %>%
  mutate(IDandTest = as.numeric(paste0(ID,".",Testnumber)))

# Combine Date and Bloodtest_time using the lubridate package
df_synacthen$Datetime <- paste0(df_synacthen$Date, " ", df_synacthen$Bloodtest_time)
df_synacthen$Datetime <- ymd_hm(df_synacthen$Datetime, tz = "")

if(FALSE) {
  # Boxplots
  df_synacthen$Variant <- as.factor(df_synacthen$Variant)
  df_synacthen$Cortisol_P_Cortisol <- as.numeric(df_synacthen$Cortisol_P_Cortisol)
  boxplot(df_synacthen$Cortisol_P_Cortisol~df_synacthen$Variant)
}

# Remove some variants of synacthen measuring?
if(exists("removeLCMS")) {
  if(removeLCMS) {
    df_synacthen <- subset(df_synacthen, !df_synacthen$Variant=="LC-MS")
  }}
if(exists("removeimmunometry")) {
  if(removeimmunometry) {
    df_synacthen <- subset(df_synacthen, !df_synacthen$Variant=="Immunometry")
  }}

# Adding data on synacthen-test over or under cut-off
df_synacthen <- df_synacthen %>%
  group_by(ID,Date) %>%
  mutate (
    No_synacthen = all(is.na(Cortisol_P_Cortisol)),
    Synacthen_over_cutoff = ifelse(No_synacthen, NA, suppressWarnings(max(Cortisol_P_Cortisol, na.rm = TRUE)) > cutoff_synacthen)
        )


# Add numbering to multiple tests!
df_synacthen <- numberingforeachfactor(df_synacthen, columnname = "Testnumber", ID, Date)

df_synacthen <- df_synacthen %>%
  group_by(ID,Datetime) %>%
  mutate(IDandTest = as.numeric(paste0(ID,".",Testnumber))) %>%
  #mutate(IDandTest = paste0(ID,".",Testnumber," (",Date,")")) %>%
  mutate(IDTestDateVariant = paste0(ID,".",Testnumber," (",Date,", ",Variant,")"))



#----- Inserting synacthen data into main dataset ----

    
    # Always prioritizes the Synacthen-test nearest to the Macimorelin-test 
    df_test$Date <- as.Date(df_test$Date)
    df_synacthen$Date <- as.Date(df_synacthen$Date)
    
    #Index for tests after
    index1 <- neardate(df_test$ID, df_synacthen$ID, df_test$Date, df_synacthen$Date)
    
    # Index for tests prior
    index2 <- neardate(df_test$ID, df_synacthen$ID, df_test$Date, df_synacthen$Date, 
                       best="prior")
    
    # Combining tests prior and after
    index <- ifelse(is.na(index1), index2, # if none after, take before
                    ifelse(is.na(index2), index1, # if none before, take after (which is NA)
                           ifelse(abs(df_synacthen$Date[index2]- df_test$Date) <
                                    abs(df_synacthen$Date[index1]- df_test$Date), index2, index1))) # if both before and after, find closest
    
    # Making sure they are numeric
    index <- as.numeric(index)
    index2 <- as.numeric(index2)
    
    # Filtering whether to only compare to dates dates prior to the Macimorelin/cortisol test
    if(OnlyPreviousDates){
      df_test$ClosestDatePrior <- df_synacthen[index2, "Date"] # Only prior dates
      df_test$Datedifference <- abs(df_test$ClosestDatePrior$Date - df_test$Date)
    } else {
      df_test$ClosestDate <- df_synacthen[index, "Date"]  # All dates
      df_test$Datedifference <- abs(df_test$ClosestDate$Date - df_test$Date)
    }
    
    # Also import data on Synacthen_over_cutoff for those specific dates
    if(OnlyPreviousDates){
      df_test$Synacthen_over_cutoff <- df_synacthen[index2, "Synacthen_over_cutoff"] # Only prior dates
    } else {
      df_test$Synacthen_over_cutoff <- df_synacthen[index, "Synacthen_over_cutoff"]  # All dates
    }
    #df_test$Synacthen_over_cutoff[is.na(df_test$ClosestDate$Date)] <- FALSE

    #----- Inserting IGF-I data into main dataset ----

    # Importing columns from the main dataset
    IGF <- main_dataset[, c("ID", "Gender", "Birthdate", "Date", "IGF_1_NPU19829")]
    
    # Checks if there is data in IGF1, removing if there isn't any
    IGF <- subset(IGF, !is.na(IGF["IGF_1_NPU19829"]))
    
    # Calculating age at IGF1
    IGF <- IGF %>%
      mutate(
        AgeAtTest = round(as.numeric((Date-Birthdate)/365),1),
        IGF = IGF_1_NPU19829
      )
    
    
    IGF1Men <- suppressWarnings(read_excel(file_location, 
                          sheet = "IGF1 Men", col_types = c("numeric", 
                                                            "skip", "skip", "skip", "skip", "skip", 
                                                            "skip", "skip", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric")))
    
    IGF1Women <- suppressWarnings(read_excel(file_location, 
                            sheet = "IGF1 Women", col_types = c("numeric", 
                                                                "skip", "skip", "skip", "skip", "skip", 
                                                                "skip", "skip", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric")))
    
    # ---- Appending data on over minimum IGF1: -----
    
    closest_value <- function(df, df_ref) {
      index <- list()
      for (i in 1:nrow(df)) {
        current_age <- df$AgeAtTest[i]
        # Find the index of the closest age in df_ref
        closest_index <- which.min(abs(df_ref$Age - current_age))
        # Extract the minimum value for that age using the index
        minvalue <- df_ref$neg2SD[closest_index]
        index <- append(index, as.numeric(as.character(minvalue)))
      }
      return(index)
    }
    
    # Appending values to beat
    IGF$Cutoff_Men <- closest_value(IGF, IGF1Men)
    IGF$Cutoff_Women <- closest_value(IGF, IGF1Women)
    
    IGF <- IGF %>%
      mutate(
        IGF_over_cutoff = case_when(
          Gender == "Man" ~ IGF > Cutoff_Men,
          Gender == "Woman" ~ IGF > Cutoff_Women,
          TRUE ~ NA  # If error
        )
      )
    
    # Always prioritizes the IGF-I -test nearest to the Macimorelin-test 
    df_test$Date <- as.Date(df_test$Date)
    IGF$Date <- as.Date(IGF$Date)
    
    #Index for tests after
    index1 <- neardate(df_test$ID, IGF$ID, df_test$Date, IGF$Date)
    
    # Index for tests prior
    index2 <- neardate(df_test$ID, IGF$ID, df_test$Date, IGF$Date, 
                       best="prior")
    

    
    # Combining tests prior and after
    index <- ifelse(is.na(index1), index2, # if none after, take before
                    ifelse(is.na(index2), index1, # if none before, take after (which is NA)
                           ifelse(abs(IGF$Date[index2]- df_test$Date) <
                                    abs(IGF$Date[index1]- df_test$Date), index2, index1))) # if both before and after, find closest

    
    # Making sure they are numeric
    index <- as.numeric(index)
    index1 <- as.numeric(index1)
    index2 <- as.numeric(index2)

    
    # Filtering whether to only compare to dates dates prior to the Macimorelin/cortisol test
    if(OnlyPreviousDates){
      df_test$IGFClosestDate <- IGF[index2, "Date"] # Only prior dates
      df_test$DatedifferenceIGF <- abs(df_test$IGFClosestDate$Date - df_test$Date)
    } else {
      df_test$IGFClosestDate <- IGF[index, "Date"]  # All dates
      df_test$DatedifferenceIGF <- (df_test$IGFClosestDate$Date - df_test$Date)
    }
    
    # Also import data on IGF for those specific dates
    if(OnlyPreviousDates){
      df_test$IGF_over_cutoff <- IGF[index2, "IGF_over_cutoff"] # Only prior dates
    } else {
      df_test$IGF_over_cutoff <- IGF[index, "IGF_over_cutoff"]  # All dates
    }
    df_test$IGF_over_cutoff[is.na(df_test$IGF_over_cutoff)] <- FALSE
    
    
    rm(index)
    rm(index1)
    rm(index2)


# Renaming by removing the tibbles
test <- (df_test$Synacthen_over_cutoff$Synacthen_over_cutoff)
df_test$Synacthen_over_cutoff <- test
test <- (df_test$ClosestDate$Date)
df_test$ClosestDate <- test

test <- (df_test$IGF_over_cutoff$IGF_over_cutoff)
df_test$IGF_over_cutoff <- test
test <- (df_test$IGFClosestDate$Date)
df_test$IGFClosestDate <- test


rm(test)

#--- Adding linear fit calculation ---#

df_test <- df_test %>%
  group_by(ID,Date) %>%
  mutate(
      Cort = Cortisol,
      NumTime = as.numeric(as.character(Time)),
      LinearSlope = {
        round((lm(Cortisol ~ as.numeric(as.character(Time))))$coefficients[2],digits = 3)
      },
      LinearSlopePositive = {
        LinearSlope>0
      },
      LinearRsquared = {
        round(as.numeric(summary(lm(Cortisol ~ as.numeric(as.character(Time))))[8]),digits = 3)
      }
    )


  

#---- Also adding spot-cortisol to the ones with missing Synacthen data -----

# Cortisol test - Filtering for any kind of cortisol data
df_cortisoltest <- main_dataset[, c("ID", "Birthdate", "Date", "Bloodtest_time", "Time", "Type",
                                    "Cortisol_NPU01787", "Cortisol_AAB00364", "Cortisol_AAB00260", 
                                    if(RemoveCortisolDuringMacimorelin){"Macimorelin_GH_Result"},
                                    if(removeIDsWithSynacthen){"Cortisol_P_Cortisol"})]

# Remove any rows that are empty in all columns.
df_cortisoltest <- df_cortisoltest[rowSums(!is.na(df_cortisoltest[, c(
  "Type", "Cortisol_NPU01787", "Cortisol_AAB00364", "Cortisol_AAB00260", 
  if(RemoveCortisolDuringMacimorelin){"Macimorelin_GH_Result"},
  if(removeIDsWithSynacthen){"Cortisol_P_Cortisol"})])) != 0, ]


# Creating a combined Cortisol column

df_cortisoltest <- combine_columns(df_cortisoltest, "Cortisol_NPU01787", "Cortisol_AAB00364", "Cortisol_AAB00260")


#------- Creating data necessary for creating plots -------

# Combine Date and Bloodtest_time using the lubridate package
df_cortisoltest$Datetime <- paste0(df_cortisoltest$Date, " ", df_cortisoltest$Bloodtest_time)
df_cortisoltest$Datetime <- ymd_hm(df_cortisoltest$Datetime, tz = "")


# Applying the time_from baseline function by grouping by ID and Date
df_cortisoltest <- df_cortisoltest %>%
  group_by(ID, Date) %>%
  do(calculate_relative_time(.))

# (Optional) remove all IDs with synacthen test
if(removeIDsWithSynacthen){
  
  # Making sure to keep those not tagged for removal!

  # LC/MS or immunometry
  df_cortisoltest <- df_cortisoltest %>%
    mutate(Variant = case_when(
      Type == "AAB00371" ~ "Immunometry",
      Type == "AAB00372" ~ "Immunometry",
      Type == "AAB00381" ~ "Immunometry",
      Type == "AAB00593" ~ "LC-MS",
      Type == "AAB00594" ~ "LC-MS",
      Type == "NPU04139" ~ "Immunometry",
      Type == "NPU04140" ~ "Immunometry",
      Type == "NPU04968" ~ "Immunometry",
      Type == "NPU04972" ~ "Immunometry",
      TRUE ~ NA,
    ))
  
  # Remove some variants of synacthen measuring? - Removes the opposite!
  if(exists("removeLCMS")) {
    if(!removeLCMS) {
      df_cortisoltest <- subset(df_cortisoltest, !df_cortisoltest$Variant=="LC-MS"|is.na(df_cortisoltest$Variant))
    }}
  if(exists("removeimmunometry")) {
    if(!removeimmunometry) {
      df_cortisoltest <- subset(df_cortisoltest, !df_cortisoltest$Variant=="Immunometry"|is.na(df_cortisoltest$Variant))
    }}


  df_cortisoltest <- df_cortisoltest %>%
    group_by(ID) %>%
    filter(all(is.na(Cortisol_P_Cortisol)|!is.na(Variant)))

}

# (Optional) remove all cortisol-datapoints that are used during macimorelin-test
if(RemoveCortisolDuringMacimorelin){
  # Combining rows if they are the same ID, date and time:
  df_cortisoltest <- df_cortisoltest %>%
    group_by(ID,Datetime) %>%
    summarize(across(Birthdate:Variant, ~toString(unique(na.omit(.x)))), .groups = 'drop')

  #Removing all with Macimorelin-data
  df_cortisoltest <- df_cortisoltest %>%
    mutate(Macimorelin_GH_Result = replace(Macimorelin_GH_Result, Macimorelin_GH_Result == "", NA))
  # Except for time = 0!
  df_cortisoltest <- subset(df_cortisoltest, is.na(df_cortisoltest$Macimorelin_GH_Result)|df_cortisoltest$Time==0)
}

# Removing all non-Cortisol data
df_cortisoltest <- df_cortisoltest %>%
  mutate(Combined = replace(Combined, Combined == "", NA))
df_cortisoltest$Combined <- as.numeric(df_cortisoltest$Combined)

df_cortisoltest$Cortisolperformed <- !is.na(df_cortisoltest$Combined)
df_cortisoltest <- subset(df_cortisoltest, df_cortisoltest$Cortisolperformed == TRUE)


# Add numbering to multiple tests!
df_cortisoltest <- numberingforeachfactor(df_cortisoltest, columnname = "Testnumber", ID, Date)

df_cortisoltest <- df_cortisoltest %>% filter(Variant == "")


# Create IDandTest, by Macimorelin-test
df_cortisoltest <- df_cortisoltest %>%
  group_by(ID) %>%
  mutate(
    Time = as.numeric(Time),
    Date = as.Date(Date),
    IDandTest = paste0(ID, ".",cumsum(!is.na(Time))),
    IDandTest = ifelse(Time != 0, NA, IDandTest)
  )


df_cortisoltest <- df_cortisoltest %>%
  group_by(ID) %>%
  mutate(
    withinayear = abs(Date-min(Date[!is.na(IDandTest)])) <= 365 | abs(Date-max(Date[!is.na(IDandTest)])) <= 365
  )

df_cortisoltest <- subset(df_cortisoltest, withinayear == TRUE)


as.Date(df_cortisoltest$Date[2])-as.Date(df_cortisoltest$Date[20])
# Finding Cmax and Tmax for each
df_cortisoltest <- df_cortisoltest %>%
  group_by(IDandTest) %>%
  mutate (Cmax = max(Combined)) %>%
  mutate (if(!RemoveCortisolDuringMacimorelin){Tmax = Time_from_baseline[which.max(Combined)]}) %>%
  mutate (NormalCortisol = Cmax > cutoff_synacthen)

# Adding data on whether or not they are over cutoff for spot-cortisol
if(SpotCortiToSynacthen_over_cutoff){
  # Creating new dataframe
  SpotCorti <- df_cortisoltest[, c("ID", "Date", "Bloodtest_time", "Combined")]
  # Renaming for clarity
  SpotCorti <- mutate(SpotCorti, SpotCortisol = Combined)
  # Remove all but largest number for all
  SpotCorti <- SpotCorti %>%
    group_by(ID) %>%
    mutate(SpotCortisolMax = suppressWarnings(max(SpotCortisol, na.rm = TRUE)),
           SpotCortisolTime = Bloodtest_time[which.max(SpotCortisolMax)],
           SpotCortisolDate = Date[which.max(SpotCortisolMax)]) %>%
    summarize(across(Date:SpotCortisolDate, ~toString(unique(na.omit(.x)))), .groups = 'drop')
  
  # Adding the test
  SpotCorti <- SpotCorti %>%
    mutate( SpotCorti_OverCutoff = SpotCortisolMax > SpotCortiCutoff)
  
  # Simplifying
  SpotCorti <- SpotCorti[, c("ID", "SpotCortisolDate", "SpotCortisolTime", "SpotCorti_OverCutoff")]
}

if(SpotCortiToSynacthen_over_cutoff){
  
  df_test <- df_test %>%
    left_join(SpotCorti, by = "ID")

if(!substudy){
  df_test <- df_test %>%
    mutate(NormalCortisol = ifelse(is.na(Synacthen_over_cutoff), SpotCorti_OverCutoff, Synacthen_over_cutoff))
  
  df_test$NormalCortisol[is.na(df_test$NormalCortisol)] <- FALSE
}
  if(substudy){
    df_test <- df_test %>%
      mutate(NormalCortisol = Synacthen_over_cutoff)
  df_test <- subset(df_test, !is.na(NormalCortisol))
  }
  
}
