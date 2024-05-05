source("Main tests.R")

df_synacthencompare <- df_synacthen

if(removeimmunometry | removeLCMS) {
  stop("either removeimmunometry or removeLCMS is active!! Cannot compare these!")
  }

# Optional - Removing synacthen-data over 60 min
if(!longsynacthen){
  df_synacthencompare <- subset(df_synacthencompare, !df_synacthencompare$Time == "480")
}


#----- Testing if ID have had more than one variant of blood-test

# Test if both types present on the same day
df_synacthencompare <- df_synacthencompare %>%
  group_by(ID,Date) %>%
  mutate(Hasboth = if(any(Variant %in% "LC-MS") & any(Variant %in% "Immunometry")) {TRUE} else {FALSE})

if(onlyrunpaired){
# Remove columns without both types
df_synacthencompare <- subset(df_synacthencompare, df_synacthencompare$Hasboth == TRUE)
}

# Boxplots of these
  df_synacthencompare$Variant <- as.factor(df_synacthencompare$Variant)
  df_synacthencompare$Cortisol_P_Cortisol <- as.numeric(df_synacthencompare$Cortisol_P_Cortisol)
if(showplots){
  boxplot(df_synacthencompare$Cortisol_P_Cortisol~df_synacthencompare$Variant)
  }
#----- Graphing and t-tests
  
  # Combine Date and Bloodtest_time using the lubridate package
  df_synacthencompare$Datetime <- paste0(df_synacthencompare$Date, " ", df_synacthencompare$Bloodtest_time)
  df_synacthencompare$Datetime <- ymd_hm(df_synacthencompare$Datetime, tz = "")
  
  df_synacthencompare <- numbering(df_synacthencompare,"Number",Datetime)

# Creating a dataframe with the variants as different columns
  df_synacthencompare_immuno <- subset(df_synacthencompare, df_synacthencompare$Variant == "Immunometry")
  df_synacthencompare_LCMS <- subset(df_synacthencompare, df_synacthencompare$Variant == "LC-MS")
if(onlyrunpaired) {
  df_synacthencompare_combo <- df_synacthencompare_immuno
  df_synacthencompare_combo$immuno <- df_synacthencompare_immuno$Cortisol_P_Cortisol
  df_synacthencompare_combo$LCMS <- df_synacthencompare_LCMS$Cortisol_P_Cortisol

#Plotting against an agreement line  
plot <- group_plot(ID = NULL, 
           standardtest = NULL, 
           input_dataframe = df_synacthencompare_combo,
           group_by = NULL,
           group_by_legend = ,
           x_axis = "immuno",
           x_axis_name = "Roche immunoassay (nmol/L)",
           y_axis = "LCMS",
           y_axis_name = "LC-MS/MS (nmol/L)",
           appendID = FALSE, 
           title = "Cortisol during Synacthen test, LC-MS/MS versus immunoassay", 
           subtitle = "plotted against an agreement line",
           meancurve = TRUE,
           showlines = FALSE,
           linear = FALSE,
           cutoff = NULL, 
           autoXlimits = FALSE, 
           autoYlimits = FALSE, 
           monocolor = "#3cb44b",
           xmin = 100, 
           xmax = 900, 
           ymin = 100, 
           ymax = 900,
           agreementline = TRUE)
if(showplots){plot}
if(saveplots){ggsave(filename = "LCMS vs Immunoassay agreement line.png", plot = plot,
                     path = save_file_location,
                     width = 7.1, 
                     height = 5.5)}
}
  
#---------
if(onlyrunpaired) {
plot <- blandr.draw(df_synacthencompare_combo$immuno,df_synacthencompare_combo$LCMS)
if(showplots){plot}
if(saveplots){ggsave(filename = "Bland-Altman plot LCMS vs immunoassay.png", plot = plot,
                     path = save_file_location,
                     width = , 
                     height = )}

if(FALSE){
group_plot(ID = NULL, 
           standardtest = NULL, 
           input_dataframe = df_synacthencompare,
           group_by = "IDTestDateVariant",
           group_by_legend = "Measuring modality",
           x_axis = "Time",
           x_axis_name = "Time",
           y_axis = "Cortisol_P_Cortisol",
           y_axis_name = "Cortisol (XX/XX)",
           appendID = FALSE, 
           title = "Cortisol during Synacthen-test as a function of time, 
           grouped by ID, date and measuring modality",
           meancurve = FALSE,
           showlines = TRUE,
           linear = FALSE,
           cutoff = cutoff_synacthen, 
           autoXlimits = TRUE, 
           autoYlimits = TRUE, 
           xmin = , 
           xmax = , 
           ymin = , 
           ymax = 
)
}
}
  if(!onlyrunpaired) {
    #Plotting
    group_plot(ID = NULL, 
               standardtest = NULL, 
               input_dataframe = df_synacthencompare,
               group_by = "IDTestDateVariant",
               group_by_legend = "Measuring modality",
               x_axis = "Time",
               x_axis_name = "Time",
               y_axis = "Cortisol_P_Cortisol",
               y_axis_name = "Cortisol (XX/XX)",
               appendID = FALSE, 
               title = "Cortisol during Synacthen-test as a function of time, grouped by ID, date and measuring modality",
               meancurve = FALSE,
               showlines = TRUE,
               linear = FALSE,
               cutoff = cutoff_synacthen, 
               autoXlimits = TRUE, 
               autoYlimits = TRUE, 
               xmin = 100, 
               xmax = 900, 
               ymin = 100, 
               ymax = 900,
               )
  }

#------ Students t test and density plotting between time 0 and time 30

# Splitting up at time 0 and time 30
  
# Needs to be presented separately!
  
  # LCMS
  df_synacthencompare_LCMS_0 <- subset(df_synacthencompare_LCMS, df_synacthencompare_LCMS$Time == 0)
  df_synacthencompare_LCMS_30 <- subset(df_synacthencompare_LCMS, df_synacthencompare_LCMS$Time == 30)
  # Immuno
  df_synacthencompare_immuno_0 <- subset(df_synacthencompare_immuno, df_synacthencompare_immuno$Time == 0)
  df_synacthencompare_immuno_30 <- subset(df_synacthencompare_immuno, df_synacthencompare_immuno$Time == 30)


# Shapiro at time 0, immuno
shapirotest <- as.numeric(shapiro.test(df_synacthencompare_immuno_0$Cortisol_P_Cortisol)[2])
shapiro <- if(shapirotest >= 0.05) {paste0("Fail to reject normality - p-value: ",round(shapirotest,6))} else {paste0("Not normally distributed! - p-value: ",round(shapirotest,6))}


plot <- ggdensity(df_synacthencompare_immuno_0$Cortisol_P_Cortisol,
          title = "Density plot of Cortisol concentrations during synacthen 
at time 0 min, measured with immunoassay",
          subtitle = shapiro,
          xlab = "Cortisol (nmol/L)",ylab = "Density")
if(showplots){plot}
if(saveplots){ggsave(filename = "Density plot 0immuno.png", plot = plot,
                     path = save_file_location,
                     width = , 
                     height = )}

# Shapiro at time 30, immuno
shapirotest <- as.numeric(shapiro.test(df_synacthencompare_immuno_30$Cortisol_P_Cortisol)[2])
shapiro <- if(shapirotest >= 0.05) {paste0("Fail to reject normality - p-value: ",round(shapirotest,6))} else {paste0("Not normally distributed! - p-value: ",round(shapirotest,6))}


plot <- ggdensity(df_synacthencompare_immuno_30$Cortisol_P_Cortisol,
          title = "Density plot of Cortisol concentrations during synacthen 
at time 30 min, measured with immunoassay",
          subtitle = shapiro,
          xlab = "Cortisol (nmol/L)",ylab = "Density")
if(showplots){plot}
if(saveplots){ggsave(filename = "Density plot 30immuno.png", plot = plot,
                     path = save_file_location,
                     width = , 
                     height = )}
  
  

# Shapiro at time 0, LCMS
shapirotest <- as.numeric(shapiro.test(df_synacthencompare_LCMS_0$Cortisol_P_Cortisol)[2])
shapiro <- if(shapirotest >= 0.05) {paste0("Fail to reject normality - p-value: ",round(shapirotest,6))} else {paste0("Not normally distributed! - p-value: ",round(shapirotest,6))}


plot <- ggdensity(df_synacthencompare_LCMS_0$Cortisol_P_Cortisol,
          title = "Density plot of Cortisol concentrations during synacthen
at time 0 min, measured with LC-MS/MS",
          subtitle = shapiro,
          xlab = "Cortisol (nmol/L)",ylab = "Density")
if(showplots){plot}
if(saveplots){ggsave(filename = "Density plot 0LCMS.png", plot = plot,
                     path = save_file_location,
                     width = , 
                     height = )}

# Shapiro at time 30, LCMS
shapirotest <- as.numeric(shapiro.test(df_synacthencompare_LCMS_30$Cortisol_P_Cortisol)[2])
shapiro <- if(shapirotest >= 0.05) {paste0("Fail to reject normality - p-value: ",round(shapirotest,6))} else {paste0("Not normally distributed! - p-value: ",round(shapirotest,6))}


plot <- ggdensity(df_synacthencompare_LCMS_30$Cortisol_P_Cortisol,
          title = "Density plot of Cortisol concentrations during synacthen 
at time 30 min, measured with LC-MS/MS",
          subtitle = shapiro,
          xlab = "Cortisol (nmol/L)",ylab = "Density")
if(showplots){plot}
if(saveplots){ggsave(filename = "Density plot 30LCMS.png", plot = plot,
                     path = save_file_location,
                     width = , 
                     height = )}

#------ T-testing: ------

ttest <- if(as.numeric(t.test(df_synacthencompare_combo$immuno, df_synacthencompare_combo$LCMS)[3]) >= 0.05) {
  paste0("p = ",round(as.numeric(t.test(df_synacthencompare_combo$immuno, 
                                        df_synacthencompare_combo$LCMS)[3]),6),
         " - ","No significant difference between methods!")
} else {
  paste0("p = ",as.numeric(t.test(df_synacthencompare_combo$immuno, 
                                  df_synacthencompare_combo$LCMS)[3]),
         " - ","Statistical significant difference between methods!")}
print(ttest)
rm(ttest)

# The closer to 1, the closer to normal distribution
# The closer to 0, the further from normal distribution
if(FALSE){
print("T-tests")
print("# The closer to 1, the closer to normal distribution, the closer to 0, the further from normal distribution.")

# At time 0
print("At time 0")
print(if(as.numeric(t.test(df_synacthencompare_LCMS_0$Cortisol_P_Cortisol, df_synacthencompare_immuno_0$Cortisol_P_Cortisol)[3]) >= 0.05) {paste0("p = ",round(as.numeric(t.test(df_synacthencompare_LCMS_0$Cortisol_P_Cortisol, df_synacthencompare_immuno_0$Cortisol_P_Cortisol)[3]),6)," - ","No significant difference between LCMS and immunoassay!")} else {paste0("p = ",round(as.numeric(t.test(df_synacthencompare_LCMS_0$Cortisol_P_Cortisol, df_synacthencompare_immuno_0$Cortisol_P_Cortisol)[3]),6)," - ","Statistical significant difference between LCMS and immunoassay!")})
# At time 30
print("At time 30")
print(if(as.numeric(t.test(df_synacthencompare_LCMS_30$Cortisol_P_Cortisol, df_synacthencompare_immuno_30$Cortisol_P_Cortisol)[3]) >= 0.05) {paste0("p = ",round(as.numeric(t.test(df_synacthencompare_LCMS_30$Cortisol_P_Cortisol, df_synacthencompare_immuno_30$Cortisol_P_Cortisol)[3]),6)," - ","No significant difference between LCMS and immunoassay!")} else {paste0("p = ",round(as.numeric(t.test(df_synacthencompare_LCMS_30$Cortisol_P_Cortisol, df_synacthencompare_immuno_30$Cortisol_P_Cortisol)[3]),6)," - ","Statistical significant difference between LCMS and immunoassay!")})

# Between time 0 and 30
print("Between time 0 and 30, LCMS")
print(if(as.numeric(t.test(df_synacthencompare_LCMS_0$Cortisol_P_Cortisol, df_synacthencompare_LCMS_30$Cortisol_P_Cortisol)[3]) >= 0.05) {paste0("p = ",round(as.numeric(t.test(df_synacthencompare_LCMS_0$Cortisol_P_Cortisol, df_synacthencompare_LCMS_30$Cortisol_P_Cortisol)[3]),6)," - ","No significant difference for LCMS at 0 and 30 min!")} else {paste0("p = ",round(as.numeric(t.test(df_synacthencompare_LCMS_0$Cortisol_P_Cortisol, df_synacthencompare_LCMS_30$Cortisol_P_Cortisol)[3]),6)," - ","Statistical significant difference for LCMS at 0 and 30 min!")})
print("Between time 0 and 30, Immunoassay")
print(if(as.numeric(t.test(df_synacthencompare_immuno_0$Cortisol_P_Cortisol, df_synacthencompare_immuno_30$Cortisol_P_Cortisol)[3]) >= 0.05) {paste0("p = ",round(as.numeric(t.test(df_synacthencompare_immuno_0$Cortisol_P_Cortisol, df_synacthencompare_immuno_30$Cortisol_P_Cortisol)[3]),6)," - ","No significant difference for immunoassay at 0 and 30 min!")} else {paste0("p = ",round(as.numeric(t.test(df_synacthencompare_immuno_0$Cortisol_P_Cortisol, df_synacthencompare_immuno_30$Cortisol_P_Cortisol)[3]),6)," - ","Statistical significant difference for immunoassay at 0 and 30 min!")})
}
# Remove work-dataframes again
if(onlyrunpaired) {rm(df_synacthencompare_combo)}
rm(df_synacthencompare_immuno)
rm(df_synacthencompare_immuno_0)
rm(df_synacthencompare_immuno_30)
rm(df_synacthencompare_LCMS)
rm(df_synacthencompare_LCMS_0)
rm(df_synacthencompare_LCMS_30)

