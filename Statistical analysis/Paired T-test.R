# Paired T-tests
source("Main tests.R")

# Creating a new dataframe based on df_test
paired_df_test <- df_test

# Removing unnecessary columns
paired_df_test <- paired_df_test %>%
  select(ID, Date, Datetime, Birthdate, maci_cortisol_t0, maci_cortisol_cmax, Synacthen_over_cutoff, IDandTest)

# Combining rows if they are the same ID, date and time:
paired_df_test <- paired_df_test %>%
  group_by(IDandTest) %>%
  summarize(across(ID:maci_cortisol_cmax, ~toString(unique(na.omit(.x)))), .groups = 'drop')

# Making the cortisol columns numeric

paired_df_test$maci_cortisol_t0 <- as.numeric(paired_df_test$maci_cortisol_t0)
paired_df_test$maci_cortisol_cmax <- as.numeric(paired_df_test$maci_cortisol_cmax)


# Shapiro test for normality
# Cortisol at t0
shapirotest <- as.numeric(shapiro.test(paired_df_test$maci_cortisol_t0)[2])
shapiro <- if(shapirotest >= 0.05) {paste0("Fail to reject normality - p-value: ",round(shapirotest,6))} else {paste0("Not normally distributed! - p-value: ",round(shapirotest,6))}
plot <- ggdensity(paired_df_test$maci_cortisol_t0,
                  title = "Density plot of Cortisol concentrations during macimorelin-test at 0 min",
                  subtitle = shapiro,
                  xlab = "Cortisol (nmol/L)",ylab = "Density")

if(showplots){plot}
if(saveplots){ggsave(filename = "Density plot t0.png", plot = plot,
                     path = save_file_location,
                     width = , 
                     height = )}



# Cortisol at cmax
shapirotest <- as.numeric(shapiro.test(paired_df_test$maci_cortisol_cmax)[2])
shapiro <- if(shapirotest >= 0.05) {paste0("Fail to reject normality - p-value: ",round(shapirotest,6))} else {paste0("Not normally distributed! - p-value: ",round(shapirotest,6))}
plot <- ggdensity(paired_df_test$maci_cortisol_cmax,
                  title = "Density plot of Cortisol concentrations during macimorelin-test at cmax",
                  subtitle = shapiro,
                  xlab = "Cortisol (nmol/L)",ylab = "Density")

if(showplots){plot}
if(saveplots){ggsave(filename = "Density plot cmax.png", plot = plot,
                     path = save_file_location,
                     width = , 
                     height = )}

# T-test
if(as.numeric(t.test(paired_df_test$maci_cortisol_t0, paired_df_test$maci_cortisol_cmax)[3]) >= 0.05) {paste0("p = ",round(as.numeric(t.test(paired_df_test$maci_cortisol_t0, paired_df_test$maci_cortisol_cmax)[3]),6)," - ","No significant difference between t0 and Cmax!")} else {paste0("p = ",as.numeric(t.test(paired_df_test$maci_cortisol_t0, paired_df_test$maci_cortisol_cmax)[3])," - ","Statistical significant difference between t0 and Cmax!")}
print(if(as.numeric(t.test(paired_df_test$maci_cortisol_t0, paired_df_test$maci_cortisol_cmax)[3]) >= 0.05) {paste0("p = ",round(as.numeric(t.test(paired_df_test$maci_cortisol_t0, paired_df_test$maci_cortisol_cmax)[3]),6)," - ","No significant difference between t0 and Cmax!")} else {paste0("p = ",as.numeric(t.test(paired_df_test$maci_cortisol_t0, paired_df_test$maci_cortisol_cmax)[3])," - ","Statistical significant difference between t0 and Cmax!")})
