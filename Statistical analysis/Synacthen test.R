# Load main script
source("Main tests.R")

# Create a dataframe only for plotting
# Remove tests where only one data-point is registered (correctly)
df_synacthenplot <- df_synacthen %>%
  group_by(IDandTest) %>%
  filter(n() > 1) %>%  # Filter groups with more than 1 row
  ungroup() 



# ----- Plots -----
if(createplots){
  
  Synacthen <- list()
  if(FALSE){
    Synacthen$ID1 <- group_plot(1,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID2 <- group_plot(2,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID3 <- group_plot(3,df_synacthenplot,standardtest = "Synacthen") #Ingen adgang
    Synacthen$ID4 <- group_plot(4,df_synacthenplot,standardtest = "Synacthen") # Ingen test?
    Synacthen$ID5 <- group_plot(5,df_synacthenplot,standardtest = "Synacthen") #Ingen vellykket test
    Synacthen$ID6 <- group_plot(6,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID7 <- group_plot(7,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID8 <- group_plot(8,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID9 <- group_plot(9,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID10 <- group_plot(10,df_synacthenplot,standardtest = "Synacthen") #Ingen adgang
    Synacthen$ID11 <- group_plot(11,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID12 <- group_plot(12,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID13 <- group_plot(13,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID14 <- group_plot(14,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID15 <- group_plot(15,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID16 <- group_plot(16,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID17 <- group_plot(17,df_synacthenplot,standardtest = "Synacthen")
    Synacthen$ID18 <- group_plot(18,df_synacthenplot,standardtest = "Synacthen") #Ingen adgang
    Synacthen$ID19 <- group_plot(19,df_synacthenplot,standardtest = "Synacthen")
  }
  Synacthen$NullID <- group_plot(ID = NULL, 
                                 standardtest = NULL,
                                 input_dataframe = df_synacthenplot,
                                 group_by = "IDandTest",
                                 group_by_legend = "ID and test-number",
                                 x_axis = "Time",
                                 x_axis_name = "Time since baseline blood-test (min)",
                                 y_axis = "Cortisol_P_Cortisol",
                                 y_axis_name = "Cortisol response (nmol/L)",
                                 appendID = FALSE, 
                                 title = "Serum Cortisol during Synacthen test",
                                 meancurve = TRUE,
                                 showlines = TRUE,
                                 linear = FALSE,
                                 autoYlimits = FALSE,
                                 autoXlimits = FALSE,
                                 ymin = 0, 
                                 ymax = 900,
                                 xmin = 0,
                                 xmax = 30,
                                 cutoff = cutoff_synacthen,
                                 customcolors = FALSE,
                                 alpha = 0.7)
  
  if(showplots){showggplot(Synacthen, "null")}
  if(saveplots){saveggplot(Synacthen)}
  
}

plot <- ggplot(df_synacthen, aes(x = Time, y = Cortisol_P_Cortisol)) +
  theme_classic2() +
  scale_color_manual(values = custompalette) +
  labs(color = "Time") +
  geom_boxplot(aes(group = Time, color = factor(Time)), notch = FALSE, ) +
  geom_hline(yintercept= cutoff_synacthen, linetype="dashed", color = "red", size=0.5) +
  geom_jitter(shape = 16, position = position_jitter(w = 6, h = 0), aes(color = factor(Time)), alpha = 0.6) +
  geom_smooth(aes(), color = "black") +
  scale_x_continuous("Synacthen test time (minutes)", breaks = seq(0,30,30)) +
  labs(
    title = "Serum Cortisol during Synacthen test, boxplot",
    y = "Cortisol (nmol/L)"
  )
if(showplots){plot}
if(saveplots){ggsave(filename = "Synacthen test boxplot.png", plot = plot,
                     path = save_file_location,
                     width = , 
                     height = )}