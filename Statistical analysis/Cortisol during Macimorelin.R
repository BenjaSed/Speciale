source("Main tests.R")

# Testing plots
if(createplots) {
  
  CortisolDuringMacimorelin <- list()
  
  if(FALSE){
    CortisolDuringMacimorelin$ID1 <- group_plot(ID = 1, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID2 <- group_plot(ID = 2, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID3 <- group_plot(ID = 3, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID4 <- group_plot(ID = 4, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID5 <- group_plot(ID = 5, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID6 <- group_plot(ID = 6, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID7 <- group_plot(ID = 7, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID8 <- group_plot(ID = 8, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID9 <- group_plot(ID = 9, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID10 <- group_plot(ID = 10, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID11 <- group_plot(ID = 11, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID12 <- group_plot(ID = 12, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID13 <- group_plot(ID = 13, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID14 <- group_plot(ID = 14, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID15 <- group_plot(ID = 15, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID16 <- group_plot(ID = 16, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID17 <- group_plot(ID = 17, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID18 <- group_plot(ID = 18, standardtest = "CortisolDuringMacimorelin", df_test)
    CortisolDuringMacimorelin$ID19 <- group_plot(ID = 19, standardtest = "CortisolDuringMacimorelin", df_test)
  }
  CortisolDuringMacimorelin$NullID <- group_plot(ID = NULL, 
                                                 standardtest = NULL, 
                                                 input_dataframe = df_test,
                                                 group_by = "IDandTest",
                                                 group_by_legend = "ID",
                                                 x_axis = "Time",
                                                 x_axis_name = "Time (min)",
                                                 y_axis = "Cortisol",
                                                 y_axis_name = "Cortisol level during Macimorelin test (nmol/L)",
                                                 appendID = FALSE, 
                                                 title = "Cortisol level during Macimorelin-test",
                                                 meancurve = TRUE,
                                                 showlines = TRUE,
                                                 linear = FALSE,
                                                 autoYlimits = FALSE, 
                                                 ymin = 100, 
                                                 ymax = 600,
                                                 cutoff = cutoff_synacthen,
                                                 customcolors = TRUE,
                                                 monocolor = FALSE,
                                                 hidelegend = TRUE,
                                                 alpha = 0.8)
  if(saveplots){saveggplot(CortisolDuringMacimorelin)}
  if(showplots){showggplot(CortisolDuringMacimorelin, "all")}
}
