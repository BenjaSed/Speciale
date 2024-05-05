source("Main tests.R")

# Importing GH_over_cutoff data
IGF <- IGF %>%
  left_join(df_test[, c("ID", "IDandTest", "GH_over_cutoff", "IGFClosestDate", "DatedifferenceIGF")], 
            by = "ID", relationship = "many-to-many")
IGF <- IGF %>%
  group_by(ID, Date, IDandTest, IGF_1_NPU19829) %>%
    summarize(across(Gender:DatedifferenceIGF, ~toString(unique(na.omit(.x)))), .groups = 'drop')

IGF$IGF <- as.numeric(IGF$IGF)
IGF$AgeAtTest <- as.numeric(IGF$AgeAtTest)

IGF$DatedifferenceIGF <- as.numeric(IGF$DatedifferenceIGF)
IGF <- IGF %>%
  mutate(
    IDandTimesince = paste0(abs(DatedifferenceIGF)," days"),
    absdatediff = as.numeric(abs(DatedifferenceIGF))
    )
  
IGF$IDandTest <- factor(IGF$IDandTest)

IGF <- IGF[order(IGF$absdatediff),]

IGF$GH_cutoffdata <- factor(IGF[["GH_over_cutoff"]], 
                                levels = c(FALSE, TRUE), 
                                labels = c("GH under cut-off", "GH over cut-off"))


IGF_closest <- IGF %>%
  group_by(IDandTest) %>%
  filter(Date == IGFClosestDate) %>%
  select(everything())


IGF_closest <- IGF_closest %>%
  group_by(DatedifferenceIGF) %>%
  mutate( 
    count = as.numeric(row_number())
  ) %>%
  ungroup()

IGF_closest$spaces <- sapply(IGF_closest$count, function(x) paste0(rep(" ", x), collapse = ""))

IGF_closest <- IGF_closest %>%
  mutate(
    IDandTimesince = paste0(IDandTimesince,spaces)
  )
IGF_closest$IDandTimesince <- factor(IGF_closest$IDandTimesince, levels=unique(mixedsort(IGF_closest$IDandTimesince)))


shapes <- c(15,17)

show1sd <- FALSE # Show extra confidence interval


plot <- ggplot(environment = environment()) +
  ggtitle("IGF-1 data for males, 
plotted against reference-interval by Bidlingmaier et al.") +
  theme_classic2() +
  labs(color = "ID") +
  scale_color_manual(values = custompalette) +
  geom_line(data = IGF1Men, aes(x = Age, y = neg2SD), color = "black") +
  {if(show1sd)geom_line(data = IGF1Men, aes(x = Age, y = neg1SD), color = "gray", linetype = "dashed")} +
  geom_line(data = IGF1Men, aes(x = Age, y = SD), color = "black", linetype = "dotted") +
  {if(show1sd)geom_line(data = IGF1Men, aes(x = Age, y = pos1SD), color = "gray", linetype = "dashed")} +
  geom_line(data = IGF1Men, aes(x = Age, y = pos2SD), color = "black") +
  #geom_line(data = (subset(IGF, Gender=="Man")), aes(x = AgeAtTest, y = IGF, color = factor(ID)),size=0.4, alpha = 0.5) +
  geom_point(data = (subset(IGF, Gender=="Man")), aes(x = AgeAtTest, y = IGF, color = factor(ID)),size=1.1) +
  scale_x_continuous("Age (years)", breaks = seq(0,90,10)) +
  scale_y_continuous("Serum IGF-I (μg/L)", breaks = seq(0,600,100), limits = c(0,600))

if(showplots){plot}
if(saveplots){ggsave(filename = "IGF-I Men.png", plot = plot,
                     path = save_file_location,
                     width = , 
                     height = )}


plot <- ggplot(environment = environment()) +
  ggtitle("IGF-1 data for females, 
plotted against reference-interval by Bidlingmaier et al.") +
  theme_classic2() +
  labs(color = "ID") +
  scale_color_manual(values = custompalette) +
  geom_line(data = IGF1Women, aes(x = Age, y = neg2SD), color = "black") +
  {if(show1sd)geom_line(data = IGF1Women, aes(x = Age, y = neg1SD), color = "gray", linetype = "dashed")} +
  geom_line(data = IGF1Women, aes(x = Age, y = SD), color = "black", linetype = "dotted") +
  {if(show1sd)geom_line(data = IGF1Women, aes(x = Age, y = pos1SD), color = "gray", linetype = "dashed")} +
  geom_line(data = IGF1Women, aes(x = Age, y = pos2SD), color = "black") +
  #geom_line(data = (subset(IGF, Gender=="Woman")), aes(x = AgeAtTest, y = IGF, color = factor(ID)),size=0.4, alpha = 0.8) +
  geom_point(data = (subset(IGF, Gender=="Woman")), aes(x = AgeAtTest, y = IGF, color = factor(ID)),size = 1.1) +
  scale_x_continuous("Age (years)", breaks = seq(0,90,10)) +
  scale_y_continuous("Serum IGF-I (μg/L)", breaks = seq(0,600,100), limits = c(0,600))

if(showplots){plot}
if(saveplots){ggsave(filename = "IGF-I Women.png", plot = plot,
                      path = save_file_location,
                      width = , 
                      height = )}

#------ Grouping for Macimorelin test-----
plot <- ggplot(environment = environment()) +
  ggtitle("IGF-I data nearest to macimorelin test for male events, grouped 
by macimorelin result, plotted against male reference interval") +
  theme_classic2() +
  labs(color = "Days between
macimorelin test
and IGF-I test") +
  guides(colour = guide_legend(order = 2), shape = guide_legend(order = 1)) +
  scale_color_manual(values = custompalette) +
  scale_shape_manual(values = shapes) +
  labs(shape = "Macimorelin result") +
  geom_line(data = IGF1Men, aes(x = Age, y = neg2SD), color = "black") +
  {if(show1sd)geom_line(data = IGF1Men, aes(x = Age, y = neg1SD), color = "gray", linetype = "dashed")} +
  geom_line(data = IGF1Men, aes(x = Age, y = SD), color = "black", linetype = "dotted") +
  {if(show1sd)geom_line(data = IGF1Men, aes(x = Age, y = pos1SD), color = "gray", linetype = "dashed")} +
  geom_line(data = IGF1Men, aes(x = Age, y = pos2SD), color = "black") +
  geom_point(data = (subset(IGF_closest, Gender=="Man")), aes(x = AgeAtTest, y = IGF, color = (IDandTimesince), shape = factor(GH_cutoffdata)),size=1.9) +
  scale_x_continuous("Age (years)", breaks = seq(0,90,10)) +
  scale_y_continuous("Serum IGF-I (μg/L)", breaks = seq(0,600,100), limits = c(0,600))
if(showplots){plot}
if(saveplots){ggsave(filename = "IGF-I Men GH cutoff.png", plot = plot,
                     path = save_file_location,
                     width = 7.1, 
                     height = 5.5)}


plot <- ggplot(environment = environment()) +
  ggtitle("IGF-I data nearest to macimorelin test for female events, grouped 
by macimorelin result, plotted against female reference interval") +
  theme_classic2() +
  labs(color = "Days between
macimorelin test
and IGF-I test") +
  guides(colour = guide_legend(order = 2), shape = guide_legend(order = 1)) +
  scale_color_manual(values = custompalette) +
  scale_shape_manual(values = shapes) +
  labs(shape = "Macimorelin result") +
  geom_line(data = IGF1Women, aes(x = Age, y = neg2SD), color = "black") +
  {if(show1sd)geom_line(data = IGF1Women, aes(x = Age, y = neg1SD), color = "gray", linetype = "dashed")} +
  geom_line(data = IGF1Women, aes(x = Age, y = SD), color = "black", linetype = "dotted") +
  {if(show1sd)geom_line(data = IGF1Women, aes(x = Age, y = pos1SD), color = "gray", linetype = "dashed")} +
  geom_line(data = IGF1Women, aes(x = Age, y = pos2SD), color = "black") +
  geom_point(data = (subset(IGF_closest, Gender=="Woman")), aes(x = AgeAtTest, y = IGF, color = (IDandTimesince), shape =factor(GH_cutoffdata)),size=1.9) +
  scale_x_continuous("Age (years)", breaks = seq(0,90,10)) +
  scale_y_continuous("Serum IGF-I (μg/L)", breaks = seq(0,600,100), limits = c(0,600))
if(showplots){plot}
if(saveplots){ggsave(filename = "IGF-I Women GH cutoff.png", plot = plot,
                     path = save_file_location,
                     width = 7.1, 
                     height = 5.5)}



