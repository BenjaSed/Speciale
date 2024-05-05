source("Main tests.R")

variable_1 <- "NormalCortisol"
variable_2 <- "GH_over_cutoff" 
variable_3 <- "LinearSlopePositive" # Slope of Cortisol during Macimorelin
variable_4 <- "maci_cortisol_over_cutoff"
enablevariable_1 <- TRUE
enablevariable_2 <- TRUE
enablevariable_3 <- TRUE
enablevariable_4 <- TRUE

if(substudy){
  df_test <- df_test[df_test$ID %in% c(1, 5, 6, 9, 11, 13, 14, 16), ]
}

UseLog <- TRUE
skipnormalitytesting <- TRUE

# Making time a factor
df_test$TimeFactor <- factor(df_test$Time)

# Making sure the continuous variables are continuous, while the categorical variables are categorical.
df_test$Cortisol <- as.numeric(df_test$Cortisol)
if(UseLog) {df_test$Cortisol <- log(df_test$Cortisol)}
df_test$Time <- as.factor(df_test$Time)

# Splitting df_test into datasets based on variable TRUE/FALSE.

# Synacthen or spot-cortisol over cutoff
df_cortisol_over <- subset(df_test, df_test[[variable_1]]==TRUE)
df_cortisol_under <- subset(df_test, df_test[[variable_1]]==FALSE)

# GH over/under cutoff during Macimorelin test
df_GH_over <- subset(df_test, df_test[[variable_2]]==TRUE)
df_GH_under <- subset(df_test, df_test[[variable_2]]==FALSE)

# Linear slope of Cortisol during Macimorelin positive/negative (over/under 0)
df_cortimacilinear_over <- subset(df_test, df_test[[variable_3]]==TRUE)
df_cortimacilinear_under  <- subset(df_test, df_test[[variable_3]]==FALSE)

#Cortisol during Macimorelin over Synacthen-cutoff
df_cortimaci_over <- subset(df_test, df_test[[variable_4]]==TRUE)
df_cortimaci_under  <- subset(df_test, df_test[[variable_4]]==FALSE)

# R^2 values for linear slope:
meansd(df_cortimacilinear_over, "LinearRsquared",3)
meansd(df_cortimacilinear_under, "LinearRsquared",3)


# Enabling extra variables, and making sure these also are categorical.
if(enablevariable_1 == TRUE){df_test[[variable_1]] <- as.factor(df_test[[variable_1]])}
if(enablevariable_2 == TRUE){df_test[[variable_2]] <- as.factor(df_test[[variable_2]])}
if(enablevariable_3 == TRUE){df_test[[variable_3]] <- as.factor(df_test[[variable_3]])}
if(enablevariable_3 == TRUE){df_test[[variable_4]] <- as.factor(df_test[[variable_4]])}


#------ Tests for homoscedasticity, which is needed for ANOVA! ------

if(UseLog){df_test$Cortisol <- exp(df_test$Cortisol)} # Temporarily remove LOG again to produce graph
  
# Shapiro-Wilk test of normality!
shapirotest <- as.numeric(shapiro.test(df_test$Cortisol)[2])
shapiro <- if(shapirotest >= 0.05) {paste0("Fail to reject normality - p-value: ",round(shapirotest,6))} else {paste0("Not normally distributed! - p-value: ",round(shapirotest,6))}
shapirotest
shapiro

plot <- ggdensity(df_test$Cortisol,
                  title = "Density plot of serum Cortisol during Macimorelin test",
                  subtitle = shapiro,
                  xlab = "Cortisol (nmol/L)",ylab = "Density")
if(showplots){plot}
if(saveplots){ggsave(filename = "Density plot no LN.png", plot = plot,
                     path = save_file_location,
                     width = , 
                     height = )}

# Shapiro-Wilk test again, with LN

df_test$Cortisol <- log(df_test$Cortisol) # Re-add LOG again to produce graph


shapirotest <- as.numeric(shapiro.test(df_test$Cortisol)[2])
shapiro <- if(shapirotest >= 0.05) {paste0("Fail to reject normality - p-value: ",round(shapirotest,6))} else {paste0("Not normally distributed! - p-value: ",round(shapirotest,6))}
shapirotest
shapiro


plot <- ggdensity(df_test$Cortisol,
                  title = "Density plot of ln(Cortisol) during Macimorelin test",
                  subtitle = shapiro,
                  xlab = "ln(Cortisol)",ylab = "Density")
if(showplots){plot}
if(saveplots){ggsave(filename = "Density plot LN.png", plot = plot,
                     path = save_file_location,
                     width = , 
                     height = )}

if(!UseLog){df_test$Cortisol <- exp(df_test$Cortisol)} # Remove LN again if not supposed to use LN


if(skipnormalitytesting){
# And for double one-way ANOVA
shapirotest <- as.numeric(shapiro.test(df_cortisol_over$Cortisol)[2])
shapiro <- if(shapirotest >= 0.05) {paste0("Fail to reject normality - p-value: ",round(shapirotest,6))} else {paste0("Not normally distributed! - p-value: ",round(shapirotest,6))}
print(shapirotest)
print(shapiro)

if(!(substudy & removeimmunometry)){ # Substudy does not have enough datapoints, have to skip!
shapirotest <- as.numeric(shapiro.test(df_cortisol_under$Cortisol)[2])
shapiro <- if(shapirotest >= 0.05) {paste0("Fail to reject normality - p-value: ",round(shapirotest,6))} else {paste0("Not normally distributed! - p-value: ",round(shapirotest,6))}
print(shapirotest)
print(shapiro)
}

#------ Visual plotting for normality ------
  # Density plot

if(!UseLog){
  xlab = "Cortisol (X/X)"
  title = "Density plot of Cortisol concentration during macimorelin test"
}

if(UseLog){
  xlab = "ln(Cortisol)"
  title = "Density plot of ln(Cortisol) concentration during macimorelin test"
}

  ggdensity(df_test$Cortisol,
            title = title,
            subtitle = shapiro,
            xlab = xlab,
            ylab = "Density")
  rm(xlab)
  rm(title)
  
  # Q-Q plot
  ggqqplot(df_test$Cortisol)
  
  
  # par is useful for displaying multiple plots in a grid, but needs to reverted to 1x1 grid afterwards for other plots
  par(mfrow=c(2,2))
  #plot(onewayanova)
  par(mfrow=c(1,1))
  #https://www.scribbr.com/statistics/anova-in-r/

}

#----- ANOVA -----
summary(df_test)

# Creating box plots
if(showplots){
if(UseLog){ylab <- "ln(Cortisol during macimorelin test)"}
if(!UseLog){ylab <- "Cortisol-level during macimorelin test (nmol/L)"}

boxplot(df_test$Cortisol~df_test[[variable_1]], xlab = paste0("Is ",variable_1,"?"), ylab = ylab)
boxplot(df_test$Cortisol~df_test[[variable_2]], xlab = paste0("Is ",variable_2,"?"), ylab = ylab)
boxplot(df_test$Cortisol~df_test[[variable_3]], xlab = paste0("Is ",variable_3,"?"), ylab = ylab)
boxplot(df_test$Cortisol~df_test[[variable_4]], xlab = paste0("Is ",variable_4,"?"), ylab = ylab)
rm(ylab)
}

# Building the ANOVA models
#------ AIC and Tukey to determine best ANOVA model ------
# akaike information criterion - Lower is better - Compares models, but does not say if any are good models. Needs validation of absolute quality of model
# https://en.wikipedia.org/wiki/Akaike_information_criterion


if(TRUE){
# df_test
if(!(substudy & removeimmunometry)){ # Substudy does not have enough datapoints, have to skip!
aic <- anova_models(df_test,variable_1,variable_2)
aictab(aic$models, modnames = aic$model_names)
}}
# Additive effect of synacthen over cutoff was significant within 0.01


if(TRUE){
# df_synacthen over
aic <- anova_models(df_cortisol_over,variable_2)
aictab(aic$models, modnames = aic$model_names)
# df_synacthen under
if(!(substudy & removeLCMS)){ # Substudy does not have enough datapoints, have to skip!
if(!(substudy & removeimmunometry)){ # Substudy does not have enough datapoints, have to skip!
aic <- anova_models(df_cortisol_under,variable_2)
aictab(aic$models, modnames = aic$model_names)
}}}
# One-way ANOVA best to explain both over and under cutoff, but not significant at all

if(TRUE){
# df_GH over
aic <- anova_models(df_GH_over,variable_1)
aictab(aic$models, modnames = aic$model_names)
# df_GH under
aic <- anova_models(df_GH_under,variable_1)
aictab(aic$models, modnames = aic$model_names)
}
# Additive effect of synacthen cutoff was best, but not significant for 0.05

if(TRUE){
#Variable 2 (GH over cutoff) cannot be used, all in one group!
# df_cortisol during macimorelin linear over
aic <- anova_models(df_cortimacilinear_over,variable_1)
aictab(aic$models, modnames = aic$model_names)
# df_cortisol during macimorelin linear under
aic <- anova_models(df_cortimacilinear_under,variable_1)
aictab(aic$models, modnames = aic$model_names)
# Additive effect of synacthen cutoff was best. For negative slope, significant for 0.05, but not positive slope.
}

if(TRUE){
#Variable 2 (GH over cutoff) cannot be used, all in one group!
# df_cortisol during macimorelin over
aic <- anova_models(df_cortimaci_over,variable_1)
aictab(aic$models, modnames = aic$model_names)
# df_cortisol during macimorelin under
aic <- anova_models(df_cortimaci_under,variable_1)
aictab(aic$models, modnames = aic$model_names)
# One-way best for over cutoff, synacthen cutoff best for under, significant for 0.01.
}

#Should choose the best model here!
if(!(substudy & removeimmunometry)){ # Substudy does not have enough datapoints, have to skip!
twowayanova <- aov(Cortisol ~ Time + df_test[[variable_1]], data = df_test)
summary(twowayanova)
TukeyHSD(twowayanova)
}
# Tukey Honestly Significant Difference test indicates no statistically significant difference between any time, 
# but a significant difference between whether or not Synacthen is over/under cutoff

# Compact letter displays
# Tukeycld <- multcompLetters4(twowayanova,TukeyHSD(twowayanova))
# print(Tukeycld)
# However, here all factors get the same letter (a), except for over/under cutoff.
# Thereby no reason to add this to the graph.

# Renaming the factors
df_test$Cortisol_cutoffdata <- factor(df_test[[variable_1]], 
                                        levels = c(FALSE, TRUE), 
                                        labels = c("Not sufficient", "Sufficient"))

# Plotting this
plot <- ggplot(df_test, aes(x = Cortisol_cutoffdata, y = exp(Cortisol), group = TimeFactor)) +
  theme_classic2() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(panel.spacing.x = unit(0, "lines"),
        strip.background = element_rect(color=NULL, fill=NULL, size=0, linetype="blank"),
        strip.placement = "outside") +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(cex = 1.5, pch = 1.0, position = position_jitter(w = 0.2, h = 0),aes(color = Cortisol_cutoffdata)) +
  facet_grid(~ TimeFactor,switch = "x") +
  scale_x_discrete(expand = c(2, 0)) +
  scale_y_continuous("Cortisol (nmol/L)", breaks = seq(0,600,100), limits = c(100,600)) +
  labs(color = "HPA function") +
  labs(
    title = "Cortisol during Macimorelin test",
    subtitle = "Stratified by assessed HPA axis function",
    x = "Macimorelin test time (minutes)"
    )
# NOTE TO SELF: SE non-overlapping is not indicative of statistical significance!
# Compact letter displays have been omitted, as there are no statsitically significant differences
# except for over/under synacthen cutoff
if(showplots){plot(plot)}
if(saveplots){
  ggsave(plot, filename = "ANOVA plotted data Cortisol.png", 
         width = 10, height = 5, units = "in", dpi = 300, 
         path = save_file_location)
  }
# No significant difference between any combination of times.
# However, there is a significant difference between over/under cutoff for Synacthen.


#---- Also for GH over cutoff: ----
#Should choose the best model here!
var2twowayanova <- aov(Cortisol ~ Time + df_test[[variable_2]], data = df_test)
summary(var2twowayanova)
TukeyHSD(var2twowayanova)
# Tukey Honestly Significant Difference test indicates no statistically significant difference between any time, 
# nor whether or not Synacthen is over/under cutoff

# Renaming the factors
df_test$GH_cutoffdata <- factor(df_test[[variable_2]], 
                                       levels = c(FALSE, TRUE), 
                                       labels = c("GH under cut-off", "GH over cut-off"))

# Plotting this
plot <- ggplot(df_test, aes(x = GH_cutoffdata, y = exp(Cortisol), group = TimeFactor)) +
  theme_classic2() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(panel.spacing.x = unit(0, "lines"),
        strip.background = element_rect(color=NULL, fill=NULL, size=0, linetype="blank"),
        strip.placement = "outside") +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(cex = 1.5, pch = 1.0, position = position_jitter(w = 0.2, h = 0),aes(color = GH_cutoffdata)) +
  facet_grid(~ TimeFactor,switch = "x") +
  scale_x_discrete(expand = c(2, 0)) +
  scale_y_continuous("Cortisol (nmol/L)", breaks = seq(0,600,100), limits = c(100,600)) +
  labs(color = "GH response") +
  labs(
    title = "Cortisol during Macimorelin test",
    subtitle = "Stratified by Macimorelin test result",
    x = "Macimorelin test time (minutes)"
  )
# NOTE TO SELF: SE non-overlapping is not indicative of statistical significance!
# Compact letter displays have been omitted, as there are no statsitically significant differences
# except for over/under synacthen cutoff
if(showplots){plot(plot)}
if(saveplots){
  ggsave(plot, filename = "ANOVA plotted data GH.png", 
         width = 10, height = 5, units = "in", dpi = 300, 
         path = save_file_location)
}
# No significant difference between any combination of times.
# However, there is a significant difference between over/under cutoff for Synacthen.

#---- Also for slope test model: ----
var3twowayanova <- aov(Cortisol ~ Time + df_test[[variable_3]], data = df_test)
summary(var3twowayanova)
TukeyHSD(var3twowayanova)
# Tukey Honestly Significant Difference test indicates statistically significant difference for
# the slope, within 0,05

# Renaming the factors
df_test$Linear_slopedata <- factor(df_test[[variable_3]], 
                                   levels = c(FALSE, TRUE), 
                                   labels = c("Negative", "Positive"))

# Plotting this
plot <- ggplot(df_test, aes(x = Linear_slopedata, y = exp(Cortisol), group = TimeFactor)) +
  theme_classic2() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(panel.spacing.x = unit(0, "lines"),
        strip.background = element_rect(color=NULL, fill=NULL, size=0, linetype="blank"),
        strip.placement = "outside") +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(cex = 1.5, pch = 1.0, position = position_jitter(w = 0.2, h = 0),aes(color = Linear_slopedata)) +
  facet_grid(~ TimeFactor,switch = "x") +
  scale_x_discrete(expand = c(2, 0)) +
  scale_y_continuous("Cortisol (nmol/L)", breaks = seq(0,600,100), limits = c(100,600)) +
  labs(color = "Cortisol slope") +
  labs(
    title = "Cortisol during Macimorelin test",
    subtitle = "Stratified by linear regression slope of Cortisol during Macimorelin",
    x = "Macimorelin test time (minutes)"
  )
# NOTE TO SELF: SE non-overlapping is not indicative of statistical significance!
# Compact letter displays have been omitted, as there are no statsitically significant differences
# except for over/under synacthen cutoff
if(showplots){plot(plot)}
if(saveplots){
  ggsave(plot, filename = "ANOVA plotted data Cortisol slope.png", 
         width = 10, height = 5, units = "in", dpi = 300, 
         path = save_file_location)
}
# No significant difference between any combination of times.


#---- Also for 1-way ANOVA: ----
#Everything combined ANOVA
onewayanova <- aov(Cortisol ~ Time, data = df_test)
summary(onewayanova)
TukeyHSD(onewayanova)

# Plotting this
plot <- ggplot(df_test, aes(x = Time, y = exp(Cortisol))) +
  theme_classic2() +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(cex = 1.5, pch = 1.0, position = position_jitter(w = 0.1, h = 0), color = "#3cb44b") +
  scale_y_continuous("Cortisol (nmol/L)", breaks = seq(0,600,100), limits = c(100,600)) +
  labs(
    title = "Cortisol during Macimorelin test",
    subtitle = "No stratification",
    x = "Macimorelin test time (minutes)")
if(showplots){plot(plot)}
if(saveplots){
  ggsave(plot, filename = "1 way ANOVA combined.png", 
         width = 10, height = 5, units = "in", dpi = 300, 
         path = save_file_location)}

# One way ANOVA for HPA normal group
onewayanovaOverCutoff <- aov(Cortisol ~ Time, data = df_cortisol_over)
summary(onewayanovaOverCutoff)
TukeyHSD(onewayanovaOverCutoff)

# Plotting this
plot <- ggplot(df_cortisol_over, aes(x = Time, y = exp(Cortisol))) +
  theme_classic2() +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(cex = 1.5, pch = 1.0, position = position_jitter(w = 0.1, h = 0), color = "#3cb44b") +
  scale_y_continuous("Cortisol (nmol/L)", breaks = seq(0,600,100), limits = c(100,600)) +
  labs(
    title = "Cortisol during Macimorelin test",
    subtitle = "Subgroup with assessed normal HPA axis function",
    x = "Macimorelin test time (minutes)")
if(showplots){plot(plot)}
if(saveplots){
  ggsave(plot, filename = "1 way ANOVA over.png", 
         width = 10, height = 5, units = "in", dpi = 300, 
         path = save_file_location)}

# One way ANOVA for HPA under cutoff group
if(!(substudy & removeimmunometry)){ # Substudy does not have enough datapoints, have to skip!
onewayanovaUnderCutoff <- aov(Cortisol ~ Time, data = df_cortisol_under)
summary(onewayanovaUnderCutoff)
TukeyHSD(onewayanovaUnderCutoff)
}

# Plotting this
plot <- ggplot(df_cortisol_under, aes(x = Time, y = exp(Cortisol))) +
  theme_classic2() +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(cex = 1.5, pch = 1.0, position = position_jitter(w = 0.1, h = 0), color = "#3cb44b") +
  scale_y_continuous("Cortisol (nmol/L)", breaks = seq(0,600,100), limits = c(100,600)) +
  labs(
    title = "Cortisol during Macimorelin test",
    subtitle = "Subgroup with assessed not sufficient HPA axis function",
    x = "Macimorelin test time (minutes)")
if(showplots){plot(plot)}
if(saveplots){
  ggsave(plot, filename = "1 way ANOVA under.png", 
         width = 10, height = 5, units = "in", dpi = 300, 
         path = save_file_location)}






calculateSD(df_test)

calculateSD(df_cortisol_over)
calculateSD(df_cortisol_under)

calculateSD(df_cortimacilinear_over)
calculateSD(df_cortimacilinear_under)

calculateSD(df_GH_over)
calculateSD(df_GH_under)

var3twowayanovaOverCutoff <- aov(Cortisol ~ Time, data = df_cortimacilinear_over)
var3twowayanovaUnderCutoff <- aov(Cortisol ~ Time, data = df_cortimacilinear_under)
var2twowayanovaOverCutoff <- aov(Cortisol ~ Time, data = df_GH_over)
var2twowayanovaUnderCutoff <- aov(Cortisol ~ Time, data = df_GH_under)

summary(onewayanova)

summary(twowayanova)
summary(onewayanovaOverCutoff)
summary(onewayanovaUnderCutoff)

summary(var3twowayanova)
summary(var3twowayanovaOverCutoff)
summary(var3twowayanovaUnderCutoff)

summary(var2twowayanova)
summary(var2twowayanovaOverCutoff)
summary(var2twowayanovaUnderCutoff)



#-------
#Old version:
if(FALSE){
  # Plotting this
  plot <- ggplot(df_test, aes(x = Time, y = Cortisol, group = Linear_slopedata)) +
    theme_classic2() +
    stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
    stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
    geom_point(cex = 1.5, pch = 1.0, position = position_jitter(w = 0.1, h = 0)) +
    facet_wrap(~ Linear_slopedata) +
    scale_y_continuous("ln(Cortisol)", breaks = seq(0,7,1), limits = c(4.5,6.5)) +
    labs(
      title = "Natural logarithm of Cortisol levels during Macimorelin test",
      subtitle = "Stratified by linear regression slope of Cortisol during Macimorelin",
      x = "Macimorelin test time (minutes)"
    )
  # NOTE TO SELF: SE non-overlapping is not indicative of statistical significance!
  # Compact letter displays have been omitted, as there are no statsitically significant differences
  # except for over/under synacthen cutoff
  if(showplots){plot(plot)}
  if(saveplots){
    ggsave(plot, filename = "ANOVA plotted data Cortisol slope.png", 
           width = 10, height = 5, units = "in", dpi = 300, 
           path = save_file_location)
  }
}
