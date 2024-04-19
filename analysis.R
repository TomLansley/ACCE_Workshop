## Set up ----
library(ggplot2)
library(dplyr)
library(readr)
library(here)

#Load data
individual <- read_csv(
  here("data", "individual.csv")) %>% 
  select(stem_diameter, height, growth_form)

#Subset data
analysis_df <- individual %>% 
  filter(complete.cases(.), #only complete data (no NAs) for all the data
         growth_form != "liana")  #remove the liana rows

#Order growth form levels
gf_levels <- table(analysis_df$growth_form) %>% 
  sort() %>% #sorts default lowest to highest
  names() #just extract the names

analysis_df <- analysis_df %>% 
  mutate(growth_form = factor(growth_form, levels = gf_levels)) #make factor with specified levels

#Plots ----
#Figure 1: Bar plot of growth forms
analysis_df %>% 
  ggplot(aes(y = growth_form, fill = growth_form, colour = growth_form))+
  geom_bar(alpha = 0.5, show.legend = FALSE)

#Figure 2: Violin/box plot
#pivot data into a different format
analysis_df %>% 
  tidyr::pivot_longer(
    cols = c(stem_diameter, height),
    names_to = "var",
    values_to = "value"
  ) %>% #now pipe through to plot
  ggplot(aes(x = log(value), y = growth_form, 
             colour = growth_form, fill = growth_form))+
  geom_violin(alpha = 0.5, trim = TRUE, show.legend = FALSE) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE)+
  facet_grid(~var) #one plot for each variable type

#Fit overall linear model ----

lm_overall <- lm(log(stem_diameter) ~ log(height), data = analysis_df)

#extract useful information from models using broom package
lm_overall %>% 
  broom::glance()
lm_overall %>% 
  broom::tidy()

#plot representation of linear model
analysis_df %>% 
  ggplot(aes(x = log(height), y = log(stem_diameter)))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm")+
  xlab("Log of height (m)") +
  ylab("Log of stem diameter (cm)")+
  theme_linedraw()

#Fit linear model with a growth form interaction
lm_growth <- lm(log(stem_diameter) ~ log(height) * growth_form, data = analysis_df)

lm_growth %>% 
  broom::glance()
lm_growth %>% 
  broom::tidy()

analysis_df %>% 
  ggplot(aes(x = log(height), y = log(stem_diameter),
             colour = growth_form))+
  geom_point(alpha = 0.1)+
  geom_smooth(method = "lm")+
  labs(x = "Log of height (m)", y = "Log of stem diameter (cm)",
       colour = "Growth form")+
  theme_linedraw()

