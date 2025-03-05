# 2025-02-25

######################
#T-test/Correlations
######################

install.packages("rstatix")

library(tidyverse)
library(palmerpenguins)
library(rstatix)
library(knitr)

#is gentoo body mass 

gentoo= penguins %>%
  filter(species== "Gentoo")

ggplot(data = penguins)+
  geom_histogram(aes(x=body_mass_g, fill = species))

#QQplot
#shows normality is your data falling on the 1:1 line
ggplot() + 
  stat_qq(aes(sample= body_mass_g), data = gentoo)
length(gentoo$body_mass_g)
dim(gentoo %>% filter(!is.na(body_mass_g))) #show me parameters of how many body masses we have excluding the na's

lit_body_mass_g= 5500 #EOL

t.test(gentoo$body_mass_g, mu = lit_body_mass_g)
#this shows us that the chance of getting our exact mass sample is very unlikely, accept alternative hypothesis meaning that it is different

my_ttest= t.test(gentoo$body_mass_g, mu = lit_body_mass_g)
summary(my_ttest)
my_ttest$p.value
my_ttest$statistic

other_ttest_way= gentoo %>% 
  t_test(body_mass_g ~ 1, mu = lit_body_mass_g) # ~1 means that I want a one sample ttest
kable(other_ttest_way)

##Independent sample t test (:
#comparing the means of two independent groups one group of penguins vs the other group of penguins body mass

#first can filter to the data you only want to use
data_for_ttest= penguins %>%
  filter(species %in% c("Gentoo", "Adelie"),
!is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels()    #drops the zeros so they dont appear in data

head(data_for_ttest)
summary(data_for_ttest)


data_for_ttest %>%
  group_by(species) %>%
  summarize(avg= mean(body_mass_g), std= sd(body_mass_g))
#adelie have smaller mean body mass than gentoo

ggplot(data= data_for_ttest) +
  geom_histogram(aes(x=body_mass_g, fill = species)) +
  facet_wrap(~species)

ggplot(data= data_for_ttest) +
 stat_qq(aes(sample=body_mass_g, color = species)) +
  facet_wrap(~species, scales="free") #y axis is now based on the scale of the type of penguins body mass

#check if variance is similar, you don't want this test significant, our p value is large  meaning we accept null meaning that the body masses of our two spenguins are relativley similar
data_for_ttest %>%
  levene_test(body_mass_g ~ species)

body_masses_adelie_gentoo= t.test(data_for_ttest$body_mass_g ~ data_for_ttest$species, var.equal = TRUE)
  #there is a significant difference between gentoo and adelie penguins
#gentoo body masses are significantly heavier than the adelie body masses

summary(body_masses_adelie_gentoo)
head(body_masses_adelie_gentoo)

data_for_ttest %>%
  t_test(body_mass_g ~ species)

#paired sample t test: have a group apply a treatment to a group and measure them, then apply another treatment and measure them again and see if there is a difference 


##############
#Correlations
##############

#is bill length correlated with bill depth

glimpse(gentoo)

#correlation bill depth vs bill length just gentoo

ggplot( data= gentoo) +
  geom_point(aes(x= bill_length_mm, y = bill_depth_mm))

ggplot( data= gentoo) +
  stat_qq(aes(sample = bill_length_mm))
ggplot( data= gentoo) +
  stat_qq(aes(sample = bill_depth_mm))

ggplot( data= gentoo) +
  geom_histogram(aes(x= bill_length_mm))
ggplot( data= gentoo) +
  geom_histogram(aes(x= bill_depth_mm))

cor(x= gentoo$bill_depth_mm, y= gentoo$bill_length_mm, use= "complete.obs") #need to remove NAs and use use=complet.obs
# this means that they are correlated with one another 
#if cor=0 that means they are not correlates and if cor=1 that means they are perfectly correlated if cor= -1 that mean your two variables are inversely correlated

correlation_test = cor(x= gentoo$bill_depth_mm, y= gentoo$bill_length_mm, use= "complete.obs")

r_correlationtest =cor.test(x=gentoo$bill_depth_mm, y= gentoo$bill_length_mm, use= "complete.obs")
class(r_correlationtest)
summary(r_correlationtest)

#pearson correlation
gentoo %>%
  cor_test(bill_length_mm, bill_depth_mm)


#running lots of correlations at once::: correlation matrix 
head(gentoo)
gentoo[, 3:6] #subset of the data that is numeric
cor(gentoo[, 3:6], use = "complete.obs") #check correlation of all numeric columns 3-6


install.packages("GGally")
library(GGally)
#gives density plots of each single variable and shows how distributed they are
gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()

penguins %>%
  select(species,body_mass_g, ends_with("_mm")) %>%  #a way to select all data in stead of writing all the variables i want that end with _mm
  GGally::ggpairs(aes(color= species))

##Correlation does not imply causation
#if two variables have high correlation, it does not mean that one variables causes the value of the other variable to increase

