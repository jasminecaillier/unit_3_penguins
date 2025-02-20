#2025-02-18 Class notes dplyr and tidyverse

#####################
#Unit 3 TIDYVERSE
#####################

#tidyverse is a series of packages and today we focus on dplyr

install.packages("tidyverse")
library(tidyverse)
tidyverse_packages()

#Penguin data set: data from 3 species of penguins from 2007-2009
install.packages("palmerpenguins")
library(palmerpenguins)
view(penguins)
head(penguins)
summary(penguins)
glimpse(penguins) # flips columns not a good way to look at this
dim(penguins)


#####dplyr!!!!!!

#filter my data to only show me a specific species
gentoo= filter(penguins, species== "Gentoo")
summary(gentoo)
#now filter out a specific species of penguins and only females
gentoo_ladies= filter(penguins, species== "Gentoo", sex== "female")
summary(gentoo_ladies)
# another way to filter out the ladies because we already filtered specific penguin we want filter(gentoo, sex=="female")

#####  THE PIPE %>%

#what we are doing is taking x applying one function to it %>% then apply another function to it 

#does same thing as we did above, filter what I wanted from my data
gentoo = penguins %>% 
  filter(species=="Gentoo") %>%
  filter(sex== "female")
summary(gentoo)

#want females and second row-  body mass mean and save in column mean mass grams 
female_mass= penguins%>%
  filter(sex=="female")%>%
  summarize(mean_mass_g= mean(body_mass_g))
female_mass

female_mass= mean(penguins$body_mass_g[which(penguins$sex== "female")]) #another way in base r to find body mass, but this is less readable 

#told it to use penguin data, group by penguin species adn give me the mean mass of each species 
species_mean_mass_g= penguins %>%
  group_by(species) %>%
  summarize(mean_mass_g= mean(body_mass_g, na.rm=TRUE)) %>% #because there are NA so to get rid of them
  print()

species_sex_and_mean_mass_g= penguins %>%
  filter(!is.na(sex)) %>%  #takes away NAs for sex 
  group_by(species, sex) %>%
  summarize(mean_mass_g= mean(body_mass_g, na.rm=TRUE)) %>% 
  print()

species_sex_island_and_mean_mass_g= penguins %>%
  filter(!is.na(sex)) %>%  #takes away NAs for sex 
  group_by(species, sex, island) %>%
  summarize(mean_mass_g= mean(body_mass_g, na.rm=TRUE), count=n()) %>%  #n() provides the count of sample size
  print()
write.csv(file = "data/processed/penguin_mass_sex_island_table.csv", x=species_sex_island_and_mean_mass_g)
#can do same thing with write_csv()

temporary= read.csv("data/processed/penguin_mass_sex_island_table.csv")
temporary


#mutate() to convert body mass units and created a new column now in pounds instead of grams

penguins_for_america= penguins %>%
  mutate(body_mass_lbs= body_mass_g * 0.0022) %>% #conversion is 0.0022 pounds per grams
  print()

#distinct() what is every distinct type of data

penguin_island= penguins %>%
  distinct(island)%>%
  print()

#select() only selects what you want to include 
peguins_select= penguins %>%
  select(species, sex) %>%
  print()

#now use select() for columns want tp exclude
peguins_no_body_mass= penguins %>%
  select(-body_mass_g) %>%
  print()

#arrange() put things in order from least to greatest, we sorted first by body mass and then bill depth 
peguins_sorted= penguins %>%
  arrange(desc(body_mass_g), desc(bill_depth_mm)) %>%  #desc flips now can see from greatest to larger
  print()

#####Exercise 1.3 mutate, filter, and summarize

penguins_exercise= penguins %>%
  filter(species== "Adelie", island %in% c("Dream", "Biscoe")) %>%
mutate(bill_length_inch= (bill_length_mm*0.039)) %>%
  summarize(bill_length_inch_sd= sd(bill_length_mm, na.rm = TRUE),
            bill_length_inch_mean= mean(bill_length_mm, na.rm= TRUE)) 
penguins_exercise  

