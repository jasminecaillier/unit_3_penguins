# 2025-02-20 ggplot

library(tidyverse)
library(palmerpenguins)

find("filter")

gentoo = penguins %>%
  dplyr::filter(species== "Gentoo") %>%  #from dpylr package use filter function
  dplyr::select(-body_mass_g) %>%  #remove bosy mass from data frame
  print()

###########
#GGPLOT- Scatterplot
###########
head(penguins)

#putting data first specifies use this data for every line of code I write
#aes() is just aesthetics, and it maps aesthetics of my plot to a data set
#aes is a function to relate to your data set so if I want color species I need to add within parantheses of aes, if I just want dots differnt color throw it outside 
#warning two rows missing because we had NAs
ggplot(data = penguins)+
  geom_point(aes(x=flipper_length_mm, y= body_mass_g), color= "red")

ggplot(data = penguins)+
  geom_point(aes(x=flipper_length_mm, y= body_mass_g, color= species)) # can assign color to my factors so here each penguin is a different color 

penguins_no_nas = penguins %>%
  filter(!is.na(body_mass_g)) #can make a plot with this and makes no warning now for above plots 

ggplot(data = penguins_no_nas)+
  geom_point(aes(x=flipper_length_mm, y= body_mass_g, color= species, shape= sex)) +
  xlab("Flipper Length (mm)")+
  ylab("Body Mass (g)")+
  ggtitle("Penguins are cute (:")+
  theme_bw()

penguins_no_nas = penguins %>%
  filter(!is.na(sex)) #now on graph na is removed from legend 
#add bill length
ggplot(data = penguins_no_nas)+
  geom_point(aes(x=flipper_length_mm, y= body_mass_g, color= bill_length_mm, shape= sex)) +
  xlab("Flipper Length (mm)")+
  ylab("Body Mass (g)")+
  ggtitle("Penguins are cute (:")+
  theme_bw()
#geom_smooth= shows trends in data ie increasing or decreasing
ggplot(data = penguins_no_nas)+
  geom_point(aes(x=flipper_length_mm, y= body_mass_g, color= species, shape= sex)) +
  geom_smooth(aes(x=flipper_length_mm, y= body_mass_g))+
  xlab("Flipper Length (mm)")+
  ylab("Body Mass (g)")+
  ggtitle("Penguins are cute (:")+
  theme_bw()
###EXERCISE 2.1

####LINE PLOTS
#is sampling effort the same across all years?? (ts= timeseries)
#how many penguins are counted each year? is it the same each year?

penguin_ts = penguins %>% 
  group_by(species, year) %>%
  summarize(count=n())
penguin_ts

#saw that sampled less chinstrap penguins 
ggplot(data = penguin_ts) +
  geom_line(aes(x= year, y= count, color= species)) + # since i put color as species it drew 3 separate lines for my penguins and not one continuous line
  theme_classic()

#####HISTOGRAMS- 1 dimensional

#this shows us two modes very big penguins and some little
# fill= just means fill my polygon with a color dont just outline it (which color= does)
ggplot(data = penguins)+
  geom_histogram(aes(x= flipper_length_mm, fill= species), binwidth = 2) # binwidth=2 means I want each bar to equal 2 bars of data

#making seperate and not stacked bars
#scale fill manual and values is just that I want my colors to be a certain color 
#can use colors() and provdes you every color you can use for graph
ggplot(data = penguins)+
  geom_histogram(aes(x= flipper_length_mm, fill= species), 
                 binwidth = 2, position = "identity", alpha= 0.7) + # alpha is transparency so that you can see if any adelie behind chinstrap
          scale_fill_manual(values = c("springgreen2", "darkorchid3", "dodgerblue1"))

##can use national center of ecology NCEAS UCSB to find colors 
penguins %>% distinct(species) #see order of variables coloring 

######BOXPLOT
#boxplot (box is interquartile range, line in middle is median, and the vertical lines are extremes, and points on top of line is outlier)
#define what each thing in boxplot means in figure caption
#DO NOT USE SIMPLE BOXPLOT BELOW FOR HW OR PRESENTATION
ggplot(data = penguins) +
  geom_boxplot(aes(x= species, y= flipper_length_mm))

#can use this but hides sample size 
ggplot(data = penguins) +
  geom_boxplot(aes(x= species, y= flipper_length_mm))+
  geom_point(aes(x=species, y = flipper_length_mm))

#BEST BOX PLOT
ggplot(data = penguins) +
  geom_boxplot(aes(x= species, y= flipper_length_mm))+
  geom_jitter(aes(x=species, y = flipper_length_mm, color= species), width = 0.2) #width made the points closer to middle of box

####BARCHARTS

ggplot(data= penguins) +
  geom_bar(aes(x= sex, fill= species))
#facet wrap (~species)make me a different panel for each category (so each different species of penguins)
#do not forget ~
ggplot(data= penguins) +
  geom_bar(aes(x= sex, fill= species)) +
  facet_wrap(~species, nrow=3)  #nrow changes how many panels per each row 
  
ggplot(data= penguins) +
  geom_bar(aes(x= island, fill= species)) +
  facet_wrap(~species, nrow=3) +
  coord_flip()

####Saving plot
#if want pdf change device to pdf and end of figure name
#save as png because it doesn't lose resolution when you move it across documents
#always make save line under plot you want 
ggsave(filename = "figures/penguins_islands_species.png", 
       device = "png", units="in", width=5, height= 7, dpi= 300)


##EXERCISE 2.2
ggplot(data = penguins)+
  geom_point(aes(x= bill_length_mm, y= bill_depth_mm, color= sex)) +
  facet_wrap( ~species, scales= "free") +
  theme_minimal()
#scale= free means chnages y axis scales so it meets each penguin 