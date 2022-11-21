#install.packages('tidyverse')

#Working with tidyverse package ggplot!!
library(tidyverse)

surveys <- read_csv("data/portal_data_joined.csv") %>%
  filter(!is.na(weight) & !is.na(hindfoot_length))

## General format:
## ggplot uses the + sign
## ggplot(data = <INPUT_DATA>, mapping = aes(<INPUT_MAPPINGS>)) +
##  <GEOM_FUNCTION>() #THE SHAPE OF THE DATA

# blank canvas
ggplot(data = surveys) # no information on the plot yet 

# Examples of geoms
# geom_point() for scatter plots (continuous * continuous variables)
# geom_boxplot() for boxplots (categorical * continuous data)
# geom_line() for trend lines

# add aes for 'mapping coordinates'
ggplot(data = surveys, mapping = aes(x = weight, y = hindfoot_length))

# add layer for geom
ggplot(data = surveys, 
       mapping = aes(x = weight, y = hindfoot_length)) + 
       geom_point()

# ggplots can be assigned as objects 
base_plot <- ggplot(data = surveys, 
       mapping = aes(x = weight, y = hindfoot_length)) #+ 
  #geom_point()

#and then basically the ggplot file can be used replacing the 
# whole lining code, modifications can be written on this ggplot
#object without writing all the previous code!

#Adding plot elements: transparency = alpha, color = color, 
#infill = fill
base_plot + 
  geom_point(alpha = .5) #default is 1, 0 is completely transparent


base_plot + 
  geom_point(alpha = .5, color = "tomato") #hex codes for colors

base_plot + 
  geom_point(alpha = .5, fill = "tomato") #does nothing with this geom

#color based on the data itself
base_plot + 
  geom_point(mapping = aes(color = species_id))
#IMPORTANT!! TO INCLUDE INFO ABOUT DATA, THIS MUST BE INSIDE 
# the aesthetic mapping, like in the example, otherwise, has no
#effect. 

#geom_boxplot cat * cont
ggplot(data = surveys, mapping = aes(x = species_id, #categorical
y = weight )) + #continuous data (numeric) 
  geom_boxplot(fill = "tomato")  + #if instead of fill we use color =,then the whole lines aand dots will turn to that color
  geom_point(alpha = 0.1)

#or opposite, point first, then boxplot
ggplot(data = surveys, mapping = aes(x = species_id, #categorical
                                     y = weight )) + #continuous data (numeric) 
   geom_point(alpha = 0.1) + 
   geom_boxplot(fill = "tomato")
 

## Lecture video #2
surveys_complete <- read_csv("data/portal_data_joined.csv") %>%
  filter(complete.cases(.)) #this filter removes all NAs

# Time series in ggplot
yearly_counts <- surveys_complete %>%
  count(year, species_id)
#This tell us, the year, the species and how many times that 
#species appeared that year (or was counted)
yearly_counts

#shows the same as mentioned above in a plot
ggplot(data = yearly_counts) + 
  geom_point(mapping = aes(x = year, y = n))

#same but in a path 
ggplot(data = yearly_counts) + 
  geom_path(mapping = aes(x = year, y = n))

#Corrected with group function
ggplot(data = yearly_counts) + 
  geom_path(mapping = aes(x = year, y = n, group = species_id))

#add color
ggplot(data = yearly_counts, mapping = aes(x = year, 
                                        y = n,
                                        color = species_id)) 
# add a point for every year, this cna be achieved by combining 
#the mapping portion in the same aprenthesis of data, and then 
#adding the path + point after that, do not forget to include the
#parenthesis, otherwise you will get an error!
ggplot(data = yearly_counts, mapping = aes(x = year, 
                                           y = n,
                                           color = species_id)) + 
  geom_path() + geom_point()

## facetting (with group by species previous plot)
ggplot(data = yearly_counts) + 
  geom_path(mapping = aes(x = year, y = n)) +
facet_wrap(~ species_id, scales = "free_y") #this creates individual plots for each 
#species (in this case, as well as the counts) ; the ~ only represents
#the sides of a formula x ~ y, in this case, we leave the 
#left side empty.
# This kind of formula allows to create two-way grips 

#different wrap allineations
ggplot(data = yearly_counts) + 
  geom_path(mapping = aes(x = year, y = n)) +
  facet_wrap(~ species_id, cols = 4)

#facet_grid (for multi variables)
# ggplot(data = yearly_counts) + 
#   geom_path(mapping = aes(x = year, y = n)) +
#   facet_grid(~ species_id)


# THEMES (optional)
ggplot(data = yearly_counts, mapping = aes(x = year, 
  y = n,
  color = species_id)) + 
  geom_path() + geom_point() + theme_bw() #black&white

#it is possible to change things withing the theme function 
ggplot(data = yearly_counts, mapping = aes(x = year, 
                                        y = n,
                                        color = species_id)) + 
  geom_path() +
  geom_point() +
  theme_bw() + 
  theme(text = element_text(size = 20))

library(ggthemes) # more professional
ggplot(data = yearly_counts, mapping = aes(x = year, 
                                         y = n,
                                         color = species_id)) + 
  geom_path() +
  geom_point() +
  theme_stata() #theme_economist <- nice ones, you liked stata























