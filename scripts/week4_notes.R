#library(tydiverse)
#tidyr
#dplyr

# Headers ----

surveys <- read_csv(file = 'data/portal_data_joined.csv')
str(surveys)
head(surveys)

# Select ---- 
#tydiverse methods of indexing 
select(surveys) #no column selected, 

select(.data = surveys, plot_id, species_id, weight)
#or selecting with head + select
head(select(surveys, plot_id, species_id, weight))

# Data filter ----
filter(surveys, year == 1995) #select only 1995
filter(survyeys, year != 1995) #seleects all not 1995

# combine selection and filter 
# Pipes ----
# >#>

surveys2 <- select(surveys, plot_id, species_id, weight)

surveys_sml <- filter(surveys, weight <5)

# OR

surveys2_filtered <- select(surveys_sml, plot_id, species_id, weight)
head(surveys2_filtered)

#Or by nesting the functions 
# Nesting ----
surveys_filtered_2 <- select(filter(surveys, weight <5), plot_id, species_id, weight)

# to check if the data frames are identical we can use 
identical(surveys2_filtered, surveys_filtered_2) #they are

#OR PIPES
## Pipes Examples ----
# we can read %>% like "THEN"
survey_filtered_pipe <- surveys %>%  #select surveys, THEN
  filter(weight <5) %>% #FILTER all weight less than 5, THEN
  select(plot_id, species_id, weight) #Select the given columns with that weight

head(survey_filtered_pipe)

identical(surveys_filtered_2, survey_filtered_pipe) #they are

#Wrangling data part 2

# Mutate -- creating a new object/variable 
str(surveys)
#change weight of rodents from grams to kilograms 
surveys$weight/1000

#append a new column
#ctrl + shift + m = %>%
# Mutate ----
#mutate(new_column = edit_old_column)
surveys_2 <- surveys %>% 
  mutate(weigt_kg = weight/1000) # no need to specify dataframe name, is in top line

# group by and summarize ----
mean(surveys$weight)

# remove NAs within a function 
mean(surveys$weight, na.rm = T)

#summarize
surveys %>% 
  summarize(max(weight, na.rm = T))

#layer in this group by function, check the difference in weight by sex for example
surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = T), #name columns internally 
            max_weight = max(weight, na.rm = T))
#This partiular line removed NAs from weight, but not form sex, hence we end with 3 readings 

#Multiple summary -----
surveys %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight, na.rn = T))

#Cleaning this up by removing NAs, prior to grouping ----
surveys %>% 
  filter(!is.na(sex)) %>% # This will take the NAs out of the sex, whatever is not na (!is.na)
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = T)) %>% 
  arrange(mean_weight) #sort them from lower to higher means









