#1
surveys <- read_csv(file = "data/portal_data_joined.csv")
head(surveys)

#2
''' Subset surveys using Tidyverse methods to keep rows with weight between 30 and 
60, and print out the first 6 rows.
'''
surveys %>% 
  filter(weight>30 & weight <60)
head(surveys, 6)

#3
''' Create a new tibble showing the maximum weight for each species + sex combination
and name it biggest_critters. Sort the tibble to take a look at the biggest and 
smallest species + sex combinations. HINT: it’s easier to calculate max if there 
are no NAs in the dataframe '''

biggest_critters <- surveys %>% 
  filter(!is.na(weight)) %>% #HINT, first we filter all the NAs in weight prior to any calc
  group_by(species_id, sex) %>% #we select the groups we want to create 
  summarise(max_weight = max(weight)) # to show max weight as a column name from column weight

head(biggest_critters$max_weight) # to check maximum values
biggest_critters %>% arrange(max_weight) # min to max arranged from smallest to max froma all species
biggest_critters %>% arrange(desc(max_weight)) #This displays it from max to min

#4
''' Try to figure out where the NA weights are concentrated in the data- is there 
a particular species, taxa, plot, or whatever, where there are lots of NA values? 
There isn’t necessarily a right or wrong answer here, but manipulate surveys a 
few different ways to explore this. Maybe use tally and arrange here. '''

#?tally -> lets you quickly count unique values of one OR more variables
'''df %>%  count(a,b) goes to:
df %>%  group_by(a,b) %>% 
summarise(n = n())
'''
surveys %>% 
  filter(is.na(weight)) %>% # everything that is an NA in the weight column
  group_by(species, sex, taxa) %>% 
  tally(sort = T) %>% #sort =T arranges largest numbers at the top
  arrange(desc(n))

#Harrisi species seems to have more NAs than other species, there is also a big
#concentration of NAs in the sex column, rodents are also high on the NAs 

#5
'''
Take surveys, remove the rows where weight is NA and add a column that contains 
the average weight of each species+sex combination to the full surveys dataframe. 
Then get rid of all the columns except for species, sex, weight, and your new 
average weight column. Save this tibble as surveys_avg_weight.
'''
survey_mean_weight <- surveys %>%  #if no new df is created we won't be able to see the new columns 
  filter(!is.na(weight)) %>% #"ignore" all NAs in weight 
  group_by(species_id, sex) %>% #group by weight of species + sex
  mutate(mean_weight = mean(weight)) %>% # add the new column called mean_weight 
  select(species_id, sex, weight, mean_weight) #only keep species_id, sex, weight and mean_weight

head(survey_mean_weight) # only our 4 selected columns

#6
'''
Take surveys_avg_weight and add a new column called above_average that contains 
logical values stating whether or not a row’s weight is above average for its 
species+sex combination (recall the new column we made for this tibble).
'''
survey_mean_weight <- survey_mean_weight %>% 
  mutate(above_average = weight > mean_weight) #add new column, considering that the weight is bigger than our mean weight value, created in last question 
#logical value is detemrined by the > symbol

head(survey_mean_weight)




















 