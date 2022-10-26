#Week 5 notes

#Conditional statements 

#library(tydyverse)
surveys <- read_csv("data/portal_data_joined.csv")
head(surveys)

# if ; else ; for 
if(surveys$year == 1977){print('year is 1977')} #not vector wise with booleans 
if(surveys$year == 1977){print('year is 1977')}else{'other year'} #same as ifelse


#ifelse, works nice if you only have two categories 
ifelse(surveys$year == 1977, yes = print('year is 1977'), no = print('is not 1977'))

#group animals between small and big
surveys$hindfoot_cat <- ifelse(surveys$hindfoot_length < 29.29, yes = "small", no = "big")
head(surveys$hindfoot_cat, 100) # NA means no value was found in length column 
table(surveys$hindfoot_cat) #shows how many of each we have

#for more than two categories, in tydiverse
#case_when ----
surveys %>% 
  mutate(hindfoot_cat = case_when(
    hindfoot_length > 29.9 ~ 'big', #if footlength is bigger than the value, label it 'big'
    hindfoot_length < 10 ~ 'really small',
    TRUE ~ 'small' # Short hand for any other value
  )) %>% 
  select(hindfoot_length, hindfoot_cat) %>% # need to re watch 
  group_by(hindfoot_cat) #NAs, recorded as smalls, that is because the TRUE above includes everything that does not fit in the ifelse!
  summarise(n()) # shows all categories created in the above code 

#group_by(hindfoot_cat) %>% 
  #summarise(n()) # shows all categories created in the above code 

# n() is basically count the number 

# Not including NAs as small ----
  surveys %>% 
    mutate(hindfoot_cat = case_when(
      hindfoot_length > 29.9 ~ 'big', 
      is.na(hindfoot_length) ~ NA_character_, #this line will leave NA values as Nas instead of smalls
      #IT CANNOT BE NA, it needs to be NA_character_
      TRUE ~ 'small'
    )) %>% 
    select(hindfoot_length, hindfoot_cat) %>% # need to re watch 
    group_by(hindfoot_cat) #NAs, recorded as smalls, that is because the TRUE above includes everything that does not fit in the ifelse!
    summarise(n()) # shows all categories created in the above code 

# Joining two data sets ----
    
#1st data is surveys 
    
tail <- read_csv("data/tail_length.csv")
summary(tail)    
str(tail)

summary(surveys$record_id) # we need to check in this case, that the recor_id in both 
#datasets is actually the same, we can check ith the mean an median in both DF

# x is the first data argument you present to R, right is the second
surveys_join <- left_join(x = surveys, 
                          y = tail,
                          by = "record_id") # if not specified, R wil  join all columns sharing name 

    
    
# Pivoting (or re-shaping) datasets     
#create a summary table and reshape it
    
#GOAL    
# compare the different mean weight of each species between plots. So, we'd want 
# genus name on the left hand side, and columns to be different plot numbers, 
#with mean weight inside them 
    
#1) compare mean weight of each specie by plot 
surveys_weight <- surveys %>% 
  filter(!is.na(weight)) %>% #ignore the NAs
  group_by(genus, plot_id) %>% #creating the columns 
  summarise(mean_weight = mean(weight))
surveys_weight #while this work, is not useful for papers. Instead we want each column
#to be one specific plot, with the species as rows and each cell containing the average weight!

# Re-shaping to wider data 
length(unique(surveys_weight$genus)) # 10 total genus of our data 
length(unique(surveys_weight$plot_id)) # total plots 

#pivot_wider(data, 
#names_from = the single column that has the thing you want to be your new column names)
#values_from = the single column that has the values you want inside the table 

wider_table <- pivot_wider(surveys_weight, 
                           names_from = plot_id,
                           values_from = mean_weight)

wider_table


# pivot_longer(data, 
#   cols = the columns that you want to stack on top of one another,
#   names_to = the new column name where you want to store the old/wide column names,
#   values_to = the new column name where you want to store the old/wide column values)

survey_long <- pivot_longer(wider_table,
                            #cols = 1:24,#calling by location because is a number,
                            #shows an error, starts with genus
                            #cols = 1:25, #25 because the first column is GENUS!, this works
                            cols = `1`:`24`,#calling by name using back ticks, same beyboard as ~, it works, but not adviced
                            names_to = 'plot_id', #new name of the column compiling all old column names 
                            values_to = 'mean_weight') # compiles all the values in the table and asigns them to this new single column 

survey_long
wider_table












