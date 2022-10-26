#1 
'''
Create a tibble named surveys from the portal_data_joined.csv file. Then 
manipulate surveys to create a new dataframe called surveys_wide with a column for
genus and a column named after every plot type, with each of these columns 
containing the mean hindfoot length of animals in that plot type and genus. 
So every row has a genus and then a mean hindfoot length value for every plot 
type. The dataframe should be sorted by values in the Control plot type column. 
This question will involve quite a few of the functions you’ve used so far, and 
it may be useful to sketch out the steps to get to the final result.
'''
surveys = read_csv("data/portal_data_joined.csv")

#First we create the mean_hindfoot table
surveys_hindfoot_length <- surveys %>% 
  filter(!is.na(hindfoot_length)) %>% #ignoring the NAs
  group_by(genus, plot_id) %>% #creating the columns 
  summarise(mean_hindfoot_length = mean(hindfoot_length))
#We can include the pivot_wider function in the same %>% , but I rather separate
# them to see clearly what I am doing

surveys_hindfoot_length

wider_table <- pivot_wider(surveys_hindfoot_length, 
                           names_from = plot_id,
                           values_from = mean_hindfoot_length) %>% 
                           arrange(Control)

#?arrange(Control)
wider_table  

#2
'''
Using the original surveys dataframe, use the two different functions we laid 
out for conditional statements, ifelse() and case_when(), to calculate a new 
weight category variable called weight_cat. For this variable, define the rodent 
weight into three categories, where “small” is less than or equal to the 1st 
quartile of weight distribution, “medium” is between (but not inclusive) the 1st 
and 3rd quartile, and “large” is any weight greater than or equal to the 3rd 
quartile. (Hint: the summary() function on a column summarizes the distribution). 
For ifelse() and case_when(), compare what happens to the weight values of NA, 
depending on how you specify your arguments.
'''

summary(surveys$weight)
#small <= 1st quartile (small <= 20)
#medium > 1st quartile & < 3rd quartile (20 < med < 48) 
#large > 3rd quartile ( large > 48)


## Ifelse
surveys %>% 
  mutate(weight_cat = ifelse(weight <= 20.00, "small",
                      ifelse(weight > 20.00 & weight < 48.00, "medium","large")))
#in the large argument, is not necessary to include the ifelse, since everything 
#we assigned before is now enclosed in the other 2 categories. 

## case_when 
surveys %>% 
  mutate(weight_cat = case_when(
    weight <= 20.00 ~ "small", #less than or equal to 20
    weight > 20.00 & weight < 48.00 ~ "medium", #from 20 to 48 not inclusive
    weight >= 48 ~ "large" # larger than 48
  ))

#BONUS: 
'''How might you soft code the values (i.e. not type them in manually) of 
the 1st and 3rd quartile into your conditional statements in question 2?
'''
# would be maybe by indexing?
quantiles <- summary(surveys$weight)
quantiles #1 is min, 2 is first quantile, etc etc.

quantile1 <- quantiles[2]
quantile3 <- quantiles[5]

quantile1
quantile3

surveys %>% 
  mutate(weight_cat = case_when(
    weight <= quantile1 ~ "small",
    weight > quantile1 & weight < quantile3 ~ "medium",
    weight >= quantile3 ~ "large"
  ))



