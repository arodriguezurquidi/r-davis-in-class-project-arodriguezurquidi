# Load your survey data frame with the read.csv() function.
surveys = read.csv(file = 'data/portal_data_joined.csv')
colnames(surveys)

# Create a new data frame called surveys_base with only the  species_id, the weight, and the plot_type columns. 
# Have this data frame only be the first 5,000 rows. 
surveys_base = surveys[1:5000, c("species_id", "weight", "plot_type")]

summary(surveys_base)
str(surveys_base)  
# Convert both species_id and plot_type to factors
surveys_base$species_id <- as.factor(surveys_base$species_id)
surveys_base$plot_type <- as.factor(surveys_base$plot_type)
str(surveys_base)  # now look at the classes


# Remove all rows where there is an NA in the weight column.
# from cran 
surveys_base <- surveys_base[complete.cases(surveys_base), ]
str(surveys_base) #NAs removed from the data frame

# Explore these variables and try to explain why a factor is different from a character. Why might we want to use factors? 
# Can you think of any examples?

# ANS----> Factors are easier to work with if you have repeated values or families of values, if for example I am working 
#with plants and I have ranges of age among them, it would be easier to name my factors "young", "mature", "old" rather 
#than using a single value (either numeric or character) to identify my ranges. Is simpler


# CHALLENGE: Create a second data frame called challenge_base that only consists of individuals from your surveys_base 
# data frame with weights greater than 150g.
colnames(surveys_base)
challenge_base <- surveys_base[surveys_base[,"weight"] > 150,]
summary(challenge_base)
str(challenge_base)

