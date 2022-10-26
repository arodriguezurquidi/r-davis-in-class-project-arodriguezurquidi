#Read in data
surveys <- read.csv('data/portal_data_joined.csv')

#inspect data (nrow and ncol)
nrow(surveys)
ncol(surveys)

#What kind of data 
class(surveys)
View(surveys)
typeof(surveys)

#looking
head(surveys) #fisrt 6
tail(surveys) #last 6

#look at the structure & the summary
str(surveys) #structure of each column 
summary(surveys)

#indexing data frames 
''' for data frames indexing you use [] -> data.frame_name[]
row, column
'''

surveys[1,1] #row 1 of column 1
surveys[1:20,1:3] 
surveys[6] # by default, if no row specified, R will show the column with the n number in this case 6 

#signs or operators 
# : represents a range 
# - substracts
# c() helps us ist values 

surveys[1:6, ] # rows 1 to 6 from all the columns
surveys[-(1:6),] # this will remove the first 6 rows, the - sign needs to be OUTSIDE the parentesis 
surveys[-1,] # since we are only removing one row, no need for parenthesis 

#Indexing by column by name 
colnames(surveys)
surveys[,"day"] # all rows of the column days 

#Indexing by $ 
surveys$day

# video 3.3 
# install package tydiverse 
#install.packages("tydiverse")
library(tidyverse)
surveys_t <- read_csv(file = 'data/portal_data_joined.csv') # the difference with read.csv is that this tydiverse
#function, reads the data as a "tibble". A special data.frame 
class(surveys_t)
head(surveys_t)
























