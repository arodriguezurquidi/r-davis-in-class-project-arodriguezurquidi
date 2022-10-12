Alan R. Urquidi 
Him/his 
Horticulture & Agronomy (research in Viticulture & Enology)


## Where to find useful information 
Google
Stack Overflow
RStudio cheat sheets
Cookbook for R

c() -> is the concatenate function  (adds things together)
length() -> the function tells you the length of the vector 
str() -> function provides details on a vector 

Indexing:
vector_name[] <- empty brakets to index [1] will pull the first item in th vector vector_name[c(1,2,3,n)]

when working with booleans (T,F) we can provide conditional statements, in R | character states for  "OR"
weight <- c(100, 55, 60, 75)
weigth < 100 | weight > 100 #This will show the values for the values in weight that fullfill the conditions 

%in% is a comparisson function, for example:
animals <- c("dog", "cat", "duck", "horse") # we create a vector 

and we copare it to other vector
animals %in% c("Whale", "Alligator", "Canary", "Dog")

R will tel us if the items in the first vector, appear in the second vector:
TRUE(dog) FALSE(cat) FALSE(duck) FALSE(horse)
 
CREATE A MATRIX
x <- 1:10
y <- 101:110
z <- cbind(x, y, x+y) #cbind states for column bind , rbind does it in rows

DEALING WITH NAs

vector = c(2,4,6,8,10, NA)
mean(vector) = NA because R will include the NA in the calculation 
mean(vecor, na.rm =TRUE) -> will not include the NA in the calculations

is.na(vector) -> give us back a boolean of the vector, to see if it has any NAs
!is.na(vector) -> is there none NA in the vectir? If there is one, it will return TRUE

to subset data from a vector removing the NAs

vector[!is.na(vector)] -> this will return te vector without the NAs in it 

#OTHE TYPES OF DATA IN R (not a vector)
- lists
my_list <- list(4,5, 'rat')
class(my_list)
str(my_list)
length(my_list) #length of the whole list
length(my_list[[2]]) <- this will bring the list of item #2 in the list variable 

- data frames (they can have different types of data)
my_df <- data.frame(letters) #letters is a built in function in base R, it shows letter from A-Z
length(my_df) will only show 1 as a result (the number of columns)

dim(my_df) will show the whole structure of the data frame == [1] 26 1 -> number of rows, number of columns 

my_df2 = data.frame(letters, letters, letters) ## This will create the same as before but with 3 letter columns [1]26 3

my_df3 = data.frame(letters, x=1) ## in here again 26 rows, two columns, and the HEADER of the second column will be x


- matrix (They require that all the data is of the same type)
matrix(2 , nrow=10, ncol = 10) ## The first number assigns the value to the matric rows/columns, in this case all values will be ==2

to index matrix
my_matrix[x,y] (row, column respectively)
my_matrix[,1] == will return the first column (you leave the row space blank)
my_matrix[,c(1,2)] == will return the first two columns
my_matrix[c(1,2),] == will return the first two rows
my_matrix[c(1,2),c(1,2)] == will return the first two rows of the first two columns 


- array (an stack of matrices, think about a 3D matrix, where the Z dimension could be for example TIME)

to index 
my_array[x,y,z] # Same as with the matrices, but including the 3rd time dimension


- factors
sex -> factor(c('female', 'male', 'female', 'female')) ### R will assign a value to each term, in this case 
female = 1 ; male = 2 


class(sex) ==factor
typeof(sex) == integer 

levels(sex) == returns the observed values within th factor (female, male)
nlevels(sex) ==  how many levels there are (2), which are they(male, female), and returns the number in order (1 2 1 1)

Factors can be created in different ways:
my_factor = factor(sex, levels = c("male", "female")) ## In this case the male = 1 and female = 2

years = factor(c(1990, 1989, 1970, 1968)) ## Factors from 1-4
as.numeric(factor) # this will return the numbers assigned to the factor values, (1-4, not the actual years)

To get the reslt as a numeric from the years we can use 
as.numeric(as.character(years)) ## This will return the year values as actual numbers 

To index factor or change level values 
levels(sex) # will return "male" "female"
levels(sex)[1] <- "MALE"  ## Will change the level of the factor from "male" -> "MALE"






























































 










