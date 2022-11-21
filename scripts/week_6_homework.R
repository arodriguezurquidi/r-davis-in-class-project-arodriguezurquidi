library(tidyverse)

gapminder <- read_csv("https://gge-ucd.github.io/R-DAVIS/data/gapminder.csv") #ONLY change the "data" part of this path if necessary

#structure & summary
str(gapminder)
summary(gapminder)
colnames(gapminder) #lifeExp

### 1) 
#First calculates mean life expectancy on each continent. Then create a plot that shows how life expectancy has changed over time in 
# each continent. Try to do this all in one step using pipes! (aka, try not to create intermediate dataframes)
gapminder %>%
  group_by(continent, year) %>% #no filtering here, we are using years as we want to see the difference over time! 
  summarize(mean_lifeExp = mean(lifeExp)) %>% # to get life expectancy on each continent, and then (%>%)...
  ggplot()+ #blank ggplot canvas
  geom_point(aes(x = year, y = mean_lifeExp, color = continent)) +#instead of grouping, we are creating different colors for each continent,
  #just with the points it looks a bit odd!
  geom_line(aes(x = year, y = mean_lifeExp, color = continent)) +  #line plot
#There is a nice trend of increase in the continents life expectancy
  theme_stata() # I like stata, it saves the whole creation of a legend in the plot!

### 2) 
#Look at the following code and answer the following questions. What do you think the scale_x_log10() line of code is achieving? 
#What about the geom_smooth() line of code?
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent), size = .25) + 
  scale_x_log10() +
  geom_smooth(method = 'lm', color = 'black', linetype = 'dashed') +
  theme_bw()
### Ans1) 
#scale_x_log10  = scale 10 is the logarithmic conversion of an axis, which is usually utilized whn working with either extremely large or, 
# or small data variables, it can also be used to show percent change of multiplicative values, in this case as the population values are 
#considerably bigger in some continents, we need to scale the whole plot. 
#WITHOUT LOG CONVERSION
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent), size = .25) + 
  geom_smooth(method = 'lm', color = 'black', linetype = 'dashed') +
  theme_bw()
#If we remove it, while the trendline looks similar, all the values are concentrated in a single side of the plot, with the log conversion,
#everything is better distributed. 

### Ans2)
#WITHOUT SMOOT CONVERSION
#geom_smooth, it helps our eyes when there is a lot of overlapping in the plots, specially when working with big values in the data,
#creates the trendline of data distribution, which helps in reducing the amount of overlapping, by "extending" or increasing the distances
#between the data points
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent), size = .25) + 
  scale_x_log10() +
  #geom_smooth(method = 'lm', color = 'black', linetype = 'dashed') +
  theme_bw()

### CHALLENGE
# Modify the above code to size the points in proportion to the population of the country. Hint: Are you translating data to 
# a visual feature of the plot?

#ORIGINAL CODE
# ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
#   geom_point(aes(color = continent), size = .25) + 
#   scale_x_log10() +
#   geom_smooth(method = 'lm', color = 'black', linetype = 'dashed') +
#   theme_bw()


ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + #this should remain the same, still interested in the same variable interactions
  geom_point(aes(color = continent, size = pop)) + #in the original code, the size was stablished to .25, here we are basically "grouping" them by the size of the population!
  scale_x_log10() + #values are still high, conversion remains the same
  geom_smooth(method = "lm", color = 'black', linetype = 'dashed') + #We don't need to modify nothin in here either
  theme_stata() # I like stata
#IT worked!


### 3)
#Create a boxplot that shows the life expectency for Brazil, China, El Salvador, Niger, and the United States, with the data points 
#in the backgroud using geom_jitter. Label the X and Y axis with “Country” and “Life Expectancy” and title the plot 
#“Life Expectancy of Five Countries”.

#?geom_jitter

#Brasil, China, El Salvador, Niger, US 
#country_list <- c("Brasil", "China", "El Salvador", "Niger", "US") #First create a vector with required names
country_select <- c("Brazil", "China", "El Salvador", "Niger", "United States") #Brazil??

gapminder %>% #database selection, and then (%>%)
  filter(country %in% country_select) %>% #We filter based on the vector we created above, we are telling R to only grab the countries in the database that match our created vectors, names have to be the same!
  ggplot(aes(x = country, y = lifeExp))+ #we are aiming to see the same variables as before
  geom_boxplot() + 
  geom_jitter(alpha = 0.3, color = "blue")+ #geom_jitter is a shortcut for geom_point, alpha is used in geom_point = transparency
  theme_stata() +
  ggtitle("Life Expectancy of Five Countries") + #plot tittle, nice!
  xlab("Country") + ylab("Life Expectancy") #Rename axis names!

# in the answer they used 
#theme(plot.title = element_text(hjust = 0.5)) + #centered the plot title
countries <- c("Brazil", "China", "El Salvador", "Niger", "United States") #create a vector with just the countries we are interested in

gapminder %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(x = country, y = lifeExp))+
  geom_boxplot() +
  geom_jitter(alpha = 0.3, color = "blue")+
  theme_minimal() +
  ggtitle("Life Expectancy of Five Countries") + #title the figure
  theme(plot.title = element_text(hjust = 0.5)) + #centered the plot title, I actually got it centered without this portion, not sure why the plot looks a bit different (point distribution)
  xlab("Country") + ylab("Life Expectancy") #how to change axis names






