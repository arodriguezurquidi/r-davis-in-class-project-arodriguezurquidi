library(readxl)
library(ggpubr)
library(ggplot2)
library(data.table)
library(corrplot)
library(Hmisc)
library(ggstatsplot)
library(correlation)
library(psych)

data <- read_excel("data/r-analysis.xlsx")
View(data)

dat2 <- lapply(data,as.numeric)
data2 <- as.data.frame(data)

data2$id <- as.factor(data2$id)
data2$block <- as.factor(data2$block)
data2$time <- as.factor(data2$time)
data2$row <- as.factor(data2$row)

#data3 = data2, but in data.table format
data3 = data.table(data2) # First turn the data frame into a data table, otherwise you get a type error
#data3[,sapply(.SD, mean)]

# data4 = data3, but witout the character (non-numerical variables)
data4 <- data3[,-c(1:3,6,7)] #remove the non-numerical variables for correlation 


names(which(colSums(is.na(data4)) > 0))

### CORRELATIONS ###
#Aqui primero viste cuales quitar, las quitaste en la linea de abajo. 
data4 <- subset(data4, select = -c(gbw, gtc, SVPleaf, RHcham, VPcham, VPDleaf, LatHFlux, NetTherm, Qabs_fs, A_fs))

data5 = data3
data5$id <- as.factor(data5$id)
data5$block <- as.factor(data5$block)
data5$time <- as.factor(data5$time)
data5$row <- as.factor(data5$row)
data5$`vine#`= as.factor(data5$`vine#`)


# improved correlation matrix
corrplot(cor(data4),
         method = "number",
         type = "upper" # show only upper side
)

res <- rcorr(as.matrix(data4))
res

# Extract the correlation coefficients
res$r
# Extract p-values
res$P

# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

#Hmisc
# correlation tests for whole dataset
res <- rcorr(as.matrix(data4)) # rcorr() accepts matrices only
# display p-values (rounded to 3 decimals)
round(res$P, 3)

#ESTE ESTA CHIDO
# ggstatsplot
## plot with statistical results
ggscatterstats(
  data = data4,
  x = nsc,
  y = starch,
  bf.message = FALSE,
  marginal = FALSE # remove histograms
)
#TleafEB, SVPcham, relhum, soiltemp

# correlation library ALL RELATIONSHIPS, WITH p-values
correlation(data4, include_factors = TRUE, method = "auto")

# grouped correlations
data5 %>%
  group_by(time) %>%
  correlation()


# Correlation between two variables, with p-values displayed
ggpubr::ggscatter(data4, x = "nsc", y = "TleafEB",
          add = "reg.line",                                 # Add regression line
          conf.int = TRUE,                                  # Add confidence interval
          add.params = list(color = "blue",
                            fill = "gray")
)+
  ggpubr::stat_cor(method = "pearson", label.x = 0.005, label.y = 0.005)  # Add correlation coefficient



#By block

ggpubr::ggscatter(data5, x = "nsc", y = "starch",
          add = "reg.line",                         # Add regression line
          conf.int = TRUE,                          # Add confidence interval
          color = "block", palette = "jco",           # Color by groups "cyl"
          shape = "block"                             # Change point shape by groups "cyl"
)+
  ggpubr::stat_cor(aes(color = block), label.x = 0.005)           # Add correlation coefficient



#2d density estimation
sp <- ggpubr::ggscatter(data5, x = "nsc", y = "starch",
                color = "gray")
sp + geom_density_2d()
# Gradient color
sp + stat_density_2d(aes(fill = ..level..), geom = "polygon")
# Change gradient color: custom
sp + stat_density_2d(aes(fill = ..level..), geom = "polygon")+
  ggpubr::gradient_fill(c("white", "steelblue"))
# Change the gradient color: RColorBrewer palette
sp + stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  ggpubr::gradient_fill("YlOrRd")


round(cor(data4),
      digits = 2 # rounded to 2 decimals
)





#######################################################################################################
# PLOTS

# library(data.table)
# data3 = data.table(data2)
#data5 

plot(nsc ~ block, data = data5, type = 'l')           # numeric vector ~ factor
plot(starch ~ block, data = data5, type = 'l') 

plot(nsc ~ time, data = data5, type = 'l')           # numeric vector ~ factor
plot(starch ~ time, data = data5, type = 'l') 

plot(nsc ~ id, data = data5, type = 'l')           # numeric vector ~ factor
plot(starch ~ id, data = data5, type = 'l') 

qplot(id, nsc, data = data5) # facotr, then numeric


subsets<-split(data5, data5$id, drop=TRUE)

V1 <- subsets$V1
plot(nsc ~ time, data = V1, type = 'p')
plot(starch ~ time, data = V1, type = 'l')
subsets$V1

