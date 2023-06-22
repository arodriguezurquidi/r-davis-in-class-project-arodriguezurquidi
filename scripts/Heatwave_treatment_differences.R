library(readxl)
library(dplyr)
library(purrr)
library(corrplot)
library(rstatix)
library(Hmisc)
library(psych)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(cowplot)
library(rlang)
library(gridExtra)
library(Rmisc)
library(ggsignif)
library(emmeans)
library(randomForest)
library(multcomp)
library(multcompView)
library(patchwork)
library(ggside)
library(ggstatsplot)
library(correlation)
library(forestplot)
library(caret)

############################################ Data preparation ----
data <- read_excel("E:/Actual - Edited-NSCs 5_16_2023/R-DAVIS-2022/r-davis-in-class-project-arodriguezurquidi/data/marc_hw_treatments.xlsx")
#View(marc_hw_treatments)
str(data)

data$block = as.factor(data$block)
data$treatment = as.factor(data$treatment)
data$hw_treatment = as.factor(data$hw_treatment)
data$id = as.factor(data$id)
data$row = as.factor(data$row)
data$time = as.factor(data$time)
str(data)
summary(data)

### data with all licor variables + soil & temp
data2 <- read_excel("E:/Actual - Edited-NSCs 5_16_2023/R-DAVIS-2022/r-davis-in-class-project-arodriguezurquidi/data/r-analysis.xlsx")
str(data2)

data2$id = as.factor(data2$id)
data2$block = as.factor(data2$block)
data2$row = as.factor(data2$row)
data2$time = as.factor(data2$time)
data2$time <- factor(data2$time, levels = c("700", "1000", "1300", "1600", "1900", "2200", "100", "400"), ordered = T) # Tiempos ordenados
data2$vine = as.factor(data2$vine)
data2$treatment = data$treatment
data2$treatment_n = data$treatment_n
data2$hw_treatment = data$hw_treatment
str(data2)
summary(data2)

############################################ Forest plots for highest values display----

# Order the data by descending nsc values
ordered_data <- data2 %>% arrange(desc(nsc))

# Select the top 10 IDs with highest nsc values
top_10 <- head(ordered_data, 10)

# Create a dataframe for the forest plot
forest_data <- data.frame(
  id = as.character(top_10$id),
  nsc = top_10$nsc,
  prune_w = top_10$prune_w,
  harvest_w = top_10$harvest_w,
  block = top_10$block,
  stringsAsFactors = FALSE
)

# Create the forest plot using ggplot2
forest_plot <- ggplot(forest_data, aes(x = id, y = nsc)) +
  geom_point(aes(color = "nsc"), size = 3, shape = 16) +
  geom_linerange(aes(ymin = nsc, ymax = nsc), color = "blue", linewidth = 0.5) +
  geom_point(aes(x = id, y = prune_w, color = "prune_w"), size = 3, shape = 16) +
  geom_linerange(aes(ymin = prune_w, ymax = prune_w), color = "red", linewidth = 0.5) +
  geom_point(aes(x = id, y = harvest_w, color = "harvest_w"), size = 3, shape = 16) +
  geom_linerange(aes(ymin = harvest_w, ymax = harvest_w), color = "green", linewidth = 0.5) +
  labs(x = "ID", y = "Value", title = "Top 10 IDs with Highest NSC, Prune_w, and Harvest_w",
       color = "Variable") +
  scale_color_manual(values = c(nsc = "blue", prune_w = "red", harvest_w = "green")) +
  facet_wrap(~block, nrow = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5))

# Display the forest plot
print(forest_plot)


### t.tests, ANOVAs & Tukeys, for treatment Comparisson (NSCs & Starch) ###----
### USE data2!! ###

                                    ### NSCs ###
### t.test
# 2L to 4L t-test
nsc_result <- t.test(nsc ~ treatment, data = data2)
nsc_result2 <- t.test(nsc ~ treatment_n, data = data2) #numeric treatment, for after predictions
# Print the results
print(nsc_result)
print(nsc_result2) # No difference between treatments

# Perform pairwise t-tests with Bonferroni correction
posthoc_correctedp <- pairwise.t.test(data2$nsc, data2$treatment, p.adjust.method = "bonferroni")
posthoc_correctedp2 <- pairwise.t.test(data2$nsc, data2$hw_treatment, p.adjust.method = "bonferroni")

# Print the pairwise comparisons
print(posthoc_correctedp)
print(posthoc_correctedp2)

# ANOVAs

nsc_result3 <- aov(nsc ~ treatment + hw_treatment, data = data2)
nsc_result4 <- aov(nsc ~ treatment * hw_treatment, data = data2)
nsc_result5 <- aov(nsc ~ time, data = data2)
nsc_result6 <- aov(nsc ~ time * treatment * hw_treatment, data = data2)

# Print the results
print(summary(nsc_result3))
print(summary(nsc_result4))
print(summary(nsc_result5))
print(summary(nsc_result6))

# Tukeys
# Perform Tukey's HSD test
posthoc1 <- TukeyHSD(nsc_result3)
posthoc2 <- TukeyHSD(nsc_result4)
posthoc3 <- TukeyHSD(nsc_result5)
posthoc4 <- TukeyHSD(nsc_result6)

# Print the pairwise comparisons
print(posthoc1)
print(posthoc2)
print(posthoc3)
print(posthoc4)

                                 ### STARCH ###
### t. tests 
# 2L to 4L
starch_result <- t.test(starch ~ treatment, data = data2)
starch_result2 <- t.test(starch ~ treatment_n, data = data2) #numeric treatment, for alter predictions
# Print the results
print(starch_result)
print(starch_result2)

# Corroborate the results with a pairwise t-test with a Bonferroni correction in the p-values
posthoc_correctedp <- pairwise.t.test(data2$starch, data2$treatment, p.adjust.method = "bonferroni")
# Print the pairwise comparisons
print(posthoc_correctedp) # In starch levels, there are no significant changes between treatments

### ANOVAs
# Perform ANOVA to corroborate the results again 
starch_result3 <- aov(starch ~ treatment + hw_treatment, data = data2)
starch_result4 <- aov(starch ~ treatment * hw_treatment, data = data2)
starch_result5 <- aov(starch ~ time * treatment * hw_treatment, data = data2)

# Print the results
print(summary(starch_result3))
print(summary(starch_result4))
print(summary(starch_result5)) # Not significant changes between treatments on Starch levels

### Tukeys
# Lastly Perform Tukey's HSD (honestly significant difference) test
# compare the means of multiple groups. It allows for pairwise comparisons between 
# all groups to determine if there are significant differences among them.
posthoc1 <- TukeyHSD(starch_result3)
posthoc2 <- TukeyHSD(starch_result4)
posthoc3 <- TukeyHSD(starch_result5)

# Print the pairwise comparisons
print(posthoc1)
print(posthoc2)
print(posthoc3) # No difference between any

### Difference and relevance between time slots ----
# Perform Tukey's post hoc test  - Here you are comparing the importance
# in time change interactions. For example, the chang efrom 7 am to 10 am 
tukey_result <- glht(starch_result5, linfct = mcp(time = "Tukey"))
# Summarize the Tukey's test results
summary(tukey_result)
# Linear Hypotheses: Relevant time comparissons!!!
#                    Estimate  Std. Error  t value   Pr(>|t|)
# 1000 - 700  == 0  -18.7516    4.5275     -4.142    0.00161 ** 
# 1300 - 1000 == 0   6.2456     4.5275      1.379    0.86496    
# 1600 - 1300 == 0   1.9041     4.5275      0.421    0.99989     
# 1900 - 1600 == 0  -13.4936    4.5275     -2.980    0.06530 .   
# 2200 - 1900 == 0   0.1713     4.5275      0.038    1.00000     
# 2200 - 100  == 0  -29.3865    4.5275     -6.491  < 0.001 *** 
# 400  - 100  == 0   22.1041    4.5275      4.882  < 0.001 *** 
# 700  - 400  == 0  -27.5665    4.5275     -6.089  < 0.001 ***  

# This section (1-3), is just to demonstrate that the highest difference is from 
# 4 am to 7 am, and therefore we should consider reading from 10 to 7 am, ignoring
# 1 and 4 am

#1 Create a subset of data for the time intervals of interest (100 and 400)
subset_data <- data2[data2$time %in% c(1000, 400), ]
#2 Perform pairwise t-tests for the time intervals
pairwise_result <- pairwise.t.test(subset_data$nsc, subset_data$time, p.adjust.method = "bonferroni")
#3 Print the pairwise comparison results
print(pairwise_result) # p = 2.2e-13 ***


############################################ CAN WE ELIMINATE 1 and 4 AM? #######
# Create a custom contrast matrix
contrast_matrix <- matrix(c(-1, 1, 0, 0, 0, 0, 0, 0,    # 700 - 1000
                            0, -1, 1, 0, 0, 0, 0, 0,    # 1000 - 1300
                            0, 0, -1, 1, 0, 0, 0, 0,    # 1300 - 1600
                            0, 0, 0, -1, 1, 0, 0, 0,    # 1600 - 1900
                            0, 0, 0, 0, -1, 1, 0, 0,    # 1900 - 2200
                            0, 0, 0, 0, 0, -1, 1, 0,    # 2200 - 100
                            -1, 0, 0, 0, 0, 0, 0, 1     # 100 - 400
), ncol = 8, byrow = TRUE)
# Perform pairwise t-tests with the custom contrast matrix
pairwise_result <- pairwise.t.test(data2$nsc, data2$time, p.adjust.method = "bonferroni", pool.sd = FALSE,
                                   paired = FALSE, alternative = "two.sided", contr = contrast_matrix)

# Print the pairwise comparison results
print(pairwise_result)

# Create a matrix of p-values from the pairwise comparison results
p_values <- pairwise_result$p.value
row_names <- rownames(p_values)
col_names <- colnames(p_values)

# Create a data frame for the heatmap plot
heatmap_data <- expand.grid(row = row_names, col = col_names)
heatmap_data$p_value <- as.vector(p_values)

# Create the heatmap plot
ggplot(heatmap_data, aes(x = col, y = row, fill = p_value, label = sprintf("%.4f", p_value))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Time", y = "Time", fill = "p-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_equal() +
  ggtitle("Pairwise Comparison - p-values") +
  geom_text(color = "black", size = 3)  # Add p-values as text labels
#
# 1000 to 400: p-value = 1.3e-10 (very highly significant) NOT IMPORTANT
# 1900 to 400: p-value = 7.2e-12 (very highly significant) NOT IMPORTANT
# 2200 to 400: p-value = 9.3e-12 (very highly significant) NOT IMPORTANT
# 1900 to 1000: p-value = 0.00029 (highly significant) NOT
# 2200 to 1000: p-value = 0.00029 (highly significant) NOT
# 700 to 400: p-value = 4.4e-05 (significant) <- WE CAN REMOVE 4 AM 
# 1600 to 1000: p-value = 0.14866 (not significant) NOT
# 1300 to 1000: p-value = 0.23577 (not significant) NOT
# 1600 to 400: p-value = 0.28210 (not significant) NOT
# 1000 to 700: p-value = 0.00104 (significant) NOT
# 1900 to 700: p-value = 3.9e-05 (significant) NOT
# 2200 to 700: p-value = 3.0e-05 (significant) 
# 1300 to 400: p-value = 6.3e-07 (very highly significant)
# 1600 to 700: p-value = 0.19863 (not significant)
# 1300 to 700: p-value = 0.73416 (not significant)



############################################ Irrigation treatment numerical (2-4L), Linear regressions, Tukeys, t.test (data) ----

                                    ### NSCs ###

# Calculate the correlation coefficients
correlation <- cor(data2$nsc, data2$treatment_n)
print(correlation)

# Perform linear regression analysis
model <- lm(data$nsc ~ data$treatment_n)
summary(model)

####
# Coefficients:
# The coefficients section presents the estimates for the intercept and the coefficient associated with the data$treatment_n 
# variable. These estimates represent the expected change in the dependent variable for each unit increase in the independent 
# variable.
# 
# The estimate for the intercept is 73.1366, suggesting that when data$treatment_n is zero, the expected value 
# of data$nsc is 73.1366. The p-value associated with the (Intercept) represents the probability of observing such an 
# extreme or more extreme value for the intercept coefficient, assuming the null hypothesis that the true value of the 
# coefficient is zero.
# A small p-value (e.g., less than the conventional significance level of 0.05) suggests strong evidence against the null 
# hypothesis. Therefore, in this case, the small p-value for the (Intercept) term indicates that the intercept coefficient 
# is statistically significant, meaning that the y-intercept is significantly different from zero.


# The estimate for data$treatment_n is 0.5019, indicating that for each unit 
# increase in data$treatment_n, the expected change in data$nsc is 0.5019. However, the associated 
# p-value for this coefficient is 0.778, indicating that it is not statistically significant.


# Residual standard error:
# The residual standard error (RSE) is an estimate of the standard deviation of the residuals. In this case, the 
# RSE is 21.22, indicating the average amount by which the observed data$nsc values deviate from the predicted values.

# Multiple R-squared and Adjusted R-squared:
# The multiple R-squared value represents the proportion of variability in the dependent variable that can be explained 
# by the independent variable(s). In this case, the R-squared value is 0.0005602, suggesting that the data$treatment_n 
# variable explains a very small fraction of the variability in data$nsc. The adjusted R-squared value takes into account 
# the number of predictors in the model, and in this case, it is slightly negative (-0.006478), suggesting that the model does 
# not improve the prediction of data$nsc compared to a simple mean.
# 
# F-statistic and p-value:
# The F-statistic assesses the overall significance of the model by comparing the variability explained by the model to 
# the unexplained variability. In this example, the F-statistic is 0.07959, with a corresponding p-value of 0.7783, indicating 
# that the model is not statistically significant in explaining the variability in data$nsc.
# Suggesting that the treatment have no signifficant effect on the NSCs levels

# NSCs predictions with variable water treatments
# New data for prediction
data2$treatment_one = data2$treatment_n-1
data2$treatment_one = data2$treatment_n-1.5

# Make predictions
predictions <- predict(model, data = data2$treatment_one)
# Print the predictions
print(predictions)

# Obtain predictions with confidence intervals
pred_interval <- predict(model, data = data2$treatment_one, interval = "confidence")
# Print the predictions with confidence intervals
print(pred_interval)

plot(data2$nsc, data$treatment_n, pch = 16, xlab = "x", ylab = "y", main = "Linear Regression") # Plot the observed data points
abline(model, col = "blue") # Add the regression line
lines(data$nsc, pred_interval[, "lwr"], col = "red", lty = 2) # Add confidence intervals
lines(data$nsc, pred_interval[, "upr"], col = "red", lty = 2)
legend("topright", legend = c("Data", "Regression Line", "Confidence Intervals"),
       col = c("black", "blue", "red"), lty = c(NA, 1, 2), pch = c(16, NA, NA)) # Add legend


                                    ### Starch ###

# Calculate the correlation coefficient
correlation2 <- cor(data2$starch, data2$treatment_n)
print(correlation2)

# Perform linear regression analysis
model2 <- lm(data2$starch ~ data2$treatment_n)
summary(model2)

# Make predictions
predictions <- predict(model2, data = data2$treatment_one)
# Print the predictions
print(predictions)

# Obtain predictions with confidence intervals
pred_interval2 <- predict(model2, data = data2$treatment_one, interval = "confidence")
# Print the predictions with confidence intervals
print(pred_interval2)

plot(data2$starch, data2$treatment_n, pch = 16, xlab = "x", ylab = "y", main = "Linear Regression") # Plot the observed data points
abline(model, col = "blue") # Add the regression line
lines(data$nsc, pred_interval[, "lwr"], col = "red", lty = 2) # Add confidence intervals
lines(data$nsc, pred_interval[, "upr"], col = "red", lty = 2)
legend("topright", legend = c("Data", "Regression Line", "Confidence Intervals"),
       col = c("black", "blue", "red"), lty = c(NA, 1, 2), pch = c(16, NA, NA)) # Add legend

### If is not water, then what is it? Random Forest model for variable importance----

#Splitting data into training and test sets 
set.seed(666)  # Set a seed for reproducibility
train_indices <- createDataPartition(data2$nsc, times = 1, p = 0.7, list = FALSE) #70% of the data
train_data <- data2[train_indices, ]
test_data <- data2[-train_indices, ]

# Random Forest model for nsc
model_nsc <- randomForest(nsc ~ ., data = train_data, ntree = 5000, importance = TRUE)

# Random Forest model for starch
model_starch <- randomForest(starch ~ ., data = train_data, ntree = 5000, importance = TRUE)
  #ntree is the number of trees in the forest

#### Predict on test data
nsc_pred <- predict(model_nsc, newdata = test_data)
starch_pred <- predict(model_starch, newdata = test_data)

# Assess model performance
nsc_rmse <- sqrt(mean((nsc_pred - test_data$nsc)^2))
starch_rmse <- sqrt(mean((starch_pred - test_data$starch)^2))

# Print RMSE (Root Mean Squared Error) for each model, average difference between actual 
# values and predicted values. The lower, the better. 
print(paste("RMSE for nsc:", nsc_rmse))
print(paste("RMSE for starch:", starch_rmse))

# Variable importance for "nsc"
varimp_nsc <- importance(model_nsc)
# print(varimp_nsc) not ordered results
varimp_nsc_ordered <- varimp_nsc[order(varimp_nsc[, 1]), ] # %IncMSE is ordered from smallest to largest
print(varimp_nsc_ordered)


# Variable importance for "starch"
varimp_starch <- importance(model_starch)
print(varimp_starch)

##### Output of the models:
# "%IncMSE" (percentage increase in mean squared error) & 
# "IncNodePurity" (increase in node purity).
''' 
"%IncMSE": This metric measures the increase in mean squared error when a variable is 
randomly permuted or removed from the model. Higher values indicate greater importance of the 
variable in predicting the target variable. For example, variables like "sugar_perc" 
and "leafwp" have high "%IncMSE" values, suggesting they have a significant impact on 
predicting "nsc". THE HIGHER, THE MORE IMPORTANT ON PREDICTED VARIABLE

"IncNodePurity": This metric measures the increase in node purity (impurity reduction) 
associated with a variable. Variables with higher values contribute more to the overall 
purity of the nodes in the decision trees. In this case, variables like "VPcham" and "SVPleaf" 
have high "IncNodePurity" values, indicating their importance in predicting "nsc".

From the results, the variables displaying  anegative sign, negatively contribute to the 
accuracy of the forest models, and therefore we can remove them:
'''
#### THIS IS NOT STRICTLY NECESSARY, WHILE IMPROVES THE ACCURACY OF THE MODEL, THE DIFFERENCE 
# IS NOT SIGNIFICANT, LO HICISTE PARA VER SI AFECTABA< NO LO REPITAS CON EL STARCH!!

# Identify variables with non-negative %IncMSE values
positive_vars <- rownames(varimp_nsc[varimp_nsc[, "%IncMSE"] >= 0, , drop = FALSE])

# Subset the training and test data to include only the positive variables
train_data_filtered <- train_data[, c(positive_vars, "nsc")]
test_data_filtered <- test_data[, c(positive_vars, "nsc")]

# Fit a new Random Forest model using the filtered data
model_nsc_filtered <- randomForest(nsc ~ ., data = train_data_filtered, ntree = 5000, importance = TRUE)

nsc_pred <- predict(model_nsc_filtered, newdata = test_data)
nsc_rmse <- sqrt(mean((nsc_pred - test_data$nsc)^2))
print(paste("RMSE for nsc:", nsc_rmse))
# Variable importance for "nsc" filtered 
varimp_nsc <- importance(model_nsc_filtered)
# print(varimp_nsc) not ordered results
varimp_nsc_ordered <- varimp_nsc[order(varimp_nsc[, 1]), ] # %IncMSE is ordered from smallest to largest
print(varimp_nsc_ordered)

############################################ Factor Correlations & FUll variables and correlations with NSCs ----
#Data2#

#### Data preparation ###
# Subset the data frame without the factor variables
subset_data2 <- data2 %>% 
  select_if(negate(is.factor))

str(subset_data2)

cor_matrix <- cor(subset_data2)
# Create correlation plot
corrplot(cor_matrix, method = "circle", type = "full", tl.cex = 0.8)

# Extract correlations for the variables of interest (nsc and starch)
cor_subset <- cor_matrix[c("nsc", "starch"), ] #### Este es el que usas para plots solo de NSC y starch 
corrplot(cor_subset, method = "number", type = "full", tl.cex = 0.8)
# Relevant variables to NSCs and Starch:

#From Li-Cor gas exchange:
#TleafEB - in °C - Leaf temperature from energy balance
#VPcham - kPa - Vapor pressure in the chamber
#SVPcham - kPa - Saturation vapor pressure in the chamber

# From SIMIS data:
#airt, relhum, soiltemp

#Correlation between two numerical variables, this should be performed after acquiring the most significant p values for the whole dataset
# ggstatsplot
## plot with statistical results
selected_variables <- c("nsc", "starch", "TleafEB", "VPcham", "SVPcham", "airt", "relhum", "soiltemp")
subset_data3 <- subset_data2[, selected_variables]

####BORRA ESTA LINEA, SOLO ERA PARA PROBAR GIT!2
#Segunda linea de codigo ara borrar en git

### Correlation to numerical Variables (only plots) NSCs ###----
sp1 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = airt,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)
plot(sp1)
sp2 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = relhum,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
  )

sp3 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = soiltemp,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sp4 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = TleafEB,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sp5 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = VPcham,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sp6 <- ggscatterstats(
  data = subset_data3,
  x = nsc,
  y = SVPcham,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

# Combine the scatterplots into a single image
combined_plot <- sp1 + sp2 + sp3 + sp4 + sp5 + sp6

# Display the combined plot
combined_plot

### Correlation to numerical Variables (only plots) Starch ----

sps1 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = airt,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)
#plot(sp1)

sps2 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = relhum,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sps3 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = soiltemp,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sps4 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = TleafEB,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sps5 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = VPcham,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

sps6 <- ggscatterstats(
  data = subset_data3,
  x = starch,
  y = SVPcham,
  bf.message = T,
  marginal = T, # remove histograms
  binwidth = 5
)

# Combine the scatterplots into a single image
combined_plot2 <- sps1 + sps2 + sps3 + sps4 + sps5 + sps6

# Display the combined plot
combined_plot2


############################################ Box Plots, by Block, treatment, hw_treatment & row ----

                                    ### NSCs ###
### Boxplots
# Block, treatment, hw_treatment, row
by_blocks <- ggplot(data2, aes(x = time, y = nsc, fill = block)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "NSC (Mean)") +
  theme_minimal()
print(by_blocks)

by_treatment <- ggplot(data2, aes(x = time, y = nsc, fill = treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "NSC (Mean)") +
  theme_minimal()
print(by_treatment)

by_hwtreatment <- ggplot(data2, aes(x = time, y = nsc, fill = hw_treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "NSC (Mean)") +
  theme_minimal() + 
  scale_fill_manual(values = c("NS" = "light blue", "S" = "red"))
print(by_hwtreatment)

by_row <- ggplot(data2, aes(x = time, y = nsc, fill = row)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "NSC (Mean)") +
  theme_minimal()
print(by_row)

# Combine the scatterplots into a single image
combined_plot3 <- by_blocks + by_treatment + by_hwtreatment + by_row +
  ggtitle("NScs")+
  theme(plot.title = element_text(size = 20),
        plot.title.position = "panel")
# Display the combined plot
combined_plot3

                                    ### Starch ###
### Boxplots
# Block, treatment, hw_treatment, row
sby_blocks <- ggplot(data2, aes(x = time, y = starch, fill = block)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "Starch (Mean)") +
  theme_minimal()
print(sby_blocks)

sby_treatment <- ggplot(data2, aes(x = time, y = starch, fill = treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "Starch (Mean)") +
  theme_minimal()
print(sby_treatment)

sby_hwtreatment <- ggplot(data2, aes(x = time, y = starch, fill = hw_treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "Starch (Mean)") +
  theme_minimal() + 
  scale_fill_manual(values = c("NS" = "light blue", "S" = "red"))
print(sby_hwtreatment)

sby_row <- ggplot(data2, aes(x = time, y = starch, fill = row)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(x = "Time", y = "Starch (Mean)") +
  theme_minimal()
print(sby_row)

# Combine the scatterplots into a single image
combined_plot4 <- sby_blocks + sby_treatment + sby_hwtreatment + sby_row + 
  ggtitle("Starch")+
  theme(plot.title = element_text(size = 20),
        plot.title.position = "panel")

# Display the combined plot
combined_plot4

###2nd Plot Option, by Lines---- 
# Base code #
chart2 <- ggplot(data, aes(x = time, y = nsc, color = block)) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = mean_se, geom = "smooth", aes(group = block), method = "loess") +
  labs(x = "Time", y = "NSC (Mean)") +
  theme_minimal()
print(chart2)

# Convert time to a factor with the desired order of levels
#data$time <- factor(data$time, levels = c("700", "1000", "1300", "1600", "1900", "2200", "100", "400"), ordered = T) # Tiempos ordenados
summary_data <- summarySE(data, measurevar = "starch", groupvars = c("block", "time", "treatment", "hw_treatment", "row"), na.rm = TRUE)
summary_data2 <- summarySE(data, measurevar = "nsc", groupvars = c("block", "time", "treatment", "hw_treatment", "row"), na.rm = TRUE)


                            ### NSCs by Block ###
chart3 <- ggplot(summary_data2, aes(x = time, y = nsc, color = block)) +
  #geom_point(shape = 21, size = 3, fill = "white") +
  geom_smooth(aes(group = block), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) +
  labs(title = "NSCs by Block", x = "Time", y = "NSC (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart3)

# NSCs by emitter treatment
chart5 <- ggplot(summary_data2, aes(x = time, y = nsc, color = treatment)) +
  geom_smooth(aes(group = treatment), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "NSCs by Emitters (2L, 4L)", x = "Time", y = "NSC (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart5)

# NSCs by hw_treatment
chart6 <- ggplot(summary_data2, aes(x = time, y = nsc, color = hw_treatment)) +
  geom_smooth(aes(group = hw_treatment), method = "loess", se = TRUE, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "NSCs by HW_Treatment", x = "Time", y = "NSC (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart6)

# NSCs by row
chart7 <- ggplot(summary_data2, aes(x = time, y = nsc, color = row)) +
  geom_smooth(aes(group = row), method = "loess", se = F, linewidth = 1.5) +
  #geom_point(shape = 21, size = 3, fill = "white") + #Bolitas en cada punto, no son necesarias
  #geom_errorbar(aes(ymin = nsc - se, ymax = nsc + se), width = 0.2, linewidth = 1.0) + # Error bars, funky from the summarized data
  labs(title = "NSCs by Row", x = "Time", y = "NSC (Mean) in µg/mg in dry sample") +
  theme_minimal()
print(chart7)

combined_plot = chart3 + chart5 + chart6 + chart7
print(combined_plot)

summary_data11 <- summarySE(data, measurevar = "starch", groupvars = c("block", "time", "treatment", "hw_treatment", "row"), na.rm = TRUE)


### Numerical comparisson ()





############################################ Extras #Additional correlation approaches (not used yet) ----
# correlation library ALL RELATIONSHIPS, WITH p-values
correlation(subset_data2, include_factors = TRUE, method = "auto")

# grouped correlations
data2 %>%
  group_by(time) %>%
  correlation()


# Correlation between two variables, with p-values displayed
ggpubr::ggscatter(subset_data2, x = "nsc", y = "prune_w",
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "blue",
                                    fill = "gray")
)+
  ggpubr::stat_cor(method = "pearson", label.x = 0.005, label.y = 0.005)  # Add correlation coefficient



#By block

ggpubr::ggscatter(data2, x = "nsc", y = "prune_w",
                  add = "reg.line",                         # Add regression line
                  conf.int = TRUE,                          # Add confidence interval
                  color = "block", palette = "jco",           # Color by groups "cyl"
                  shape = "block"                             # Change point shape by groups "cyl"
)+
  ggpubr::stat_cor(aes(color = block), label.x = 0.005)           # Add correlation coefficient



#2d density estimation
sp <- ggpubr::ggscatter(subset_data2, x = "nsc", y = "starch",
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


round(cor(subset_data2),
      digits = 2 # rounded to 2 decimals
)
############################################ nsc & starch with predicted treatment values ----

#NSCs#
correlation <- cor(data$nsc, data$treatment_n)
correlation

# Fit a linear regression model
model <- lm(nsc ~ treatment_n, data = data)
summary(model)

# Predict nsc values for specific treatment_n levels
#new_treatment_n <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 3.5, 4.5, 5)
new_treatment_n <- seq(0.1,5, by = 0.1)
predictions <- predict(model, newdata = data.frame(treatment_n = new_treatment_n))

# Print the predicted nsc values
predictions

# Create a data frame with the predicted values and corresponding treatment_n levels
prediction_data <- data.frame(nsc = predictions, new_treatment_n = new_treatment_n)

# Plot the predictions
ggplot(prediction_data, aes(x = new_treatment_n, y = nsc)) +
  geom_point() +
  xlab("Water in Liters/hour") +
  ylab("nsc in µg/mg") +
  ggtitle("Predicted nsc values vs. new_treatment_n levels")


# Perform pairwise comparisons between predictions and treatment_n levels
comparison_results <- data.frame(new_treatment_n, predictions)

# Print the comparison results
print(comparison_results)

# Perform pairwise Wilcoxon signed-rank tests to see if the difference between the prediction value results are significant
pairwise_tests <- pairwise.wilcox.test(comparison_results$predictions, comparison_results$new_treatment_n, p.adjust.method = "bonferroni")

# Print the pairwise test results
print(pairwise_tests)

# The diagonal elements (e.g., 0.2, 0.3, 0.4, etc.) represent the comparison of a new_treatment_n level with itself, so the 
# p-value is always 1 (indicating no significant difference).
# 
# The upper and lower triangular elements represent the pairwise comparisons between the new_treatment_n levels. In this case, all 
# the p-values are 1, indicating no significant difference between the nsc levels for any pair of new_treatment_n values.
# 
# Since all the p-values are 1, there is no evidence to suggest a significant difference between any pair of nsc levels based on 
# the new_treatment_n values in the comparison_results data frame.


### Starch
correlation <- cor(data$starch, data$treatment_n)
correlation

# Fit a linear regression model
model <- lm(starch ~ treatment_n, data = data)
summary(model) # the results suggest that maybe we should not use a linear regression approach

# Predict nsc values for specific treatment_n levels
#new_treatment_n <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 3.5, 4.5, 5)
new_treatment_n <- seq(0.1,5, by = 0.1)
predictions <- predict(model, newdata = data.frame(treatment_n = new_treatment_n))

# Print the predicted nsc values
predictions

# Create a data frame with the predicted values and corresponding treatment_n levels
prediction_data <- data.frame(nsc = predictions, new_treatment_n = new_treatment_n)

# Plot the predictions
ggplot(prediction_data, aes(x = new_treatment_n, y = nsc)) +
  geom_point() +
  xlab("Water in Liters/hour") +
  ylab("Starch in µg/mg") +
  ggtitle("Predicted nsc values vs. new_treatment_n levels")


# Fit a GAM using the mgcv package
library(mgcv)
model <- gam(nsc ~ s(treatment_n), data = data)
summary(model)





