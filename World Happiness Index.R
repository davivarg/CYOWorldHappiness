
##########################################################################################################
#Code for the HarvardX: PH125.9x Data Science – World Happiness Index Project
#Compiled by Davis Varghese 4/28/2020
##########################################################################################################

###############################################################
# Install the packages if needed and load the libraries 
###############################################################

if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('caret')) install.packages('caret'); library('caret')
if (!require('RCurl')) install.packages('RCurl'); library('RCurl')
if (!require('readr')) install.packages('readr'); library('readr')
if (!require('knitr')) install.packages('knitr'); library('knitr')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')


###############################################################
# Connect to github,import and load 2019 data into a dataframe and show 10 scores
###############################################################
urlfile1 <- 'http://raw.githubusercontent.com/davivarg/CYOWorldHappiness/master/2019.csv'
data_2019 <- read.csv(url(urlfile1))
head(data_2019, n =10)

###############################################################
# Connect to github,import and load 2018 data into a dataframe and show 10 scores
###############################################################
urlfile2 <- 'http://raw.githubusercontent.com/davivarg/CYOWorldHappiness/master/2018.csv'
data_2018 <- read.csv(url(urlfile2))
head(data_2018, n =10)

###############################################################
# Summary of 2018 Happiness scores
###############################################################
summary(data_2018$Score)

###############################################################
# Histogram of 2018 Happiness scores
###############################################################
hist(data_2018$Score, col="blue", border="white", main = "Global Happiness Score in 2018", xlab = "Happiness Score")

###############################################################
# Summary of 2019 Happiness scores
###############################################################
summary(data_2019$Score)

###############################################################
# Histogram of 2019 Happiness scores
###############################################################
hist(data_2019$Score, col="green", border="white", main = "Global Happiness Score in 2019", xlab = "Happiness Score")

###############################################################
# Plot of Score vs GDP per capita
###############################################################
  ggplot(data = data_2019, aes(x = Score, y = GDP.per.capita)) + 
  geom_point(color='blue', size = 1) +
  labs(y = "GDP per capita")+
  labs(title = paste("Score vs GDP per capita"))+
  geom_smooth(method = "lm", color = "red", se = TRUE)

###############################################################
# Plot of Score vs Social Support
###############################################################
  ggplot(data = data_2019, aes(x = Score, y = Social.support)) + 
  geom_point(color='blue', size = 1) +
  labs(y = "Social Support")+
  labs(title = paste("Score vs Social Support"))+
  geom_smooth(method = "lm", color = "red", se = TRUE)

###############################################################
# Plot of Score vs Healthy life expectancy
###############################################################
  ggplot(data = data_2019, aes(x = Score, y = Healthy.life.expectancy)) + 
  geom_point(color='blue', size = 1) +
  labs(y = " Healthy life expectancy")+
  labs(title = paste("Score vs Healthy life expectancy"))+
  geom_smooth(method = "lm", color = "red", se = TRUE)

###############################################################
# Plot of Score vs Freedom to make life choices
###############################################################
  ggplot(data = data_2019, aes(x = Score, y = Freedom.to.make.life.choices)) + 
  geom_point(color='blue', size = 1) +
  labs(y = " Freedom to make life choices")+
  labs(title = paste("Score vs Freedom to make life choices"))+
  geom_smooth(method = "lm", color = "red", se = TRUE)

###############################################################
# Plot of Perceptions of corruption
###############################################################
  ggplot(data = data_2019, aes(x = Score, y = Perceptions.of.corruption)) + 
  geom_point(color='blue', size = 1) +
  labs(y = " Perceptions of corruption")+
  labs(title = paste("Score vs Perceptions of corruption"))+
  geom_smooth(method = "lm", color = "red", se = TRUE)

###############################################################
# Plot of Score vs Generosity
###############################################################
  ggplot(data = data_2019, aes(x = Score, y = Generosity)) + 
  geom_point(color='blue', size = 1) +
  labs(y = " Generosity")+
  labs(title = paste("Score vs Generosity"))+
  geom_smooth(method = "lm", color = "red", se = TRUE)

###############################################################
# 2019 Baseline model with all parameters and RMSE
###############################################################
Base_model <- data_2019 %>% mutate(rmse_score = GDP.per.capita +
                               Social.support +
                               Healthy.life.expectancy +
                               Freedom.to.make.life.choices + 
                               Generosity + 
                               Perceptions.of.corruption, 
                             RMSE = RMSE(Score, rmse_score))

# Highest Ranked countries in the Baseline model
Base_model %>%
  filter(Overall.rank <= 10) %>%
  select(Overall.rank, Country.or.region, Score, rmse_score, RMSE)

######################################################################
# 2019 Baseline model with all parameters plus dystopia score and RMSE
######################################################################

Base_model_dys <- data_2019 %>% mutate(dys_rmse_score = GDP.per.capita +
                               Social.support +
                               Healthy.life.expectancy +
                               Freedom.to.make.life.choices + 
                               Generosity + 
                               Perceptions.of.corruption +
			      1.85, 
                             RMSE_dys = RMSE(Score, dys_rmse_score))

# Highest Ranked countries in the Baseline model with dystopia included
Base_model_dys %>%
  filter(Overall.rank <= 10) %>%
  select(Overall.rank, Country.or.region, Score, dys_rmse_score, RMSE_dys)

###############################################
# 2018-2019 model with dystopia value included
###############################################

# Convert Perceptions.of.corruption column to numeric
data_2018$Perceptions.of.corruption <- as.numeric(as.character(data_2018$Perceptions.of.corruption))
data_2018[is.na(data_2018)] <- 0
# Bind 2018-2019 data to obtain a combination
data_1819 <- rbind (data_2019,data_2018)
#Calculate predicted score and RMSE
model_1819 <- data_1819 %>% mutate(score_1819 = GDP.per.capita +
Social.support +
Healthy.life.expectancy +
Freedom.to.make.life.choices +
Generosity +
Perceptions.of.corruption +
1.85,
RMSE_score = RMSE(Score, score_1819))
# RMSE Score for the 18-19 model
RMSE_1819 <- RMSE(model_1819$Score, model_1819$score_1819)
# Print RMSE for 2018-19 model
RMSE_1819

###################################
# Generalized linear model for 2019
###################################
# Set seed 
set.seed(1, sample.kind = "Rounding")
# Create a data partition on a 70-30 ratio split
index_2019 <- createDataPartition(data_2019$Score, times=1, p=0.70, list=FALSE)
train_2019 <- data_2019[index_2019,]
test_2019 <- data_2019[-index_2019,]
# # Fit the Generalized linear model on all parameters
glmfit_2019 <- glm(Score ~ GDP.per.capita +
Social.support +
Healthy.life.expectancy +
Freedom.to.make.life.choices +
Generosity +
Perceptions.of.corruption,
data = train_2019)
# # Get the results into a dataframe and add the predicted scores
results_2019 <- test_2019 %>%
mutate(pred_score_2019 = predict.glm(glmfit_2019, newdata=test_2019))
# # Print the RMSE value
RMSE(results_2019$Score, results_2019$pred_score_2019)

# Print coefficients of the model
glmfit_2019$coefficients

###############################################################
#Generalized linear model for 2019 without Generosity parameter
###############################################################
# Set seed 
set.seed(1, sample.kind = "Rounding")
# Create a data partition on a 70-30 ratio split
index_2019 <- createDataPartition(data_2019$Score, times=1, p=0.70, list=FALSE)
train_2019 <- data_2019[index_2019,]
test_2019 <- data_2019[-index_2019,]
# # Fit the Generalized linear model on all but without the generosity parameter
glmfit_nogen <- glm(Score ~ GDP.per.capita +
Social.support +
Healthy.life.expectancy +
Freedom.to.make.life.choices +
Perceptions.of.corruption,
data = train_2019)
# # Get the results into a dataframe and add the predicted scores
results_nogen <- test_2019 %>%
mutate(pred_score_nogen = predict.glm(glmfit_nogen, newdata=test_2019))
# # Print the RMSE value
RMSE(results_nogen$Score, results_nogen$pred_score_nogen)

# Print coefficients of the model
glmfit_nogen$coefficients

