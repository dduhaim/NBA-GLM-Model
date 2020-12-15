library(XML)
library(RCurl)
library(readxl)
library(standardize)
library(xlsx)
library(dplyr)
library(caret)
library(corrplot)

#Import NBA dataset

NBA_Data <- read_excel("Library/Mobile Documents/com~apple~Numbers/Documents/NBA_Data_EXCEL.xlsx", 
                       col_types = c("text", "date", "text", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "text", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "text"))

#Add column names                             
colnames(NBA_Data) <- c("Day", "Date", "Away.Neutral", "PTSA", "OFFRTA", "DEFRTA", 
                        "NETRTA", "ASTTOA","REBPCTA", "TSPCTA", "PACEA", "PIEA", "Home.Neutral", "PTSH", 
                        "OFFRTH", "DEFRTH", "NETRTH", "ASTTOH", "REBPCTH", "TSPCTH","PACEH", 
                        "PIEH","Home.Win")


#Remove 1st row
Data <- NBA_Data[-c(1),] 

#Convert Home Win to a factor for binary research
Data$Home.Win <- as.factor(Data$Home.Win)

#Convert dataset to data frame
Data <- as.data.frame(Data)
head(Data)
View(Data)

#View variable data structures
str(Data)

summary(Data)


nba_glm1 <- Data[c("OFFRTA", "DEFRTA","NETRTA", "ASTTOA", "REBPCTA", "TSPCTA", "PACEA", "PIEA","OFFRTH", 
                  "DEFRTH", "NETRTH", "ASTTOH", "REBPCTH", "TSPCTH","PACEH", "PIEH")]

nba_glm <- Data[c("Date", "Away.Neutral", "PTSA", "OFFRTA", "DEFRTA", "NETRTA", "ASTTOA","REBPCTA", "TSPCTA", "PACEA", "PIEA", 
                  "Home.Neutral", "PTSH","OFFRTH", "DEFRTH", "NETRTH", "ASTTOH", "REBPCTH", "TSPCTH","PACEH", "PIEH","Home.Win")]

nba_glm1 <- as.numeric(nba_glm1)

str(nba_glm)
print(nba_glm)


# Inspect the data by selecting random rows from a table
sample_n(nba_glm, 3)

# Split the data into training and test set
set.seed(123)
training.samples <- nba_glm$Home.Win %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- nba_glm[training.samples, ]
test.data <- nba_glm[-training.samples, ]


#Build Logistic Regression Model
model <- glm(Home.Win ~., data = nba_glm, family = binomial)
model <- glm(Home.Win ~ OFFRTA + DEFRTA + NETRTA + ASTTOA + REBPCTA + TSPCTA + PACEA + PIEA + OFFRTH + 
             DEFRTH + NETRTH + ASTTOH + REBPCTH + TSPCTH + PACEH + PIEH, data = nba_glm, family = binomial)

#Use either function to view only the coefficients of the model
coef(model)
summary(model)$coef

#How to interpret coefficients, the regression coefficient for air.2 is 0.003. This indicate that one unit increase in 
#the air yards will increase the odds of being a Hit by exp(0.003) 1.00 times.

#Make predictions using test data to evaluate model's performance. 
#Predict class membership probabilities of observations based on predictor variables. 
#Assign observations to the class with the highest probability score (ex. above .5)
probabilities <- model %>% predict(test.data, type = "response")
head(probabilities)
print(probabilities)

test.data$probabilities <- probabilities
View(test.data)


contrasts(test.data$Home.Win)

predicted.classes <- ifelse(probabilities > .7, "y", "n")
head(predicted.classes)
print(predicted.classes)


mean(predicted.classes == test.data$Home.Win)


probability <- model %>% predict(train.data, type = "response")
head(probability)
print(probability)


#View GLM results
model

#Predictions for logistic model
pred <- predict(model)

#View summary of predictions from GLM model
summary(pred)

prob <- 1 / (1 + exp(-pred))
summary(prob)

summary(model)


#Write data to csv file
write.csv(test.data,"nba_predictive_model.csv", row.names = TRUE)
write.table(test.data, file = 'nba_predictive_model.csv', row.names = F, sep = ',')


#Standardize the numeric features
nba_glm1 <- scale(nba_glm1, center = TRUE, scale = TRUE)
head(nba_glm1)
na.omit(nba_glm1)
View(nba_glm1)
str(nba_glm1)


#Perform kmeans cluster technique
km <- kmeans(nba_glm1, centers = 7)
nba_glm1$cluster <- factor(km$cluster)
head(nba_glm1)

centers <- data.frame(cluster=factor(1:3), km$centers)
centers