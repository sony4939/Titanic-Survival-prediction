#set working directory
setwd('/Users/sony/Desktop/Engineering Management/Buisness Intelligence/project/titanic')

# for data manipulation
library('dplyr') 

# create dataframe for training set
train.data <- read.csv('train.csv', stringsAsFactors = FALSE , header = TRUE)

# create df for test set
test.data <- read.csv('test.csv', stringsAsFactors = FALSE , header = TRUE)

#plot data
library(ggplot2)

# 1. Bar Chart for Passenger Class Distribution
plot1 <- ggplot(data = train.data, aes(x = train.data$Pclass)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Passenger Class Distribution")


# 2. Survival Rate by Sex
plot2 <- ggplot(data = train.data, aes(x = train.data$Sex, fill = factor(train.data$Survived))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("grey", "darkgreen"), labels = c("Died", "Survived")) +
  labs(title = "Survival Rate by Sex")


# 3. Age Distribution Faceted by Passenger Class
plot3 <- ggplot(data = train.data, aes(x = train.data$Age)) +
  geom_histogram(bins = 20, fill = "tomato", alpha = 0.7) +
  facet_wrap(~train.data$Pclass) +
  labs(title = "Age Distribution by Class")


# 4. Scatter Plot of Age vs Fare, colored by Survival
plot4 <- ggplot(data = train.data, aes(x = train.data$Age, y = train.data$Fare, color = factor(train.data$Survived))) +
  geom_point(alpha = 0.6) +
  labs(title = "Age vs. Fare by Survival")

library(gridExtra)

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)


# add training column with True to train.data
train.data$training <- TRUE

# add training column with False to test.data
test.data$training <- FALSE

# check structure of training set
str(train.data)
head(train.data)

# check structure of test set
str(test.data)

#create a column in test.data 'Survived' and fill it with NA
test.data$Survived <- NA

# bind rows of train with test sets and save as new df full.data(vertical join)
full.data <- bind_rows(test.data, train.data)

str(full.data)

# confirm NA in Survived column for number of rows in the test set(418)
full.data %>%
  filter(is.na(Survived)) %>%
  nrow()

##DATA CLEANING

# Convert columns to categorical factors
full.data$Pclass <- as.factor(full.data$Pclass)
full.data$Sex <- as.factor(full.data$Sex)
full.data$Embarked <- as.factor(full.data$Embarked)

# check the structure
str(full.data)

# summarize full.data to find any NA's
summary(full.data) # age and fare have N/A. Embarked and Cabin have missing values

# find missing value in Embarked and Cabin
table(full.data$Embarked)
table(full.data$Cabin) # 1014 missing values, hence discard it

# Filling missing column with mode of all three category
full.data[full.data$Embarked =='' , "Embarked"] <- 'S'

#check how many values of age column are missing/NA 
table(is.na(full.data$Age)) # 263 are missing

# count missing Age data
full.data %>%
  filter(is.na(Age)) %>%
  nrow()

#replace the NA with the linear regression prediction of age

# boxplot of age data
boxplot(full.data$Age, col = "purple")

# boxplot stats to find upper whisker of plot
boxplot.stats(full.data$Age)

# create variable to hold upper whisker bound
upper_whisker <- boxplot.stats(full.data$Age)$stats[5]

# create dataframe of passengers with Age <= upper_whisker
filtered.age <- full.data$Age <= upper_whisker

# check summary stats of filtered data
summary(full.data[filtered.age, ]$Age)

# create age equation
age_equation = "Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked"

# build linear regression model
age.model <- lm(formula = age_equation, data = full.data[filtered.age,])

# filter for NA Age and columns needed for age model
age.predictor.columns <- full.data[is.na(full.data$Age), 
                                    c("Pclass", "Sex", 
                                     "SibSp", "Parch", 
                                     "Fare", "Embarked")]

# make age predictions
age_prediction <- predict(age.model, newdata = age.predictor.columns)

# assign age predictions
full.data[is.na(full.data$Age), "Age"] <- age_prediction

# check summary stats for age data
summary(full.data$Age)


# save minimum age for ages > 0
min_age <- full.data %>%
  filter(Age > 0) %>%
  summarize(min = min(Age))

# replace age values of 0 or less with min_age (0.17)
full.data[full.data$Age <= 0, "Age"] <- min_age

# check Age stats
summary(full.data$Age)

boxplot(full.data$Age)

# Plotting residuals against fitted values
plot(age.model$fitted.values, resid(age.model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual vs. Fitted Plot")
abline(h = 0, col = "red")  # Adds a horizontal line at 0



#if Fare column has any misisng value, then replace it with predicted value by linear regression
table(is.na(full.data$Fare))
upper.whisker <-boxplot.stats(full.data$Fare)$stat[5]
outlier.filter <- full.data$Fare < upper.whisker
full.data[outlier.filter, ]


fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Cabin + Embarked"
fare.model <- lm(formula = fare.equation, data = full.data[outlier.filter, ])

# Subset rows with missing values in "Fare" column and select relevant columns
fare.row <- full.data[is.na(full.data$Fare), c("Pclass", "Sex", "Age", "SibSp", "Parch", "Cabin", "Embarked")]

fare.prediction = predict(fare.model, newdata=fare.row)
full.data[is.na(full.data$Fare), "Fare"] <- fare.prediction
summary(full.data$Fare)

#summarize full.data to find any NA's
summary(full.data)


# create Survived equation for model building
survived_equation <- as.formula("Survived ~ Pclass + Sex + SibSp + Parch + 
    Fare + Embarked + Age")


# create dataframe of cleaned training set
clean.training <- full.data %>%
  filter(training == TRUE) %>%
  select(-c(Name, Ticket, Cabin, training))

# check for 891 rows and desired columns of clean training data
str(clean.training)

#convert int of Survived as factor
clean.training$Survived <- as.factor(clean.training$Survived)

# create dataframe of cleaned test data
clean.test <- full.data %>%
  filter(training == FALSE) %>%
  select(-c(Survived, Name, Ticket, Cabin, training))

# check for 418 rows of clean test data
str(clean.test)

# build logistic regression model with clean training data
log_reg <- glm(survived_equation, data = clean.training, family = "binomial")


# build bayes model with clean.training data
install.packages("naivebayes")
library(naivebayes)
bayes <- naive_bayes(survived_equation, data = clean.training)


# build decision tree model with clean.training data
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
# build classification tree model
decTree <- rpart(survived_equation, data = clean.training, 
                 method = "class")
rpart.plot(decTree, main="Decision Tree", extra=106)  # extra=106 shows split labels, fitted classes, and probabilities



# load randomForest library
library(randomForest)
rand_for <- randomForest(survived_equation, data = clean.training,
                         ntree = 500, mtry = 3)


# load class library
library(class)
# pull Survived values for training set
train.labels <- clean.training[ ,'Survived']

# create knn training dataframe with desired columns (not Survived)
knn.training <- clean.training %>%
  select(Pclass, Sex, SibSp, Parch, Fare, Embarked, Age)

# create dummy variables for factors
knn.training <- knn.training %>%
  # create our dummy variables for each factor type/level
  mutate(Male = as.integer(ifelse(Sex == 'male', 1, 0)),
         Female = as.integer(ifelse(Sex == 'female', 1, 0)),
         Cherbourg = as.integer(ifelse(Embarked == 'C', 1, 0)),
         Queenstown = as.integer(ifelse(Embarked == 'Q', 1, 0)),
         Southampton = as.integer(ifelse(Embarked == 'S', 1, 0)),
         FirstClass = as.integer(ifelse(Pclass == '1', 1, 0)),
         SecondClass = as.integer(ifelse(Pclass == '2', 1, 0)),
         ThirdClass = as.integer(ifelse(Pclass == '3', 1, 0))) %>%
  # select desired columns
  select(SibSp, Parch, Fare, Age, Male, Female, Cherbourg, Queenstown,
         Southampton, FirstClass, SecondClass, ThirdClass)

# check dataframe
head(knn.training)


# create knn test set data frame with desired features
knn.test <- clean.test %>%
  select(Pclass, Sex, SibSp, Parch, Fare, Embarked, Age)

# create dummy variables for factors
knn.test <- knn.test %>%
  # create dummy variable columns
  mutate(Male = as.integer(ifelse(Sex == 'male', 1, 0)),
         Female = as.integer(ifelse(Sex == 'female', 1, 0)),
         Cherbourg = as.integer(ifelse(Embarked == 'C', 1, 0)),
         Queenstown = as.integer(ifelse(Embarked == 'Q', 1, 0)),
         Southampton = as.integer(ifelse(Embarked == 'S', 1, 0)),
         FirstClass = as.integer(ifelse(Pclass == '1', 1, 0)),
         SecondClass = as.integer(ifelse(Pclass == '2', 1, 0)),
         ThirdClass = as.integer(ifelse(Pclass == '3', 1, 0))) %>%
  # select desired columns
  select(SibSp, Parch, Fare, Age, Male, Female, Cherbourg, Queenstown,
         Southampton, FirstClass, SecondClass, ThirdClass)

# check data
head(knn.test)


#Prediction

# percentage of survived vs not survived of training set
train.data %>%
  group_by(Survived) %>%
  count() %>%
  mutate(Prop = n / nrow(train.data))


# predict probability of survival with logistic regression model
log.reg.test <- clean.test %>%
  mutate(prob = predict(log_reg, clean.test, type = "response"),
         Survived = ifelse(prob > 0.50, 1, 0))

# check predictions
head(log.reg.test)

# create dataframe of PassengerID and Survived columns
log.reg.solutions <- log.reg.test %>%
  select(PassengerId, Survived)

#plotting
plot(log.reg.test$PassengerId, log.reg.test$Survived, type = 'l', col = 'blue', lwd = 2,
     xlab = 'PassengerId', ylab = 'Predicted Probability of Survival',
     main = 'Effect of Pclass on Survival Probability')


# write to csv for submission
write.csv(log.reg.solutions, file = "log_reg_solutions.csv", row.names = FALSE)


# create solutions data frame with predictions from bayes model
bayes.solutions <- clean.test %>%
  mutate(Survived = predict(bayes, clean.test))

# select the columns PassengerId and Survived for submission
bayes.solutions <- bayes.solutions %>%
  select(PassengerId, Survived)

# write to csv for submission
write.csv(bayes.solutions, file = "bayes_solutions.csv", row.names = FALSE)

#PLOTTING
plot(bayes.solutions$PassengerId, bayes.solutions$Survived, col = 'blue', pch = 19,
     xlab = 'PassengerId', ylab = 'Probability of Survival',
     main = 'Naive Bayes: Prob. of Survival by Age')



# make predictions with decision tree model
dec.tree.test <- clean.test %>%
  mutate(Survived = predict(decTree, clean.test, type = "class"))


# check Survived predictions
head(dec.tree.test)

# create dataframe of PassengerID and Survived columns
dec.tree.solutions <- dec.tree.test %>%
  select(PassengerId, Survived)

# write to csv for submission
write.csv(dec.tree.solutions, file = "dec_tree_solutions.csv", row.names = FALSE)

rpart.plot(decTree, main="Decision Tree", extra=106)

# make predictions with Random Forest model
rand.for.test <- clean.test %>%
  mutate(Survived = predict(rand_for, clean.test))

# check Survived predictions
head(rand.for.test)

# create data frame with PassengerId and Survived
rand.for.solutions <- rand.for.test %>%
  select(PassengerId, Survived)

# write to csv for submission
write.csv(rand.for.solutions, file = "rand_for_solutions.csv", row.names = FALSE)

# make predictions with knn()
knn.pred <- clean.test %>%
  mutate(Survived = knn(train = knn.training, test = knn.test, 
                        cl = train.labels, k = 30))

# create dataframe with just PassengerId and KNN predictions
knn.pred <- knn.pred %>%
  select(PassengerId, Survived)

# write to csv for submission
write.csv(knn.pred, file = "knn_solutions.csv", row.names = FALSE)


# Check Accuracy

# Read the CSV files
actual_data <- read.csv("gender_submission.csv")
model_output1 <- read.csv("log_reg_solutions.csv")
model_output2 <- read.csv("bayes_solutions.csv")
model_output3 <- read.csv("dec_tree_solutions.csv")
model_output4 <- read.csv("rand_for_solutions.csv")
model_output5 <- read.csv("knn_solutions.csv")



# Extract ground truth labels
actual_labels <- actual_data$Survived

# Extract predicted labels and PassengerId
predicted_data_log <- model_output1[, c("PassengerId", "Survived")]
predicted_data_bayes <- model_output2[, c("PassengerId", "Survived")]
predicted_data_dec_tree <- model_output3[, c("PassengerId", "Survived")]
predicted_data_rand_for <- model_output4[, c("PassengerId", "Survived")]
predicted_data_knn <- model_output5[, c("PassengerId", "Survived")]

# Merge predicted labels with ground truth labels based on PassengerId
merged_data_log <- merge(predicted_data_log, actual_data, by = "PassengerId")
merged_data_bayes <- merge(predicted_data_bayes, actual_data, by = "PassengerId")
merged_data_dec_tree <- merge(predicted_data_dec_tree, actual_data, by = "PassengerId")
merged_data_rand_for <- merge(predicted_data_rand_for, actual_data, by = "PassengerId")
merged_data_knn <- merge(predicted_data_knn, actual_data, by = "PassengerId")

# Calculate accuracy for logarithmic regression
correct_predictions <- sum(merged_data_log$Survived.x == merged_data_log$Survived.y )
total_predictions <- nrow(merged_data_log)
accuracy <- correct_predictions / total_predictions
print(paste("Accuracy:", round(accuracy * 100, 2), "%")) # 93.54%
conf_matrix_log <- table(merged_data_log$Survived.x, merged_data_log$Survived.y)
print(conf_matrix_log)

# Calculate recall and precision for each class
recall <- diag(conf_matrix_log) / rowSums(conf_matrix_log)
precision <- diag(conf_matrix_log) / colSums(conf_matrix_log)

# Print recall and precision
print(paste("Recall for class 0: ", recall[1]))
print(paste("Recall for class 1: ", recall[2]))
print(paste("Precision for class 0: ", precision[1]))
print(paste("Precision for class 1: ", precision[2]))


# Calculate accuracy from confusion matrix
accuracy_log <- sum(diag(conf_matrix_log)) / sum(conf_matrix_log)
print(paste("Accuracy:", round(accuracy_log, 2)))

install.packages("corrplot")
library(corrplot)

# Use corrplot
corrplot(conf_matrix_log, is.corr = FALSE, method = "color", type = "full", tl.pos = "d", 
         tl.col = "black", addCoef.col = "black")


# Calculate accuracy for Naive Bayes 
correct_predictions <- sum(merged_data_bayes$Survived.x == merged_data_bayes$Survived.y )
total_predictions <- nrow(merged_data_bayes)
accuracy <- correct_predictions / total_predictions
print(paste("Accuracy:", round(accuracy * 100, 2), "%")) # 80.14%
conf_matrix_bayes <- table(merged_data_bayes$Survived.x, merged_data_bayes$Survived.y)
print(conf_matrix_bayes)
# Calculate accuracy from confusion matrix
accuracy_bayes <- sum(diag(conf_matrix_bayes)) / sum(conf_matrix_bayes)
print(paste("Accuracy:", round(accuracy_bayes, 2))) # 0.8


# Calculate accuracy for Decision tree 
correct_predictions <- sum(merged_data_dec_tree$Survived.x == merged_data_dec_tree$Survived.y )
total_predictions <- nrow(merged_data_dec_tree)
accuracy <- correct_predictions / total_predictions
print(paste("Accuracy:", round(accuracy * 100, 2), "%")) # 91.87%


# Create confusion matrix
conf_matrix_dec_tree <- table(merged_data_dec_tree$Survived.x, merged_data_dec_tree$Survived.y)
print(conf_matrix_dec_tree)


# Calculate accuracy from confusion matrix
accuracy_dec_tree <- sum(diag(conf_matrix_dec_tree)) / sum(conf_matrix_dec_tree)
print(paste("Accuracy:", round(accuracy_dec_tree, 2))) # 0.92

corrplot(conf_matrix_dec_tree, is.corr = FALSE, method = "color", type = "full", tl.pos = "d", 
         tl.col = "black", addCoef.col = "black")


# Calculate accuracy for Random Forest 
correct_predictions <- sum(merged_data_rand_for$Survived.x == merged_data_rand_for$Survived.y )
total_predictions <- nrow(merged_data_rand_for)
accuracy <- correct_predictions / total_predictions
print(paste("Accuracy:", round(accuracy * 100, 2), "%")) # 63.88%
# Create confusion matrix
conf_matrix_ran_for <- table(merged_data_rand_for$Survived.x, merged_data_rand_for$Survived.y)
print(conf_matrix_ran_for)
# Calculate accuracy from confusion matrix
accuracy_ran_for <- sum(diag(conf_matrix_ran_for)) / sum(conf_matrix_ran_for)
print(paste("Accuracy:", round(accuracy_ran_for, 2))) # 0.64


# Calculate accuracy for KNN  
correct_predictions <- sum(merged_data_knn$Survived.x == merged_data_knn$Survived.y )
total_predictions <- nrow(merged_data_knn)
accuracy <- correct_predictions / total_predictions
print(paste("Accuracy:", round(accuracy * 100, 2), "%")) # 63.88%
# Create confusion matrix
conf_matrix_KNN <- table(merged_data_knn$Survived.x, merged_data_knn$Survived.y)
print(conf_matrix_KNN)
# Calculate accuracy from confusion matrix
accuracy_KNN <- sum(diag(conf_matrix_KNN)) / sum(conf_matrix_KNN)
print(paste("Accuracy:", round(accuracy_KNN, 2))) # 0.64


#Plotting bar graph with all model with their accuracy
# Create a data frame with the model names and their accuracies
model_data <- data.frame(
  Model = c("Logistic_Model", "Bayes_Model", "DecisionTree_Model", "RandomForestModel", "KNN_Model"),
  Accuracy = c(accuracy_log, accuracy_bayes, accuracy_dec_tree, accuracy_ran_for, accuracy_KNN)  # Example accuracies
)
# Creating a bar chart
accuracy_plot <- ggplot(model_data, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5, color = "black") +  # Use identity to use the actual accuracy values
  scale_fill_brewer(palette = "Set3") +  # Optional: Adds color to the bars
  labs(title = "Comparison of Model Accuracies", x = "Model", y = "Accuracy") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better readability

# Display the plot
print(accuracy_plot)












