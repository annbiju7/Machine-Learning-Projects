# QUESTION 4A

library(boot)

data <- data.frame(
  id = c(1:10),
  homeowner = c("yes", "no", "yes", "no", "yes", "no", "no", "no", "no", "yes"),
  refund = c("t", "t", "f", "f", "f", "f", "t", "t", "t", "t")
)

boot_fun <- function(data, index) {
  return(data[index, ])
}

set.seed(2)

boot_samples <- list()
for (i in 1:10) {
  boot_samples[[i]] <- boot(data, boot_fun, R = 1)$t
}

boot_samples[[1]]

# QUESTION 6

data <- read.csv("C:/ann/spring 2023/cs 4375/AXB190082-HW4/heart1.csv")

# cp = 0.003197442:
# Average testing error: 0.2487
# Average accuracy: 0.7513

# cp = 0.006705747:
# Average testing error: 0.2533
# Average accuracy: 0.7467

# cp = 0.036903810:
# Average testing error: 0.2754
# Average accuracy: 0.7246

# cp = 0.064481748:
# Average testing error: 0.2745
# Average accuracy: 0.7255

# cp = 0.128497202:
# Average testing error: 0.2919
# Average accuracy: 0.7081

# Based on the results, the decision tree with cp = 0.003197442 has the lowest 
# average testing error and the highest average accuracy. So it's the best model 
# for predicting heart disease. The smaller cp value provides better 
# generalization performance by preventing overfitting on the training data, and
# thus improving the model's ability to generalize unseen data.

# Question 7

library(caret)

knn_model <- train(HeartDisease ~ ., data = data, method = "knn", 
                   trControl = trainControl(method = "cv", number = 10))
var_imp <- varImp(knn_model)
 
head(var_imp$importance, 3)

# Question 8

data$Sex <- ifelse(data$Sex == "M", 1, 2)

data$ChestPainType <- ifelse(data$ChestPainType == "ATA", 1, 0)
data$ChestPainType <- ifelse(data$ChestPainType == "NAP", 2, 0)
data$ChestPainType <- ifelse(data$ChestPainType == "ASY", 3, 0)

data$RestingECG <- ifelse(data$RestingECG == "ST", 1, 0)
data$RestingECG <- ifelse(data$RestingECG == "LVH", 2, 0)

data$ExerciseAngina <- ifelse(data$ExerciseAngina == "Y", 1, 0)

data$ST_Slope <- ifelse(data$ST_Slope == "Up", 1, 0)

View(data)

data.scaled <- scale(data[1:918, ])
summary(data.scaled)

# Question 9

set.seed(1)
trainIndex <- createDataPartition(data$HeartDisease, p = 0.8, list = FALSE)
training <- data[trainIndex,]
testing <- data[-trainIndex,]

knn_model <- train(HeartDisease ~ ., data = training, method = "knn", 
                   trControl = trainControl(method = "cv", number = 10))

plot(knn_model)
knn_model
which.max(knn_model$results$Accuracy)

# K = 9 was used for the model since it has the highest accuracy in predicting
# if a patient has HeartDisease