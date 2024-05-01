# Ann Biju
# axb190082
# CS 4375.002

setwd("C:/ann")

# 1a

heart <- read.csv("C:/ann/AXB190082-HW3/heart1.csv")

install.packages("rpart")
library(rpart)

set.seed(1)
hd.true <- heart$HeartDisease == 1
hd.false <- heart$HeartDisease == 0

m <- rpart(hd.true ~ hd.false, data = heart, method = "class")
m

# 1b

# Target objects have binary values of 0 and 1.

# 1c

# Predict classes for each object
pred <- predict(m, heart, type = "class")
pred

# Calculate confusion matrix
cm <- table(pred, heart$HeartDisease)
cm

# Calculate TPR, FPR, TNR, FNR, and accuracy
TPR <- cm[2, 2] / sum(heart$HeartDisease == 1)
TPR 

FPR <- cm[2, 1] / sum(heart$HeartDisease == 0)
FPR

TNR <- cm[1, 1] / sum(heart$HeartDisease == 0)
TNR

FNR <- cm[1, 2] / sum(heart$HeartDisease == 1)
FNR

accuracy <- sum(diag(cm)) / sum(cm)
accuracy

# 1d

# Predict classes for first ten objects
pred10 <- predict(m, heart[1:10, ], type = "class")

for (i in 1:10) {
  cat(sprintf("%6s   %6s\n", heart$HeartDisease[i], pred10[i]))
}

error.rate <- sum(heart$HeartDisease[1:10] != pred10) / length(pred10)
error.rate

# 1e

set.seed(1)
train.i <- sample(nrow(heart), 0.7 * nrow(heart))
train <- heart[train.i, ]
test <- heart[-train.i, ]

# Fit decision tree on training set
tree <- rpart(HeartDisease ~ ., data = train)
tree 

# Predict classes for training and testing sets
train.pred <- predict(tree, heart[train.i, ], type = "class")
test.pred <- predict(tree, test, type = "class")

# Calculate training and testing errors
train.error <- sum(train$HeartDisease != train.pred) / nrow(train)
test.error <- sum(test$HeartDisease != test.pred) / nrow(test)

# pessimistic approach
pessimistic.error <- test.error + sqrt(log(2 / 0.05) / (2 * nrow(test)))

# optimistic approach
optimistic.error <- test.error - sqrt(log(2 / 0.05) / (2 * nrow(test)))

# 2

t <- rpart(hd.true ~ hd.false, data = heart, control = rpart.control(minsplit =
                                                                       2, cp = 
                                                                       0))
t

# 2a

pred <- predict(t, heart, type = "class")

# Calculate true positive (TP), false positive (FP), true negative (TN), false 
# negative (FN)
TP <- sum(pred == 1 & heart$HeartDisease == 1)
FP <- sum(pred == 1 & heart$HeartDisease == 0)
TN <- sum(pred == 0 & heart$HeartDisease == 0)
FN <- sum(pred == 0 & heart$HeartDisease == 1)

TPR <- TP / (TP + FN)
TPR

FPR <- FP / (FP + TN)
FPR

TNR <- TN / (TN + FP)
TNR

FNR <- FN / (TP + FN)
FNR

accuracy <- (TP + TN) / nrow(heart)
accuracy

# 2b

set.seed(1)
train <- sample(nrow(heart), nrow(heart) * 0.7)
test <- setdiff(1:nrow(heart), train)

# Fit decision tree on training set
tree <- rpart(HeartDisease ~ ., data = heart[train,])

# Predict classes for testing set
pred <- predict(tree, heart[test,], type = "class")

# Calculate error rate on training set
train_error <- sum(predict(tree, heart[train,], type = "class") != 
                     heart$HeartDisease[train]) / nrow(heart[train])

# Calculate error rate on testing set
test_error <- sum(pred != heart$HeartDisease[test]) / nrow(heart[test])

# Calculate generalization error using pessimistic approach
z <- 1.96 # 95% confidence
gen_error_pessimistic <- train_error + z * sqrt(train_error * (1 - train_error)
                                                / nrow(heart[test]))

# Print results
cat("Training error:", train_error, "\n")
cat("Testing error:", test_error, "\n")
cat("Generalization error (pessimistic approach):", gen_error_pessimistic)

# 2c

# The default tree has a higher error rate, since it has not been fully grown 
# with correct data points.

# 3

# Underfitting in machine learning is when a model is too simple or has not been
# trained enough to capture the complexity of the data. In such a case, the 
# model may fail to generalize well on new data, including both the training and
# testing data.

# 4

x.train <- train_test_split(X, y, test.size=1/3, random.state=42)
x.test <- train_test_split(X, y, test.size=1/3, random.state=42)
y.train <- train_test_split(X, y, test.size=1/3, random.state=42)
y.test <- train_test_split(X, y, test.size=1/3, random.state=42) 

clf = DecisionTreeClassifier(random.state=42)
clf.fit(x.train, y.train)

x.train.small <- train.test.split(X, y, train_size=50, random_state=42)
x.test.small <- train.test.split(X, y, train_size=50, random_state=42)
y.train.small <- train.test.split(X, y, train_size=50, random_state=42)
y.test.small <- train.test.split(X, y, train_size=50, random_state=42)

train_error = 1 - clf.score(x.train, y.train)
test_error = 1 - clf.score(x.test, y.test)

train.error.small = 1 - clf.small.score(x.train.small, y.train.small)
test.error.small = 1 - clf.small.score(x.test.small, y.test.small)

# 5

set.seed(1)

# Shuffle the data
data <- heart[sample(nrow(heart)),]

# Create the K-fold object
kf <- cut(seq(1, nrow(data)), breaks = 10, labels = FALSE)

# Initialize the vectors to store the errors
train_errors <- c()
test_errors <- c()

# Loop over the folds
for (i in 1:10) {
  # Get the training and testing data
  test.indexes <- which(kf == i)
  test.data <- data[test.indexes, ]
  train.data <- data[-test.indexes, ]
  
  # Build the decision tree using the training data
  tree <- rpart(target ~ ., data = train.data, minsplit = 2)
  
  # Calculate the training error
  y.pred.train <- predict(tree, train.data, type = "class")
  train.error <- 1 - mean(y.pred.train == train.data$target)
  train.errors <- c(train.errors, train.error)
  
  # Calculate the testing error
  y.pred.test <- predict(tree, test.data, type = "class")
  test.error <- 1 - mean(y.pred.test == test.data$target)
  test.errors <- c(test.errors, test.error)
}

# Calculate the average testing error
avg.test.error <- mean(test.errors)






