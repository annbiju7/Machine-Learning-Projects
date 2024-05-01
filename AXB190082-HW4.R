# Question 6

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
install.packages("caret")
install.packages("randomForest")
library(caret)
library(randomForest)

# load the HeartDisease dataset
data < - read.csv("c:/ann/spring 2023/cs 4375/AXB190082-HW4/heart1")

# split the data into training and testing sets
set.seed(1)
trainIndex <- createDataPartition(, p = 0.8, list = FALSE)
train <- HeartDisease[trainIndex, ]
test <- HeartDisease[-trainIndex, ]

# train a random forest model
set.seed(456)
rf_model <- randomForest(HeartDisease ~ ., data = train, ntree = 1000)

# generate a feature importance plot
varImpPlot(rf_model, sort = TRUE, n.var = 3)
