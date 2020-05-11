# Import Data #
movie <- read.csv("E:/My Dictionary/Using R/Data/Movie_regression.csv")
View(movie)

# Data Preprocessing #
summary(movie) #there are missing values in variable Time_taken
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE)

# Test-Train Split
install.packages('caTools')
library(caTools)
set.seed(0)
split <- sample.split(movie,SplitRatio = 0.8)
train <- subset(movie,split == TRUE)
test <- subset(movie,split == FALSE)

############################### MODELING #################################
install.packages("gbm")
library(gbm)
set.seed(0)
boosting <- gbm(Collection~., data=train, distribution="gaussian", n.trees=5000, interaction.depth = 4, shrinkage = 0.2, verbose = FALSE)
test$gb <- predict(boosting, test, n.trees=5000)
RMSE <- sqrt(mean((test$gb-test$Collection)^2))
RMSE
