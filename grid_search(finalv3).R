library(plyr)
library(proftree)
library(EMP)
library(splitTools)

# import data
telco = read.csv(file.choose())
str(telco)

# delete customerID
telco2 <- subset(telco, select = -customerID)

# remove missing values
telco.clean <- na.omit(telco2)

# as.factor
cols <- c('gender','SeniorCitizen','Partner', 'Dependents','PhoneService','PaperlessBilling','Churn','MultipleLines','OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies','InternetService','Contract','PaymentMethod')
telco.clean[cols] <- lapply(telco.clean[cols], as.factor)
str(telco.clean)

# partition into training and testing set
tels <- partition(telco.clean$Churn, p = c(train = 0.7, test = 0.3))
str(tels)
train <- telco.clean[tels$train, ]
test <- telco.clean[tels$test, ]

# lambda grid
L <- c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00)
#L <- c(0.01, 0.05, 0.10, 0.15, 0.20)
finalMatrix <- matrix(NA, nrow = 2, ncol = 21)
iteration = 1

# 5*2 folds
for (Lambda in L) {
  rep5 <- matrix(NA, nrow = 2, ncol = 5)
  for (j in 1:5) { 
    folds <- create_folds(train$Churn, k = 2)
    metric <- vector("numeric")
    for(i in 1:2){
      train_lambda <- train[folds[[i]], ]
      test_lambda <- train[-folds[[i]], ]
      ProfTree <- proftree(formula = Churn ~ ., #fit model
                           data = train_lambda[1:2461, ],
                           control = proftree.control(lambda = Lambda,
                                                      seed = 2020,
                                                      verbose = TRUE))
      scores <- predict(ProfTree, newdata = test_lambda[1:2461, ], type = "prob")[, 2] # evaluate model
      EMP <- empChurn(scores = scores, classes = test_lambda$Churn[1:2461])$EMP
      print(EMP)
      metric[i] <- EMP
      rep5[i, j] <- metric[i]
    }
  }
  cmean <- colMeans(rep5)
  avg_rep5 <- rbind(rep5,cmean)
  avg_lambda <- rowMeans(avg_rep5)[3]
  finalMatrix [1, iteration] <- Lambda
  finalMatrix [2, iteration] <- avg_lambda
  iteration <- iteration + 1
}
print(finalMatrix)

#metric
#rep5
#avg_rep5
#avg_lambda





