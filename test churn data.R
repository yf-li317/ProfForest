# import data
data(churn)

# remove missing values
telco.clean <- na.omit(churn)

# as.factor
cols <- c('region','prod_num','autopay','churn')
telco.clean[cols] <- lapply(telco.clean[cols], as.factor)
str(telco.clean)

# partition into training and testing set for telco_factor
library(splitTools)
tels.factor <- partition(telco.clean$churn, p = c(train = 0.7, test = 0.3))
str(tels.factor)
train.factor <- telco.clean[tels.factor$train, ]
test.factor <- telco.clean[tels.factor$test, ]
#save(train.factor, file = '/Users/yvonnelee/Desktop/train_factor.RData')
#save(test.factor, file = '/Users/yvonnelee/Desktop/test_factor.RData')

#library(caret)

# create CV function
CVgroup <- function (k, datasize, seed){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k, ceiling(datasize/k))[1:datasize]
  temp <- sample (n,datasize)
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x, function(x) dataseq[temp==x])
  return(cvlist)
}

library(plyr)
library(proftree)
library(EMP)

# lambda grid
 L <- c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00)
#L <- c(0.01,0.05)

# 5*2 folds

 result <- matrix(NA, nrow = 21, ncol = 2)
 
for (Lambda in L) { 
  
  rep5 <- matrix(NA, nrow = 5, ncol = 2)
  
  for (j in 1:5) { 
    data <- train.factor
    k = 2
    cvlist <- CVgroup (k, nrow(data), seed=j)
    metric <- vector("numeric")
    for(i in 1:2){
      train <- data[-cvlist[[i]],]
      test <- data[cvlist[[i]],]
      ProfTree <- proftree(formula = churn ~ ., #fit model
                           data = train[1:217, ],
                           control = proftree.control(lambda = Lambda,
                                                      seed = 2020,
                                                      verbose = TRUE))
      scores <- predict(ProfTree, newdata = test[1:217, ], type = "prob")[, 2] # evaluate model
      EMP <- empChurn(scores = scores, classes = test$churn[1:217])$EMP
      #print(EMP - Lambda * width(ProfTree))
      metric[i] <-  EMP - Lambda * width(ProfTree)
      rep5[j,i] <- metric[i]
    }
  }
  rmean <- rowMeans(rep5)
  avg_rep5 <- cbind(rep5,rmean)
  avg_lambda <- colMeans(avg_rep5)[3]
  cat("Lambda =", Lambda, " Average EMPC value of the 5*2 CV =", avg_lambda)
}
 
 
 
 
 