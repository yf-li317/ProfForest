library(proftree)
library(EMP)
library(splitTools)
library(DMwR)
library(pROC)
library(doParallel)
registerDoParallel(cores=4)

data(churn)

# remove missing values
churn <- na.omit(churn)

# partition into training and testing set for telco_factor

tels.churn <- partition(churn$churn, p = c(train = 0.7, test = 0.3))
str(tels.churn)
train.churn <- churn[tels.churn$train, ]
#train.churn <- SMOTE(Churn ~ ., train.churn, perc.over = 100)
test.churn <- churn[tels.churn$test, ]

Lambda <- 0.35

list1 <- list()

foreach(i = 2020:2120, .combine = rbind) %dopar% {
  ProfTree.churn <- proftree(formula = churn ~ .,
                             data = train.churn,
                             control = proftree.control(lambda = Lambda,
                                                        seed = i,
                                                        verbose = TRUE))
  #plot(ProfTree.churn)
  #print(ProfTree.churn$info)
  
  scores.churn <- predict(ProfTree.churn, newdata = test.churn, type = "prob")[, 2]
  pred.churn <- predict(ProfTree.churn, newdata = test.churn, type = "response")
  
  result.churn <- empChurn(scores = scores.churn, classes = test.churn$churn)
  EMP.churn <- empChurn(scores = scores.churn, classes = test.churn$churn)$EMP
  #print(EMP.churn)
  #print(result.churn)
  
  # real churners of churn set
  churn_real <- subset(test.churn, churn == 1)
  
  #get the leadlist               
  scores.churn_new <- scores.churn[order(scores.churn, decreasing = TRUE)]
  numEntries = length(scores.churn_new) * result.churn$EMPfrac
  customer_base <- scores.churn_new[1:numEntries]
  row_num <- as.numeric(names(customer_base))
  
  leadList <- subset(churn[row_num,])
  
  #get the real churners in the leadlist
  churners <- subset(leadList, churn == 1)
  
  #defining the precision
  nprecision <- (dim(churners) / floor(numEntries))[1]
  
  #defining the recall
  nrecall <-(dim(churners) / dim(churn_real))[1]
  
  #defining F-1
  nF1 <- 2*((nprecision*nrecall)/(nprecision + nrecall))
  
  # AUC
  auc <- auc(pred.churn, as.numeric(test.churn$churn))
  
  list1 <- list(EMP.churn, nprecision, nrecall, nF1, auc)
  
  return(list1)
}