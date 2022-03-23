# import data
telco = read.csv(file.choose())
str(telco)
set.seed(2021)

# delete customerID
telco2 <- subset(telco, select = -customerID)

# remove missing values
telco.clean <- na.omit(telco2)

# as.factor
cols <- c('gender','SeniorCitizen','Partner', 'Dependents','PhoneService','PaperlessBilling','Churn','MultipleLines','OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies','InternetService','Contract','PaymentMethod')
telco.clean[cols] <- lapply(telco.clean[cols], as.factor)
str(telco.clean)
save(telco.clean, file = '/Users/yvonnelee/Desktop/telco_factor.RData')

# as.numeric
telco3 <- data.frame(telco.clean)
telco.numeric <- lapply(telco3, as.numeric)
telco.numeric <- data.frame(telco.numeric)
str(telco.numeric)
save(telco.numeric, file = '/Users/yvonnelee/Desktop/telco_numeric.RData')

# partition into training and testing set for telco_factor
library(splitTools)
tels.factor <- partition(telco.clean$Churn, p = c(train = 0.7, test = 0.3))
str(tels.factor)
train.factor <- telco.clean[tels.factor$train, ]
test.factor <- telco.clean[tels.factor$test, ]
save(train.factor, file = '/Users/yvonnelee/Desktop/train_factor.RData')
save(test.factor, file = '/Users/yvonnelee/Desktop/test_factor.RData')

# partition into training and testing set for telco_numeric
tels.numeric <- partition(telco.numeric$Churn, p = c(train = 0.7, test = 0.3))
str(tels.numeric)
train.numeric <- telco.numeric[tels.numeric$train, ]
test.numeric <- telco.numeric[tels.numeric$test, ]
save(train.numeric, file = '/Users/yvonnelee/Desktop/train_numeric.RData')
save(test.numeric, file = '/Users/yvonnelee/Desktop/test_numeric.RData')

# 5 replications of 2-fold cross validation
folds_whole_factor <- create_folds(telco.clean$Churn, k = 2, m_rep = 5)
folds_whole_numeric <- create_folds(telco.numeric$Churn, k = 2, m_rep = 5)
folds_factor <- create_folds(train.factor$Churn, k = 2, m_rep = 5)
folds_numeric <- create_folds(train.numeric$Churn, k = 2, m_rep = 5)


## grid search 
library(caret)
control <- trainControl(method = "repeatedcv",
                           ## 10-fold CV...
                           number = 2,
                           ## repeated ten times
                           repeats = 5)
grid <- expand.grid(Lambda <- c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00))

model <- train(Churn~., data=telco.clean, method="lvq", trControl=control, tuneGrid=grid)
print(model)

library(caTools)
library(kernlab)
library(e1071)

classifier = train(form = Churn ~ ., data = telco.clean, method = 'svmRadial')
classifier

## Set up the cross-validated hyper-parameter search
EVTREE_GRID= expand.grid(pmutatemajor=c(0.2,0.2), pmutateminor=c(0.2,0.2), pcrossover=c(0.2,0.2),
                         psplit=c(0.2,0.2), pprune=c(0.2,0.2), minbucket=c(5L,10L), minsplit=c(15L,25L),
                         maxdepth=c(5L,15L),alpha=c(1L,5L))



## Define a named list of parameter values
lambda <- as.data.frame(c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00))

mod <- function(...) {
  proftree(am ~ hp + mpg, data = train, control = rpart.control(...))
}




str(churn)

library(proftree)
library(EMP)

data(churn)

for (Lambda in c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00)){

 for (b in 1:5) {
  
  for (c in 0:1){
  ProfTree <- proftree(formula = Churn ~ .,
                     data = train.factor[1:4922, ],
                     control = proftree.control(lambda = Lambda,
                                                seed = 2020,
                                                verbose = TRUE))
  plot(ProfTree)
  print(ProfTree$info)

  scores <- predict(ProfTree, newdata = train.factor[1:4922, ], type = "prob")[, 2]

  EMP <- empChurn(scores = scores, classes = train.factor$Churn[1:4922])$EMP
  #print(EMP)
  print(EMP - Lambda * width(ProfTree))
  print(ProfTree$info$evalfun)
  }
 }
}













