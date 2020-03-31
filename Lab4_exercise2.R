library(caret)
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
f <- choose.files()
Bank <- read.csv(f)
#1
#creates train test split with 75% used for training the data
set.seed(36)
bank_idx = createDataPartition(Bank$y, p = 0.75, list = FALSE)
bank_trn = Bank[bank_idx, ]
bank_tst = Bank[-bank_idx, ]
#2
#runs logistic regression with 10 fold cross validation
install.packages('e1071')
bank_glm_mod = train(
  form = y ~ .,
  data = bank_trn,
  trControl = trainControl(method = "cv", number = 10),
  method = "glm",
  family = "binomial"
)
#bank_glm_mod
#bank_glm_mod$results
#bank_glm_mod$finalModel
#3
#prints out coefficients and related values for gaugin the coefficents relevance
summary(bank_glm_mod)
#4
#creates confusion matrix on test data
confusionMatrix(bank_tst,predict(bank_tst$y,bank_glm_mod),mode = "sens_spec")