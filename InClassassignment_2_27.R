library(rpart)
library(rpart.plot)
ames <- read.table("http://msudataanalytics.github.io/SSC442/Labs/data/ames.csv", 
                   header = TRUE,
                   sep = ",")
table(ames$Fireplaces)
tree_fireplaces = rpart(Fireplaces ~., data = ames, method = 'class')
rpart.plot(tree_fireplaces)
confusionMatrix(ames, Fireplaces)
