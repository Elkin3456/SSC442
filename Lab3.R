#Loads in libraries and data with propper columns
library(dplyr)
library(tidyverse)
library(caret)
install.packages("leaps")
library(leaps)
install.packages("MASs")
library(MASS)
f <- file.choose()
ames <- read.csv(f)
df <- ames[ -c(18:19) ]
begin <- lm(SalePrice ~ 1,data= df)




models <- step(begin,direction = "forward", scope = ~GrLivArea+ FullBath+YearBuilt+ YearRemodAdd+BedroomAbvGr+Fireplaces+GarageCars+MSSubClass+ Neighborhood+ HouseStyle+ RoofStyle+ Foundation+ExterQual+HeatingQC+CentralAir)
#anova(models)

fit<-lm(SalePrice ~ Neighborhood,data = df)
fit1<-lm(SalePrice ~ Neighborhood + GrLivArea, data = df)
fit2<-lm(SalePrice ~ Neighborhood + GrLivArea + ExterQual,data = df)
fit3<-lm(SalePrice ~ Neighborhood + GrLivArea + ExterQual + MSSubClass,data=df)
fit4<-lm(SalePrice ~ Neighborhood + GrLivArea + ExterQual + MSSubClass + 
  GarageCars,data=df)
fit5<-lm(SalePrice ~ Neighborhood + GrLivArea + ExterQual + MSSubClass + 
  GarageCars + Fireplaces,data=df)

fit6<-lm(SalePrice ~ Neighborhood + GrLivArea + ExterQual + MSSubClass + 
  GarageCars + Fireplaces + YearRemodAdd,data=df)
fit7<-lm(SalePrice ~ Neighborhood + GrLivArea + ExterQual + MSSubClass + 
  GarageCars + Fireplaces + YearRemodAdd + HouseStyle,data=df)
fit8<-lm(SalePrice ~ Neighborhood + GrLivArea + ExterQual + MSSubClass + 
  GarageCars + Fireplaces + YearRemodAdd + HouseStyle + Foundation,data=df)

fit9<-lm(SalePrice ~ Neighborhood + GrLivArea + ExterQual + MSSubClass + 
  GarageCars + Fireplaces + YearRemodAdd + HouseStyle + Foundation + 
  YearBuilt,data=df)
fit10<-lm(SalePrice ~ Neighborhood + GrLivArea + ExterQual + MSSubClass + 
  GarageCars + Fireplaces + YearRemodAdd + HouseStyle + Foundation + 
  YearBuilt + BedroomAbvGr,data=df)

get_rmse(fit,df,SalePrice)
get_rmse(fit1,df,SalePrice)
get_rmse(fit2,df,SalePrice)
get_rmse(fit3,df,SalePrice)
get_rmse(fit4,df,SalePrice) 
get_rmse(fit5,df,SalePrice)
get_rmse(fit6,df,SalePrice)
get_rmse(fit7,df,SalePrice)
get_rmse(fit8,df,SalePrice)
get_rmse(fit9,df,SalePrice)
get_rmse(fit10,df,SalePrice)



set.seed(9)
num_obs = nrow(df)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = df[train_index, ]
test_data = df[-train_index, ]

#get_complexity(begin)
# train RMSE
sqrt(mean((train_data$SalePrice - predict(begin, train_data)) ^ 2))
# test RMSE
sqrt(mean((test_data$SalePrice - predict(begin, test_data)) ^ 2))

get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}
