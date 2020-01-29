library(tidyverse)
library(ggplot2)

#Load data from csv file
data1 <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv", header = TRUE, sep = ",")

#Problem 1
plotOne = lm(SalePrice ~ GarageArea,data = data1)
plot(SalePrice ~ GarageArea, data = data1,
     xlab = "Garage Size (in Square Feet)",
     ylab = "Sale Price (in Dollars)",
     main = "Garage Price vs Sale Price",
     pch  = 20,
     cex  = 1,
     col  = "grey")
abline(plotOne, lwd = 3, col = "darkorange")

#Problem 2
plotSecond = lm(SalePrice ~ Id+MSSubClass+MSZoning+LotFrontage+LotArea+
                  Alley+LotShape+LandContour+LotConfig+LandSlope+
                  Neighborhood+Condition1+BldgType+HouseStyle+OverallQual+
                  OverallCond+YearBuilt+YearRemodAdd+RoofStyle+Exterior1st+
                  Exterior2nd+MasVnrType+MasVnrArea+ExterQual+ExterCond+Foundation+
                  BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinSF1+
                  BsmtFinType2+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+Heating+HeatingQC+
                  CentralAir+Electrical+LowQualFinSF+GrLivArea+BsmtFullBath+
                  BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+
                  KitchenQual+TotRmsAbvGrd+Functional+Fireplaces+
                  GarageType+GarageYrBlt+GarageFinish+GarageCars+GarageArea+
                  GarageQual+GarageCond+PavedDrive+WoodDeckSF+OpenPorchSF+
                  EnclosedPorch+MiscVal+MoSold+YrSold+SaleType+
                  SaleCondition,data = data1)
summary(plotSecond)
##There are some relationships between predicators and responses
##There are stronger relationships between Sale Price and (Lot Area, 
#1st floor square feet, and Garage Size, to name a few)
##The coefficient for the year suggests that there is a strong 
#correlation between the year the house was built and the sale price

#Problem 3
graphOne <- plot(SalePrice ~ LotArea, data = data1,
                 xlab = "Lot Area (in Square Feet)",
                 ylab = "Sale Price (in Dollars)",
                 pch  = 20,
                 cex  = 1,
                 col  = "pink")

##There are several lot area points that are severe outliers and is 
#causing the graph to skew

graphTwo <- plot(SalePrice ~ GrLivArea, data = data1,
                 xlab = "Living Room Area (in Square Feet)",
                 ylab = "Sale Price (in Dollars)",
                 pch  = 20,
                 cex  = 1,
                 col  = "blue")

## There is a very strong correlation between living room area and price

graphThree <- plot(SalePrice ~ TotalBsmtSF, data = data1,
                   xlab = "Living Room Area (in Square Feet)",
                   ylab = "Sale Price (in Dollars)",
                   pch  = 20,
                   cex  = 1,
                   col  = "green")
##There is a strong connection between total basement square footage and
#the sale price

#problem 4

lm1<- lm(SalePrice ~  GarageArea*LotArea*GrLivArea, data = data1)
summary(lm1)

lm2<- lm(SalePrice ~  GrLivArea+GarageArea:LotArea, data = data1)
summary(lm2)
plot(lm1)

# They have the significant interactions
#problem 5

lm3<- lm(SalePrice ~ sqrt(GarageArea)+(LotArea)^2, data = data1)
summary(lm3)
plot(lm3)

