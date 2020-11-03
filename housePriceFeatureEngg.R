train <- read.csv("C:/Shreya Sharma/Machine Learning/Data csvs/house-prices-advanced-regression-techniques/train.csv", stringsAsFactors = F)
test <- read.csv("C:/Shreya Sharma/Machine Learning/Data csvs/house-prices-advanced-regression-techniques/test.csv", stringsAsFactors = F)

str(train)
dim(train)
str(train[,c(1:10 , 81)])
?str

?read.csv

test_labels <- test$Id
train$Id <- NULL
test$Id <- NULL
test$SalePrice <- NA

all <- rbind(train, test)
dim(all)

library(ggplot2)
# install.packages('knitr', repos = c('http://rforge.net', 'http://cran.rstudio.org'),type = 'source')
library(plyr)
# install.packages("dplyr")
library(dplyr)
# install.packages("corrplot")
library(corrplot)
# install.packages("caret")
library(caret)
# install.packages("gridExtra")
library(gridExtra)
library(scales)
# install.packages("Rmisc")
library(Rmisc)
# install.packages("ggrepel")
library(ggrepel)
# install.packages("kableExtra")
# library(knitr)
 library(kableExtra)
# install.packages("randomForest")
library(randomForest)
# install.packages("moments")
library(moments)
# install.packages("xgboost")
library(xgboost)

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000),labels = comma)

summary(all$SalePrice)

## bifurcate numeric and non-numeric variables

numericVars <- which(sapply(all,is.numeric))
numericVarNames <- names(numericVars) # storing names of numeric variables
numericVarNames
all_numVar <- all[,numericVars]
all_numVar
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs")
cor_numVar
#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
CorHigh
cor_numVar <- cor_numVar[CorHigh, CorHigh]

cor_numVar
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#relation of numeric variables with slaes price

ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)


ggplot(data=all[!is.na(all$SalePrice),], aes(x=GrLivArea, y=SalePrice)) +
  geom_point(col='blue')+ geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) + 
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500,
  rownames(all), '')))

# identified outliers as 524 and 1299 but there overall qualtiy was 10 hence greater sales price
# need to further refine the data before removing the outlier
all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]

# Identifying variables having NA values

NAcol <- which(colSums(is.na(all)) > 0)
NAcol[47]
names(all[47])
class(NAcol)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)

all$PoolQC[is.na(all$PoolQC)] <- 'None'

Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

# converting the ordinal values as vectors by using as.integer

# PoolQc
all$PoolQC<-as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)
summary(all$PoolQC)
all[all$PoolArea>0 & all$PoolQC==0, c('PoolArea', 'PoolQC', 'OverallQual')]

all$PoolQC[2421] <- 2
all$PoolQC[2504] <- 3
all$PoolQC[2600] <- 2

# converting the nominal values as factors using as.factor

## Misc ##
all$MiscFeature[is.na(all$MiscFeature)] <- 'None'
all$MiscFeature <- as.factor(all$MiscFeature)

summary(all$MiscFeature)


ggplot(all[!is.na(all$SalePrice),], aes(x=MiscFeature, y=SalePrice))+
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
table(all$MiscFeature)

## Alley

all$Alley[is.na(all$Alley)] <- 'None'
all$Alley <- as.factor(all$Alley)
ggplot(all[!is.na(all$SalePrice),], aes(x=Alley, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma)

table(all$Alley)

## Fence ##
## Note at first the fence variable seems to be ordinal ##
all$Fence[is.na(all$Fence)] <- 'None'
table(all$Fence)
all[!is.na(all$SalePrice),] %>% group_by(Fence) %>% 
  summarise(median = median(SalePrice), counts=n())
## this data shows that no fence is good ##
## Hence the data is nominal therefore we will convert it into factor ##

all$Fence <- as.factor(all$Fence)
table(all$Fence)


## FirePlace ##

all$FireplaceQu[is.na(all$FireplaceQu)] <- 'None'
all$FireplaceQu <- as.integer(revalue(all$FireplaceQu,Qualities))
table(all$FireplaceQu)
sum(table(all$Fireplaces))

## LotVariables contain 3 variables ## 
## out of which 1 has 486 NA's and the rest 2 do not have any missing values ##

summary(all$LotFrontage)
str(all$LotFrontage)
table(all$LotFrontage)

ggplot(all[!is.na(all$LotFrontage),], aes(x=as.factor(Neighborhood), y=LotFrontage)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

for (i in 1:nrow(all)){
  if(is.na(all$LotFrontage[i])){
    all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood==all$Neighborhood[i]], na.rm=TRUE)) 
  }
}

## LotShape ##
all$LotShape<-as.integer(revalue(all$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
table(all$LotShape)
sum(table(all$LotShape))

## Lot Config ##
 ## data seemed to be ordinal ##
ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(LotConfig), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

## On visualsing the data, it is nominal -> converting it to factor ##
all$LotConfig <- as.factor(all$LotConfig)
table(all$LotConfig)
sum(table(all$LotConfig))

## Garage Variables ##
## Two of those have one NA (GarageCars and GarageArea) ##
## one has 157 NAs (GarageType), 4 variables have 159 NAs ##

str(all$GarageYrBlt)
summary(all$GarageYrBlt)
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]

#check if all 157 NAs are the same observations among the variables with 157/159 NAs
length(which(is.na(all$GarageType) &
    is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))

#Find the 2 additional NAs
kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish),
c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])


#Imputing modes.
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]

kable(all[2127, 
c('GarageYrBlt', 'GarageCars', 'GarageArea', 'GarageType',
  'GarageCond', 'GarageQual', 'GarageFinish')])

all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA

#check if NAs of the character variables are now all 158
length(which(is.na(all$GarageType) 
& is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))

## Garage Type ##
## Nominal --> factor ##

all$GarageType[is.na(all$GarageType)] <- 'No Garage'
all$GarageType <- as.factor(all$GarageType)
table (all$GarageType)

## Garage Finish ##
## ordinal -> vector ##

all$GarageFinish[is.na(all$GarageFinish)] <- 'None'
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
all$GarageFinish<-as.integer(revalue(all$GarageFinish, Finish))
table(all$GarageFinish)

## Garage Quality ##
## ordinal -> vector ##

all$GarageQual[is.na(all$GarageQual)] <- 'None'
all$GarageQual<-as.integer(revalue(all$GarageQual, Qualities))
table(all$GarageQual)


## Garage Condition ##
## ordinal -> vector ##
all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond<-as.integer(revalue(all$GarageCond, Qualities))
table(all$GarageCond)

### BaseMent Type -> 11 variables ###
## Five of those have 79-82 NAs, six have one or two NAs. ##
#check if all 79 NAs are the same observations among the variables with 80+ NAs
length(which(is.na(all$BsmtQual) & is.na(all$BsmtCond) 
  & is.na(all$BsmtExposure) & is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2)))


#Find the additional NAs; BsmtFinType1 is the one with 79 NAs
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond)|is.na(all$BsmtQual)
  |is.na(all$BsmtExposure)|is.na(all$BsmtFinType2)), 
c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]


## imputing mose values to houses with incomplete basement observations to fix data ##

#Imputing modes.
all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtQual[c(2218, 2219)] <- names(sort(-table(all$BsmtQual)))[1]

## Basement Quality ##
## ordinal -> vector ##
all$BsmtQual[is.na(all$BsmtQual)] <- 'None'
all$BsmtQual<-as.integer(revalue(all$BsmtQual, Qualities))
table(all$BsmtQual)

## Basement Condition ##
## ordinal -> vector ##

all$BsmtCond[is.na(all$BsmtCond)] <- 'None'
all$BsmtCond<-as.integer(revalue(all$BsmtCond, Qualities))
table(all$BsmtCond)

## Basement Exxposure ##
## ordinal -> vector ##

all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
all$BsmtExposure<-as.integer(revalue(all$BsmtExposure, Exposure))
table(all$BsmtExposure)

## Basement Finish type 1 ##
## ordinal -> vector ##
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1, FinType))
table(all$BsmtFinType1)

## Basement Finish Type 2 ##
## ordinal -> vector ##

all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2, FinType))
table(all$BsmtFinType2)

## remaining Basement vriable  ##

#display remaining NAs. Using BsmtQual as a reference for the 79 houses without basement agreed upon earlier
all[(is.na(all$BsmtFullBath)|is.na(all$BsmtHalfBath)|is.na(all$BsmtFinSF1)
     |is.na(all$BsmtFinSF2)|is.na(all$BsmtUnfSF)|is.na(all$TotalBsmtSF)),
    c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 
    'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]
all$BsmtFullBath[is.na(all$BsmtFullBath)] <-0
table(all$BsmtFullBath)

all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <-0
table(all$BsmtHalfBath)

all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <-0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <-0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <-0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <-0


### Masonry veneer ###
##  24 NAs. Masonry veneer area has 23 NAs ##

#check if the 23 houses with veneer area NA are also NA in the veneer type
length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))

#find the one that should have a MasVnrType
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]

#fix this veneer type by imputing the mode
all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2] #taking the 2nd value as the 1st is 'none'
all[2611, c('MasVnrType', 'MasVnrArea')]

## Masonry type ##
## check for ordinal as we are not sure ##

all$MasVnrType[is.na(all$MasVnrType)] <- 'None'
all[!is.na(all$SalePrice),] %>% group_by(MasVnrType) %>% summarise(median = median(SalePrice), counts=n()) %>% arrange(median)

## seems ordinal hence vector ##

Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
all$MasVnrType<-as.integer(revalue(all$MasVnrType, Masonry))
table(all$MasVnrType)

## masonry veener area ##
all$MasVnrArea[is.na(all$MasVnrArea)] <-0


### MS zoning ###
## 4 NAs. Values are categorical. ##

#imputing the mode
all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
all$MSZoning <- as.factor(all$MSZoning)
table(all$MSZoning)

sum(table(all$MSZoning))

### Kitchen Quality ###
## 1 Na . Ordinal -> vector ##

all$KitchenQual[is.na(all$KitchenQual)] <- 'TA' #replace with most common value
all$KitchenQual<-as.integer(revalue(all$KitchenQual, Qualities))
table(all$KitchenQual)

sum(table(all$KitchenQual))

## number of kitchens above grade ##

table(all$KitchenAbvGr)
sum(table(all$KitchenAbvGr))


### Utilities ###
## 2 NA values ##

table(all$Utilities)
all$Utilities <- NULL

## not good for prediction as the test data has all the utilites as AllPub ##

### Home Functionality ###
## 1 NA, ordinal -> vector ##

all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]
all$Functional <- as.integer(revalue(all$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(all$Functional)

sum(table(all$Functional))


### Exteriors Variable ###
## 2 variables have 1 NA, 2 variables have no NAs ##

##  Exterior covering on house ##
## 1NA categorical values -> factors ##
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]
all$Exterior1st <- as.factor(all$Exterior1st)
table(all$Exterior1st)
sum(table(all$Exterior1st))

## Exterior 2nd ## 
## categorical -> factor ##

#imputing mode
all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]
all$Exterior2nd <- as.factor(all$Exterior2nd)
table(all$Exterior2nd)

sum(table(all$Exterior2nd))

## Exterior Quality ##
## ordinal -> vector ##

all$ExterQual<-as.integer(revalue(all$ExterQual, Qualities))
table(all$ExterQual)
sum(table(all$ExterQual))


## Exterior consition ##
## ordinal -> vector ##

all$ExterCond<-as.integer(revalue(all$ExterCond, Qualities))
table(all$ExterCond)

sum(table(all$ExterCond))


### Electrical System ###
## 1NA value nominal -> factor ##

all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]
all$Electrical <- as.factor(all$Electrical)
table(all$Electrical)

sum(table(all$Electrical))
 

### type of sale ###
## 1 NA , nominal -> factor ##

#imputing mode
all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]
all$SaleType <- as.factor(all$SaleType)
table(all$SaleType)
sum(table(all$SaleType)) 


## condition of sale ##
##nominal -> factor ##
all$SaleCondition <- as.factor(all$SaleCondition)
table(all$SaleCondition)
sum(table(all$SaleCondition))


### bifurcating the character variables ###
Charcol <- names(all[,sapply(all, is.character)])
Charcol

### Foundation ###
## nominal values -> factors ##

all$Foundation <- as.factor(all$Foundation)
table(all$Foundation)
sum(table(all$Foundation))

### Heating ###
## Nominal-> factor ##
all$Heating <- as.factor(all$Heating)
table(all$Heating)
sum(table(all$Heating))

## Heating Quality and condition ##
## ordinal ##
all$HeatingQC<-as.integer(revalue(all$HeatingQC, Qualities))
table(all$HeatingQC)
sum(table(all$HeatingQC))

##  Central air conditioning ##
## ordinal -> vector ##
all$CentralAir<-as.integer(revalue(all$CentralAir, c('N'=0, 'Y'=1)))
table(all$CentralAir)
sum(table(all$CentralAir))

### Roof ###

## Roof Type ##
## nominal -> factor ##
all$RoofStyle <- as.factor(all$RoofStyle)
table(all$RoofStyle)
sum(table(all$RoofStyle))

##  Roof material ##
## nominal -> factor ##

all$RoofMatl <- as.factor(all$RoofMatl)
table(all$RoofMatl)
sum(table(all$RoofMatl))

### Land ###

## Land Slpe ##
## nominal -> factor ##
all$LandContour <- as.factor(all$LandContour)
table(all$LandContour)
sum(table(all$LandContour))

## Land slpe ##
## ordinal -> vector ##

all$LandSlope<-as.integer(revalue(all$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
table(all$LandSlope)
sum(table(all$LandSlope))

### Dweling ###

## Building Type ##
## seems ordinal, lets find out ##

ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(BldgType), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

## turns out to be nominal -> factor ##

all$BldgType <- as.factor(all$BldgType)
table(all$BldgType)
sum(table(all$BldgType))

## style of dwelling ##
## nominal -> factor ##

all$HouseStyle <- as.factor(all$HouseStyle)
table(all$HouseStyle)
sum(table(all$HouseStyle))

### Neighbourhood and conditions ###

## Neighborhood ##
## Nominal -> factor ##
all$Neighborhood <- as.factor(all$Neighborhood)
table(all$Neighborhood)
sum(table(all$Neighborhood))

## condition 1 :proximity to various conditions ##
## nominal -> factor ##
all$Condition1 <- as.factor(all$Condition1)
table(all$Condition1)
sum(table(all$Condition1))


## Condition 2 : Proximity to various conditions (if more than one is present) ##
## nominal -> factor ##
all$Condition2 <- as.factor(all$Condition2)
table(all$Condition2)
sum(table(all$Condition2))

### Pavement of Street and Drive way ###

## street ##
## Ordinal -> vector ##

all$Street<-as.integer(revalue(all$Street, c('Grvl'=0, 'Pave'=1)))
table(all$Street)
sum(table(all$Street))

## Paved Drive ##
## ordinal -> label encoding -> vector ##
all$PavedDrive<-as.integer(revalue(all$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
table(all$PavedDrive)
sum(table(all$PavedDrive))

### Month sold ###
## nominal -> factor ##
str(all$MoSold)
all$MoSold <- as.factor(all$MoSold)

dim(all)

ys <- ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(YrSold), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePr

ms <- ggplot(all[!is.na(all$SalePrice),], aes(x=MoSold, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePr

grid.arrange(ys, ms, widths=c(1,2))

### Ms Sub Class ###
## categorical ##

str(all$MSSubClass)
all$MSSubClass <- as.factor(all$MSSubClass)

#revalue for better readability
all$MSSubClass<-revalue(all$MSSubClass, c('20'='1 story 1946+',
'30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf',
'50'='1,5 story fin', '60'='2 story 1946+',
'70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', 
'85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+',
'150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel',
'190'='2 family conversion'))

str(all$MSSubClass)


numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(all, is.factor)) #index vector factor variables

cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')

## Checking the correlation again ##
all_numVar <- all[, numericVars]
#correlations of all numeric variables
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", 
               tl.cex = 0.7,cl.cex = .7, number.cex=.7)

## Applying random forest to determine the correlation of variables ##

set.seed(2018)
table(complete.cases(all[1:1460,-79]))
dim(all)
quick_RF <- randomForest(x=all[1:1460,-79], y=all$SalePrice[1:1460], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) +
 geom_bar(stat = 'identity') + labs(x = 'Variables',
y= '% increase MSE if variable is randomly permuted') +
 coord_flip() + theme(legend.position="none")


s1 <- ggplot(data= all, aes(x=GrLivArea)) +
  geom_density() + labs(x='Square feet living area')
s2 <- ggplot(data=all, aes(x=as.factor(TotRmsAbvGrd))) +
  geom_histogram(stat='count') + labs(x='Rooms above Ground')
s3 <- ggplot(data= all, aes(x=X1stFlrSF)) +
  geom_density() + labs(x='Square feet first floor')
s4 <- ggplot(data= all, aes(x=X2ndFlrSF)) +
  geom_density() + labs(x='Square feet second floor')
s5 <- ggplot(data= all, aes(x=TotalBsmtSF)) +
  geom_density() + labs(x='Square feet basement')
s6 <- ggplot(data= all[all$LotArea<100000,], aes(x=LotArea)) +
  geom_density() + labs(x='Square feet lot')
s7 <- ggplot(data= all, aes(x=LotFrontage)) +
  geom_density() + labs(x='Linear feet lot frontage')
s8 <- ggplot(data= all, aes(x=LowQualFinSF)) +
  geom_histogram() + labs(x='Low quality square feet 1st & 2nd')

layout <- matrix(c(1,2,5,3,4,8,6,7),4,2,byrow=TRUE)
multiplot(s1, s2, s3, s4, s5, s6, s7, s8, layout=layout)

cor(all$GrLivArea, (all$X1stFlrSF + all$X2ndFlrSF + all$LowQualFinSF))

head(all[all$LowQualFinSF>0, c('GrLivArea', 'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF')])

n1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=Neighborhood, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePr

n2 <- ggplot(data=all, aes(x=Neighborhood)) +
  geom_histogram(stat='count')+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(n1, n2)


## visualizing Quality variable

q1 <- ggplot(data=all, aes(x=as.factor(OverallQual))) +
  geom_histogram(stat='count')

q1
q2 <- ggplot(data=all, aes(x=as.factor(ExterQual))) +
  geom_histogram(stat='count')
q3 <- ggplot(data=all, aes(x=as.factor(BsmtQual))) +
  geom_histogram(stat='count')
q4 <- ggplot(data=all, aes(x=as.factor(KitchenQual))) +
  geom_histogram(stat='count')
q5 <- ggplot(data=all, aes(x=as.factor(GarageQual))) +
  geom_histogram(stat='count')
q6 <- ggplot(data=all, aes(x=as.factor(FireplaceQu))) +
  geom_histogram(stat='count')
q7 <- ggplot(data=all, aes(x=as.factor(PoolQC))) +
  geom_histogram(stat='count')

layout <- matrix(c(1,2,8,3,4,8,5,6,7),3,3,byrow=TRUE)
multiplot(q1, q2, q3, q4, q5, q6, q7, layout=layout)


## MSsubClass ##

ms1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=MSSubClass, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePr
ms2 <- ggplot(data=all, aes(x=MSSubClass)) +
  geom_histogram(stat='count')+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(ms1, ms2)

## Garage Variables ##
all$GarageYrBlt[2593] ## 2207 as year typo
all$GarageYrBlt[2593] <- 2007
g1 <- ggplot(data=all[all$GarageCars !=0,], aes(x=GarageYrBlt)) +
  geom_histogram()
g2 <- ggplot(data=all, aes(x=as.factor(GarageCars))) +
  geom_histogram(stat='count')
g3 <- ggplot(data= all, aes(x=GarageArea)) +
  geom_density()
g4 <- ggplot(data=all, aes(x=as.factor(GarageCond))) +
  geom_histogram(stat='count')
g5 <- ggplot(data=all, aes(x=GarageType)) +
  geom_histogram(stat='count')
g6 <- ggplot(data=all, aes(x=as.factor(GarageQual))) +
  geom_histogram(stat='count')
g7 <- ggplot(data=all, aes(x=as.factor(GarageFinish))) +
  geom_histogram(stat='count')
layout <- matrix(c(1,5,5,2,3,8,6,4,7),3,3,byrow=TRUE)
multiplot(g1, g2, g3, g4, g5, g6, g7, layout=layout)


## Basement Variable ##

b1 <- ggplot(data=all, aes(x=BsmtFinSF1)) +
  geom_histogram() + labs(x='Type 1 finished square feet')
b2 <- ggplot(data=all, aes(x=BsmtFinSF2)) +
  geom_histogram()+ labs(x='Type 2 finished square feet')
b3 <- ggplot(data=all, aes(x=BsmtUnfSF)) +
  geom_histogram()+ labs(x='Unfinished square feet')
b4 <- ggplot(data=all, aes(x=as.factor(BsmtFinType1))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 1 finished area')
b5 <- ggplot(data=all, aes(x=as.factor(BsmtFinType2))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 2 finished area')
b6 <- ggplot(data=all, aes(x=as.factor(BsmtQual))) +
  geom_histogram(stat='count')+ labs(x='Height of the basement')
b7 <- ggplot(data=all, aes(x=as.factor(BsmtCond))) +
  geom_histogram(stat='count')+ labs(x='Rating of general condition')
b8 <- ggplot(data=all, aes(x=as.factor(BsmtExposure))) +
  geom_histogram(stat='count')+ labs(x='Walkout or garden level walls')

layout <- matrix(c(1,2,3,4,5,9,6,7,8),3,3,byrow=TRUE)
multiplot(b1, b2, b3, b4, b5, b6, b7, b8, layout=layout)


### Feature Engineering ###

## Bathroom ##
all$TotBathrooms <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath + (all$BsmtHalfBath*0.5)
tb1 <- ggplot(data=all[!is.na(all$SalePrice),], aes(x=as.factor(TotBathrooms), y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
tb2 <- ggplot(data=all, aes(x=as.factor(TotBathrooms))) +
  geom_histogram(stat='count')
grid.arrange(tb1, tb2)

## Age of House ##

all$Remod <- ifelse(all$YearBuilt==all$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
all$Age <- as.numeric(all$YrSold)-all$YearRemodAdd

ggplot(data=all[!is.na(all$SalePrice),], aes(x=Age, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
cor(all$SalePrice[!is.na(all$SalePrice)], all$Age[!is.na(all$SalePrice)])

# remodled houses worth less #

ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(Remod), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice

all$IsNew <- ifelse(all$YrSold==all$YearBuilt, 1, 0)
table(all$IsNew)

# new houses worth more #
ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(IsNew), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice

all$YrSold <- as.factor(all$YrSold) #the numeric version is now not needed anymore


### Binning Neighbourhood ###

nb1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=median), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') + labs(x='Neighborhood', y='Median SalePrice') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePr

ice
nb2 <- ggplot(all[!is.na(all$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=mean), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "mean", fill='blue') + labs(x='Neighborhood', y="Mean SalePrice") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePr

grid.arrange(nb1, nb2)

all$NeighRich[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
all$NeighRich[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
all$NeighRich[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

table(all$NeighRich)


### total Square Feet ###

all$TotalSqFeet <- all$GrLivArea + all$TotalBsmtSF

ggplot(data=all[!is.na(all$SalePrice),], aes(x=TotalSqFeet, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))

cor(all$SalePrice, all$TotalSqFeet, use= "pairwise.complete.obs")
cor(all$SalePrice[-c(524, 1299)], all$TotalSqFeet[-c(524, 1299)], use= "pairwise.complete.obs")


### porch Variables ###
## consolidating ##

all$TotalPorchSF <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch
cor(all$SalePrice, all$TotalPorchSF, use= "pairwise.complete.obs")
cor(all$SalePrice, all$TotalPorchSF, use= "pairwise.complete.obs")


ggplot(data=all[!is.na(all$SalePrice),], aes(x=TotalPorchSF, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

### pre processing ###

## droping correlataed variables ##

dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

all <- all[,!(names(all) %in% dropVars)]

## removing outliers ##

all <- all[-c(524, 1299),]
all

## Pre Processing Data ##
numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] #numericVarNames was created before having done anything
numericVarNames <- append(numericVarNames, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnumeric <- all[, names(all) %in% numericVarNames]

DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')


DFnumeric[,i]
skew(DFnumeric[0,2])

## Skewness Chk ##
for(i in 1:ncol(DFnumeric)){
  if (abs(skewness(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

## Normalising the data ##

PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)

DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)


?model.matrix


## one hot encoding ##
## converting categorical data into numeric data as most ml algos require data to be numeric only ##
## model.matrix() ##

DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)

## check if some values are absent in the test set
ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$SalePrice),])+1):nrow(all),])==0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest] #removing predictors
#check if some values are absent in the train set
ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])

fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)


combined <- cbind(DFnorm, DFdummies) #combining all (now numeric) predictors into one dataframe 


## skewness of responsive variables ##
skewness(all$SalePrice)

qqnorm(all$SalePrice)
qqline(all$SalePrice)
all$SalePrice <- log(all$SalePrice) #default is the natural logarithm, "+1" is not necessary as there 
skewness(all$SalePrice)

qqnorm(all$SalePrice)
qqline(all$SalePrice)


## composing train and tes set ##
train1 <- combined[!is.na(all$SalePrice),]
test1 <- combined[is.na(all$SalePrice),]


## Laso Regrgression Model ##
set.seed(27042018)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune

min(lasso_mod$results$RMSE)

lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')


LassoPred <- predict(lasso_mod, test1)
predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
head(predictions_lasso)


## XG Boost ##
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)

label_train <- all$SalePrice[!is.na(all$SalePrice)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(train1), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(test1))

default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.05, #default = 0.3
  gamma=0,
  max_depth=3, #default=6
  min_child_weight=4, #default=1
  subsample=1,
  colsample_bytree=1
)
xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)

#train the model using the best iteration found by cross validation
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 454)

XGBpred <- predict(xgb_mod, dtest)
predictions_XGB <- exp(XGBpred) #need to reverse the log to the real values
head(predictions_XGB)


#view variable importance plot
install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp) #required for ggplot clustering
mat <- xgb.importance (feature_names = colnames(train1),model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)


## Averagin Predictions ##
sub_avg <- data.frame(Id = test_labels, SalePrice = (predictions_XGB+2*predictions_lasso)/3)
head(sub_avg)

write.csv(sub_avg, file = 'C:/Shreya Sharma/Machine Learning/Data csvs/house-prices-advanced-regression-techniques/average.csv', row.names = F)
