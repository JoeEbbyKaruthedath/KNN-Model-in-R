library(ggplot2) #Importing ggplot

#importing Data
AllData <- read.table("GradedHW1-All-Data.csv",header=T,sep=",",
                      stringsAsFactors = F,na.strings="")

AllData <- AllData[AllData$Bldg.Type=="1Fam",]

RPerm <- sample(nrow(AllData))
AllData <- AllData[RPerm,]

TrainInd <- ceiling(nrow(AllData)/2)
ValInd <- ceiling((nrow(AllData)-TrainInd)/2)+TrainInd

TrainData <- AllData[1:TrainInd,]
ValData <- AllData[(TrainInd+1):ValInd,]
TestData <- AllData[(ValInd+1):nrow(AllData),]

TrainData <- read.table("GradedHW1-Train-Data.csv",header=T,sep=",",
                        stringsAsFactors = F,na.strings="")

TestData <- read.table("GradedHW1-Test-Data.csv",header=T,sep=",",
                       stringsAsFactors = F,na.strings="")

ValData <- read.table("GradedHW1-Validation-Data.csv",header=T,sep=",",
                      stringsAsFactors = F,na.strings="")

#Data Preprocessing
TrainData$Building.Age <- 2010 - TrainData$Year.Built
TestData$Building.Age <- 2010 - TestData$Year.Built
ValData$Building.Age <- 2010 - ValData$Year.Built

ValData <- filter(ValData, !is.na(ValData$Total.Bsmt.SF))

TrainData.subset <- TrainData[c('Lot.Area','Total.Bsmt.SF','Gr.Liv.Area','Full.Bath','Bedroom.AbvGr','Building.Age')]
TrainData.target <- TrainData$SalePrice
TestData.subset <- TestData[c('Lot.Area','Total.Bsmt.SF','Gr.Liv.Area','Full.Bath','Bedroom.AbvGr','Building.Age')]
TestData.target <- TestData$SalePrice
ValData.subset <- ValData[c('Lot.Area','Total.Bsmt.SF','Gr.Liv.Area','Full.Bath','Bedroom.AbvGr','Building.Age')]
ValData.target <- ValData$SalePrice

library(Metrics)
MSE <- rep(NA, 40)
Neighbhours <- rep(NA, 40)



#Model building using Raw Data
for (x in 1:40) {
  out <- knn.reg(train=TrainData.subset, test=ValData.subset, y=TrainData.target, k=x)
  MSE[x] = rmse(ValData.target, out$pred)
}


#Plotting the K values against RMSE
for( x in 1: 40){
       Neighbhours[x] = x
   }
MSEm = data.frame(MSE)
ggplot(data = MSEm, aes(x=Neighbhours, y=MSE))+geom_line()+ggtitle("Q3) MSE vs Neighbhours for Raw Data")

#Mean Squared Error on Test Data with best K value
out <- knn.reg(train=TrainData.subset, test=TestData.subset, y=TrainData.target, k=12)
rmse(TestData.target, out$pred)


ValDataScaled <- ValData.subset
TestDataScaled<- TestData.subset

#Scaling of Data
TrainDataScaled$Lot.Area <- ((TrainData$Lot.Area - mean(TrainData$Lot.Area))/ sd(TrainData$Lot.Area))
TrainDataScaled$Total.Bsmt.SF <- ((TrainData$Total.Bsmt.SF - mean(TrainData$Total.Bsmt.SF))/ sd(TrainData$Total.Bsmt.SF))
TrainDataScaled$Gr.Liv.Area<- ((TrainData$Gr.Liv.Area - mean(TrainData$Gr.Liv.Area))/ sd(TrainData$Gr.Liv.Area))
TrainDataScaled$Full.Bath <- ((TrainData$Full.Bath - mean(TrainData$Full.Bath))/sd(TrainData$Full.Bath))
TrainDataScaled$Bedroom.AbvGr <- ((TrainData$Bedroom.AbvGr - mean(TrainData$Bedroom.AbvGr))/sd(TrainData$Bedroom.AbvGr))
TrainDataScaled$Building.Age <- ((TrainData$Building.Age - mean(TrainData$Building.Age))/sd(TrainData$Building.Age))

ValDataScaled$Lot.Area <- ((ValData$Lot.Area - mean(TrainData$Lot.Area))/ sd(TrainData$Lot.Area))
ValDataScaled$Total.Bsmt.SF <- ((ValData$Total.Bsmt.SF - mean(TrainData$Total.Bsmt.SF))/ sd(TrainData$Total.Bsmt.SF))
ValDataScaled$Gr.Liv.Area<- ((ValData$Gr.Liv.Area - mean(TrainData$Gr.Liv.Area))/ sd(TrainData$Gr.Liv.Area))
ValDataScaled$Full.Bath <- ((ValData$Full.Bath - mean(TrainData$Full.Bath))/sd(TrainData$Full.Bath))
ValDataScaled$Bedroom.AbvGr <- ((ValData$Bedroom.AbvGr - mean(TrainData$Bedroom.AbvGr))/sd(TrainData$Bedroom.AbvGr))
ValDataScaled$Building.Age <- ((ValData$Building.Age - mean(TrainData$Building.Age))/sd(TrainData$Building.Age))

TestDataScaled$Lot.Area <- ((TestData$Lot.Area - mean(TrainData$Lot.Area))/ sd(TrainData$Lot.Area))
TestDataScaled$Total.Bsmt.SF <- ((TestData$Total.Bsmt.SF - mean(TrainData$Total.Bsmt.SF))/ sd(TrainData$Total.Bsmt.SF))
TestDataScaled$Gr.Liv.Area<- ((TestData$Gr.Liv.Area - mean(TrainData$Gr.Liv.Area))/ sd(TrainData$Gr.Liv.Area))
TestDataScaled$Full.Bath <- ((TestData$Full.Bath - mean(TrainData$Full.Bath))/sd(TrainData$Full.Bath))
TestDataScaled$Bedroom.AbvGr <- ((TestData$Bedroom.AbvGr - mean(TrainData$Bedroom.AbvGr))/sd(TrainData$Bedroom.AbvGr))
TestDataScaled$Building.Age <- ((TestData$Building.Age - mean(TrainData$Building.Age))/sd(TrainData$Building.Age))

#Model building using scaled data
for (x in 1:40) {
  out <- knn.reg(train=TrainDataScaled, test=ValDataScaled, y=TrainData.target, k=x)
  MSE[x] = rmse(ValData.target, out$pred)
}

MSEm<- data.frame(MSE)
ggplot(data = MSEm, aes(x=Neighbhours, y=MSE))+geom_line()+ggtitle(("Q5) MSE vs Neighbhours for Standardized Data"))

out <- knn.reg(train=TrainDataScaled, test=TestDataScaled, y=TrainData.target, k=12)
rmse(TestData.target, out$pred)

TrainDataTransformed <- TrainData.subset
ValDataTransformed <- ValData.subset
TestDataTransformed <- TestData.subset


#Transformation of data to convert it to the normal form
TrainDataTransformed$Lot.Area <- (TrainDataTransformed$Lot.Area)^(1/3)
TrainDataTransformed$Total.Bsmt.SF <- sqrt(TrainDataTransformed$Total.Bsmt.SF)
TrainDataTransformed$Gr.Liv.Area <- (TrainDataTransformed$Gr.Liv.Area)^(1/3)
TrainDataTransformed$Building.Age <- sqrt(TrainDataTransformed$Building.Age)

ValDataTransformed$Lot.Area <- (ValDataTransformed$Lot.Area)^(1/3)
ValDataTransformed$Total.Bsmt.SF <- sqrt(ValDataTransformed$Total.Bsmt.SF)
ValDataTransformed$Gr.Liv.Area <- (ValDataTransformed$Gr.Liv.Area)^(1/3)
ValDataTransformed$Building.Age <- sqrt(ValDataTransformed$Building.Age)

TestDataTransformed$Lot.Area <- (TestDataTransformed$Lot.Area)^(1/3)
TestDataTransformed$Total.Bsmt.SF <- sqrt(TestDataTransformed$Total.Bsmt.SF)
TestDataTransformed$Gr.Liv.Area <- (TestDataTransformed$Gr.Liv.Area)^(1/3)
TestDataTransformed$Building.Age <- sqrt(TestDataTransformed$Building.Age)

#Building the KNN model out of Transformed data
for (x in 1:40) {
  out <- knn.reg(train=TrainDataTransformed, test=ValDataTransformed, y=TrainData.target, k=x)
  MSE[x] = rmse(ValData.target, out$pred)
}
MSEm <- data.frame(MSE)
ggplot(data = MSEm, aes(x=Neighbhours, y=MSE))+geom_line()+ggtitle("Q7) MSE vs Neighbhors for Transformed Data")

out <- knn.reg(train=TrainDataTransformed, test=TestDataTransformed, y=TrainData.target, k=12)
rmse(TestData.target, out$pred)


TrainDataTransformedSc <- TrainDataTransformed
ValDataTransformedSc <- ValDataTransformed
TestDataTransformedSc <- TestDataTransformed


#Standardising Transformed Data
TrainDataTransformedSc$Lot.Area <- ((TrainDataTransformed$Lot.Area - mean(TrainData$Lot.Area))/ sd(TrainData$Lot.Area))
TrainDataTransformed$Total.Bsmt.SF <- ((TrainDataTransformed$Total.Bsmt.SF - mean(TrainData$Total.Bsmt.SF))/ sd(TrainData$Total.Bsmt.SF))
TrainDataTransformed$Gr.Liv.Area<- ((TrainDataTransformed$Gr.Liv.Area - mean(TrainData$Gr.Liv.Area))/ sd(TrainData$Gr.Liv.Area))
TrainDataTransformed$Full.Bath <- ((TrainDataTransformed$Full.Bath - mean(TrainData$Full.Bath))/sd(TrainDataScaled$Full.Bath))
TrainDataTransformed$Bedroom.AbvGr <- ((TrainDataTransformed$Bedroom.AbvGr - mean(TrainData$Bedroom.AbvGr))/sd(TrainData$Bedroom.AbvGr))
TrainDataTransformed$Building.Age <- ((TrainDataTransformed$Building.Age - mean(TrainData$Building.Age))/sd(TrainData$Building.Age))

ValDataTransformedSc$Lot.Area <- ((ValDataTransformed$Lot.Area - mean(TrainData$Lot.Area))/ sd(TrainData$Lot.Area))
ValDataTransformed$Total.Bsmt.SF <- ((ValDataTransformed$Total.Bsmt.SF - mean(TrainData$Total.Bsmt.SF))/ sd(TrainData$Total.Bsmt.SF))
ValDataTransformed$Gr.Liv.Area<- ((ValDataTransformed$Gr.Liv.Area - mean(TrainData$Gr.Liv.Area))/ sd(TrainData$Gr.Liv.Area))
ValDataTransformed$Full.Bath <- ((ValDataTransformed$Full.Bath - mean(TrainData$Full.Bath))/sd(TrainData$Full.Bath))
ValDataTransformed$Bedroom.AbvGr <- ((ValDataTransformed$Bedroom.AbvGr - mean(TrainData$Bedroom.AbvGr))/sd(TrainData$Bedroom.AbvGr))
ValDataTransformed$Building.Age <- ((ValDataTransformed$Building.Age - mean(TrainData$Building.Age))/sd(TrainData$Building.Age))

TestDataTransformedSc$Lot.Area <- ((TestDataTransformed$Lot.Area - mean(TrainData$Lot.Area))/ sd(TrainData$Lot.Area))
TestDataTransformed$Total.Bsmt.SF <- ((TestDataTransformed$Total.Bsmt.SF - mean(TrainData$Total.Bsmt.SF))/ sd(TrainData$Total.Bsmt.SF))
TestDataTransformed$Gr.Liv.Area<- ((TestDataTransformed$Gr.Liv.Area - mean(TrainData$Gr.Liv.Area))/ sd(TrainData$Gr.Liv.Area))
TestDataTransformed$Full.Bath <- ((TestDataTransformed$Full.Bath - mean(TrainData$Full.Bath))/sd(TrainData$Full.Bath))
TestDataTransformed$Bedroom.AbvGr <- ((TestDataTransformed$Bedroom.AbvGr - mean(TrainData$Bedroom.AbvGr))/sd(TrainData$Bedroom.AbvGr))
TestDataTransformed$Building.Age <- ((TestDataTransformed$Building.Age - mean(TrainData$Building.Age))/sd(TrainData$Building.Age))


#Prediction for Transformed and scaled data
for (x in 1:40) {
  out <- knn.reg(train=TrainDataTransformedSc, test=ValDataTransformedSc, y=TrainData.target, k=x)
  MSE[x] = rmse(ValData.target, out$pred)
}

out <- knn.reg(train=TrainDataTransformedSc, test=TestDataTransformedSc, y=TrainData.target, k=5)
rmse(TestData.target, out$pred)

MSEm <- data.frame(MSE)
ggplot(data = MSEm, aes(x=Neighbhours, y=MSE))+geom_line()+ggtitle("Q9) MSE for Transformed and Standardized Data ")


#********************************************************************************************
#THE MODEL WITH LEAST RMSE FOR TEST DATA WAS FOUND TO BE THE ONE WHICH MADE USE OF THE SCALED DAT
#********************************************************************************************


