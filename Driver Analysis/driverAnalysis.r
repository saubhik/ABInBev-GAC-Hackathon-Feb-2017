#Preliminary lookout on the data made it evident that the data is a hierarchial clustered data.
#We first found out the hierarchy by drawing the dendogram (looking at the data)
rm(list = ls())

require(stats)
require(corrplot)
require(ggplot2)
require(plyr)
require(dplyr)
require(relaimpo)
require(randomForest)

#Setting the work directory where the csv file is located and reading the csv file
setwd("C:\\Users\\Pramit\\Desktop\\Hackathon data\\Focus Area - Driver Analysis, Visualisation")
data <- read.csv("GAC Hackathon_Sales_Data.csv")

# adjusting for NS values in columns concerned to economic demographics
#Imputing values by taking mean at segment level
df <- data[, c(41:53)]

anyNA(df$X.State.GDP.Millions.of.dollars..seasonally.adjusted.at.annual.rates...)
gdp <- data.frame(df$X.State.GDP.Millions.of.dollars..seasonally.adjusted.at.annual.rates...)
quarter_gdp <- data.frame(unique(gdp[c(1:182),]))
quarter_time <- c(rep(1:14))
data_gdp_train <- cbind(quarter_gdp, quarter_time)
names(data_gdp_train) <- c("quarter_gdp", "quarter_time")
model <- lm(quarter_gdp ~ quarter_time, data = data_gdp_train)
quarter_gdp[15,] <- as.numeric(model$coefficients[1] + model$coefficients[2] * 15)
quarter_gdp[16,] <- as.numeric(model$coefficients[1] + model$coefficients[2] * 16)
final_gdp <- data.frame()
for (i in 1:16) {
  final_gdp <- rbind(final_gdp, data.frame(rep(quarter_gdp[i,], 13)))
}
final_gdp <- rep(as.numeric(final_gdp[, 1]), nrow(df) / nrow(final_gdp))
df$X.State.GDP.Millions.of.dollars..seasonally.adjusted.at.annual.rates... <- final_gdp
anyNA(df$X.State.GDP.Millions.of.dollars..seasonally.adjusted.at.annual.rates...)

anyNA(df[,2]) # None!

anyNA(df[,3])
state_pci <- data.frame(df$State.Personal.income..thousands.of.dollars..seasonally.adjusted.quarterly.)
quarter_state_pci <- data.frame(unique(state_pci[c(1:169),]))
quarter_time <- c(rep(1:13))
data_state_pci_train <- cbind(quarter_state_pci, quarter_time)
names(data_state_pci_train) <- c("quarter_state_pci", "quarter_time")
model <- lm(quarter_state_pci ~ quarter_time, data = data_state_pci_train)
quarter_state_pci[14,] <- as.numeric(model$coefficients[1] + model$coefficients[2] * 14)
quarter_state_pci[15,] <- as.numeric(model$coefficients[1] + model$coefficients[2] * 15)
quarter_state_pci[16,] <- as.numeric(model$coefficients[1] + model$coefficients[2] * 16)
final_state_pci <- data.frame()
for (i in 1:16) {
  final_state_pci <- rbind(final_state_pci, data.frame(rep(quarter_state_pci[i,], 13)))
}
final_state_pci <- rep(as.numeric(final_state_pci[, 1]), nrow(df) / nrow(final_state_pci))
df$State.Personal.income..thousands.of.dollars..seasonally.adjusted.quarterly. <- final_state_pci
anyNA(df$State.Personal.income..thousands.of.dollars..seasonally.adjusted.quarterly.)

anyNA(df$Resident.Population.in.city..Thousands.of.Persons..Annual..Not.Seasonally.Adjusted)
res_pop <- data.frame(df$Resident.Population.in.city..Thousands.of.Persons..Annual..Not.Seasonally.Adjusted)
res_pop_train <- data.frame(unique(res_pop[c(1:156),]))
year_time <- c(rep(1:3))
res_pop_train <- cbind(res_pop_train, year_time)
names(res_pop_train) <- c("res_pop", "year_time")
model <- lm(res_pop ~ year_time, data = res_pop_train)
res_pop <- res_pop_train[,1]
res_pop[4] <- as.numeric(model$coefficients[1] + model$coefficients[2] * 4)
final_res_pop <- data.frame()
for (i in 1:4) { final_res_pop <- rbind(final_res_pop, data.frame(rep(res_pop[i], 52))) }
final_res_pop <- rep(as.numeric(final_res_pop[,1]), nrow(df) / nrow(final_res_pop))
df$Resident.Population.in.city..Thousands.of.Persons..Annual..Not.Seasonally.Adjusted <- final_res_pop
anyNA(df$Resident.Population.in.city..Thousands.of.Persons..Annual..Not.Seasonally.Adjusted)

anyNA(df$Per.Capita.Personal.Income.in.city..Dollars..Annual..Not.Seasonally.Adjusted)
pcpi_city <- data.frame(df$Per.Capita.Personal.Income.in.city..Dollars..Annual..Not.Seasonally.Adjusted)
pcpi_city <- as.numeric(unique(pcpi_city[c(1:104),]))
pcpi_city[3] <- pcpi_city[2] + (pcpi_city[2] - pcpi_city[1])
pcpi_city[4] <- pcpi_city[3] + (pcpi_city[3] - pcpi_city[2])
final_pcpi_city <- data.frame()
for (i in 1:4) { final_pcpi_city <- rbind(final_pcpi_city, data.frame(rep(pcpi_city[i], 52))) }
final_pcpi_city <- rep(as.numeric(final_pcpi_city[1,]), nrow(df) / nrow(final_pcpi_city))
df$Per.Capita.Personal.Income.in.city..Dollars..Annual..Not.Seasonally.Adjusted <- final_pcpi_city
anyNA(df$Per.Capita.Personal.Income.in.city..Dollars..Annual..Not.Seasonally.Adjusted)

occupancy <- df$Occupancy....
levels(occupancy) <- c("NA", levels(occupancy)[2:42])
df$Occupancy.... <- as.numeric(levels(occupancy))[occupancy]
anyNA(df$Occupancy....)
df$Occupancy....[is.na(df$Occupancy....)] <- mean(df$Occupancy...., na.rm = T)
anyNA(df$Occupancy....)

anyNA(df$Producer.Price.Index.by.Industry)
df$Producer.Price.Index.by.Industry[is.na(df$Producer.Price.Index.by.Industry)] <- mean(df$Producer.Price.Index.by.Industry, na.rm = T)
anyNA(df$Producer.Price.Index.by.Industry)

anyNA(df)

data <- cbind(data[, -c(41:53)], df)

miss_perc <- function(x) {
  sum(is.na(x)) / length(x) * 100
}


#Creating principal components with the geographical data provided (Eg: Humidity,Precipitation,Rainfall)
data_geo<-data[,20:40]
View(data_geo)
summary(data_geo)
pc1<-prcomp(data_geo,center = TRUE, scale = TRUE, na.action = na.exclude)
summary(pc1)
pc_geo<-pc1$x[,1:4]   #4 principal components explain almost 85 percent of the data.

data<-data[,-c(20:40)]
data<-data.frame(data,pc_geo)


#EM algorithm use for missing value imputation (Expectation Maximization)
#The Amelia package is used to create the impute value data frames and then mean is taken over the three data frames created
#Around 80 percent of data was missing for some columns and so EM algorithm was put to use.
require(Amelia)
sapply(data, function(x) sum(is.na(x)))
data2<-data.frame(data)
data2<-data2[,-c(1:9)]
data2<-data2[,-c(12:31)]
missmap(data2)

ncpus = 8
Completed_data<-amelia(data2,p2s=0,m=3,ncpus=ncpus)
impData1<-data.frame(Completed_data$imputations[1])
impData2<-data.frame(Completed_data$imputations[2])
impData3<-data.frame(Completed_data$imputations[3])
DCount <- (impData1$imp1.Display.Count + impData2$imp2.Display.Count + impData3$imp3.Display.Count)/3
DShare <- (impData1$imp1.Display.Share + impData2$imp2.Display.Share + impData3$imp3.Display.Share)/3
FCount <- (impData1$imp1.Feature.Count + impData2$imp2.Feature.Count + impData3$imp3.Feature.Count)/3
FShare <- (impData1$imp1.Feature.Share + impData2$imp2.Feature.Share + impData3$imp3.Feature.Share)/3
Unit.Sales <- (impData1$imp1.Unit.Sales + impData2$imp2.Unit.Sales +impData3$imp3.Unit.Sales)/3
  
#Creating the final dataframe for running models :
data<-data.frame(data[,-c(10,11,13,14,17)])
impData<-data.frame(DCount,DShare,FCount,FShare,Unit.Sales)
impData<-apply(impData, 2, function(x) {ifelse(x < 0, 0, x)}) 
data<-cbind(data,impData)
data<-data[-c(19)]

#Breaking the entire data into parts using the dendogram ( Dendogram is drawn manually )
data1 <- data[which(data$AB.Megasegment.Value == "ABOVE CORE" & data$AB.Segment.Value == "HE BEER"), ]
data2 <- data[which(data$AB.Megasegment.Value == "ABOVE CORE" & data$AB.Segment.Value == "HE NEAR BEER"), ]
data3 <- data[which(data$AB.Megasegment.Value == "CORE & VALUE" & data$AB.Segment.Value == "CORE"), ]
data4 <- data[which(data$AB.Megasegment.Value == "CORE & VALUE" & data$AB.Segment.Value == "VALUE"), ]
data5 <- data[which(data$AB.Megasegment.Value == "NON ALC" & data$AB.Segment.Value == "NON ALC"), ]


#Deleting the columns with categorical variables
#already we have 5 datasets built over the segments and megasegments
data1<-data1[,-c(1:9)]
data2<-data2[,-c(1:9)]
data3<-data3[,-c(1:9)]
data4<-data4[,-c(1:9)]
data5<-data5[,-c(1:9)]

# Fitting models
# Model 1A : Linear Regression Analysis
# The relative importance of predictors are found to check which is the driver in the regression analysis.
#Megasegment value : ABOVE CORE , AB Segment value : HE BEER
data11<-data1[,-c(2,5,7,8,9,11,12,13,14,15,16,23,24)]
colnames(data11) = c("Distribution","Price.per.Volume","Volume.Sales",                                                           
                     "StateGDP","Occupancy","Producer.Price.Index",                                       
                     "PC1","PC2","PC3","PC4","DCount","FShare","Unit.Sales")

modfit11<-lm(Volume.Sales ~ Distribution+Price.per.Volume+                                                           
                     StateGDP+Occupancy+Producer.Price.Index+                                       
                     PC1+PC2+PC3+PC4+DCount+FShare+Unit.Sales,data=data11)
coefficients(modfit11) 
summary(modfit11)
calc.relimp(modfit11,type=c("lmg"),
            rela=TRUE)



#Megasegment value : ABOVE CORE , AB Segment value : NEAR HE BEER
data12<-data2[,-c(2,5,7,8,9,11,12,13,14,15,16,23,24)]
colnames(data12) = c("Distribution","Price.per.Volume","Volume.Sales",                                                           
                     "StateGDP","Occupancy","Producer.Price.Index",                                       
                     "PC1","PC2","PC3","PC4","DCount","FShare","Unit.Sales"  )
modfit12<-lm(Volume.Sales ~ Distribution+Price.per.Volume+                                                           
               StateGDP+Occupancy+Producer.Price.Index+                                       
               PC1+PC2+PC3+PC4+DCount+FShare+Unit.Sales,data=data12)
coefficients(modfit12) 
summary(modfit12)
calc.relimp(modfit12,type=c("lmg"),
            rela=TRUE)


#Megasegment value : CORE & VALUE , AB Segment value : CORE
data13<-data3[,-c(2,5,7,8,9,11,12,13,14,15,16,23,24)]
colnames(data13) = c("Distribution","Price.per.Volume","Volume.Sales",                                                           
                     "StateGDP","Occupancy","Producer.Price.Index",                                       
                     "PC1","PC2","PC3","PC4","DCount","FShare","Unit.Sales"  )
modfit13<-lm(Volume.Sales ~ Distribution+Price.per.Volume+                                                           
               StateGDP+Occupancy+Producer.Price.Index+                                       
               PC1+PC2+PC3+PC4+DCount+FShare+Unit.Sales,data=data13)
coefficients(modfit13) 
summary(modfit13)
calc.relimp(modfit13,type=c("lmg"),
            rela=TRUE)



#Megasegment value : CORE & VALUE , AB Segment value : VALUE
data14<-data4[,-c(2,5,7,8,9,11,12,13,14,15,16,23,24)]
colnames(data14) = c("Distribution","Price.per.Volume","Volume.Sales",                                                           
                     "StateGDP","Occupancy","Producer.Price.Index",                                       
                     "PC1","PC2","PC3","PC4","DCount","FShare","Unit.Sales"  )
modfit14<-lm(Volume.Sales ~ Distribution+Price.per.Volume+                                                           
               StateGDP+Occupancy+Producer.Price.Index+                                       
               PC1+PC2+PC3+PC4+DCount+FShare+Unit.Sales,data=data14)
coefficients(modfit14) 
summary(modfit14)
calc.relimp(modfit14,type=c("lmg"),
            rela=TRUE)


#Megasegment value : NON ALC , AB Segment value : NON ALC
data15<-data5[,-c(2,5,7,8,9,11,12,13,14,15,16,23,24)]
colnames(data15) = c("Distribution","Price.per.Volume","Volume.Sales",                                                           
                     "StateGDP","Occupancy","Producer.Price.Index",                                       
                     "PC1","PC2","PC3","PC4","DCount","FShare","Unit.Sales"  )
modfit15<-lm(Volume.Sales ~ Distribution+Price.per.Volume+                                                           
               StateGDP+Occupancy+Producer.Price.Index+                                       
               PC1+PC2+PC3+PC4+DCount+FShare+Unit.Sales,data=data15)
coefficients(modfit15) 
summary(modfit15)
calc.relimp(modfit15,type=c("lmg"),
            rela=TRUE)


#Model 1B: Linear Regression Analysis on a higher level of hierarchy
#Regressing on a higher level in the hierarchy i.e. combining the datasets we have :
#So now we have Megasegment value : ABOVE CORE , AB Segment Value : ALL
data_1<-rbind(data1,data2)
data_11<-data_1[,-c(2,5,7,8,9,11,12,13,14,15,16,23,24)]
colnames(data_11) = c("Distribution","Price.per.Volume","Volume.Sales",                                                           
                      "StateGDP","Occupancy","Producer.Price.Index",                                       
                      "PC1","PC2","PC3","PC4","DCount","FShare","Unit.Sales"   )
modfit_11<-lm(Volume.Sales ~ Distribution+Price.per.Volume+                                                           
               StateGDP+Occupancy+Producer.Price.Index+                                       
               PC1+PC2+PC3+PC4+DCount+FShare+Unit.Sales,data=data_11)
coefficients(modfit_11) 
summary(modfit_11)
calc.relimp(modfit_11,type=c("lmg"),
            rela=TRUE)


#So now we have Megasegment value : CORE & VALUE , AB Segment Value : ALL
data_2<-rbind(data3,data4)
data_22<-data_2[,-c(2,5,7,8,9,11,12,13,14,15,16,23,24)]
colnames(data_22) = c("Distribution","Price.per.Volume","Volume.Sales",                                                           
                      "StateGDP","Occupancy","Producer.Price.Index",                                       
                      "PC1","PC2","PC3","PC4","DCount","FShare","Unit.Sales"  )
modfit_22<-lm(Volume.Sales ~ Distribution+Price.per.Volume+                                                           
                StateGDP+Occupancy+Producer.Price.Index+                                       
                PC1+PC2+PC3+PC4+DCount+FShare+Unit.Sales,data=data_22)
coefficients(modfit_22) 
summary(modfit_22)
calc.relimp(modfit_22,type=c("lmg"),
            rela=TRUE)




#Model 2A : Random Forest model
#The relative importance of each predictor is found and plotted to see which has the highest importance
#Megasegment value : ABOVE CORE , AB Segment value : HE BEER
modfit21<-randomForest(data11[,3]~.,data=data11)
importance(modfit21)
plot(modfit21,type="o")

#Megasegment value : ABOVE CORE , AB Segment value : NEAR HE BEER
modfit22<-randomForest(data12[,3]~.,data=data12)
importance(modfit22)
plot(modfit22,type="o")

#Megasegment value : CORE & VALUE , AB Segment value : CORE
modfit23<-randomForest(data13[,3]~.,data=data13)
importance(modfit23)
plot(modfit23,type="o")

#Megasegment value : CORE & VALUE , AB Segment value : VALUE
modfit24<-randomForest(data14[,3]~.,data=data14)
importance(modfit24)
plot(modfit24,type="o")

#Megasegment value : NON ALC , AB Segment value : NON ALC
modfit25<-randomForest(data15[,3]~.,data=data15)
importance(modfit25)
plot(modfit25,type="o")



#Model 2B: Random forest Analysis on a higher level of hierarchy
#Using random forest on a higher level in the hierarchy i.e. combining the datasets we have :
#So now we have Megasegment value : ABOVE CORE , AB Segment Value : ALL
data_2<-rbind(data1,data2)
data_22<-data_1[,-c(2,5,7,8,9,11,12,13,14,15,16,23,24)]
colnames(data_22) = c("Distribution","Price.per.Volume","Volume.Sales",                                                           
                      "StateGDP","Occupancy","Producer.Price.Index",                                       
                      "PC1","PC2","PC3","PC4","DCount","FShare","Unit.Sales"   )
modfit33<-randomForest(data_22[,3]~.,data=data_22)
importance(modfit33)
plot(modfit33,type="o")

#So now we have Megasegment value : CORE & VALUE , AB Segment Value : ALL
modfit44<-randomForest(data_22[,3]~.,data=data_22)
importance(modfit44)
plot(modfit44,type="o")




#Future scope : We have used two models and found the drivers for each regression analysis and random forest model.
#We may fit more models and create an ensemble for better understanding of the drivers that play very important role.
