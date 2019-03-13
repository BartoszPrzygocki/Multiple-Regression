#Multiple Regression 
#Bartosz Przygocki 
#November 2018

#Librarys
pacman::p_load("readr", "ggplot2", "e1071", "caret","mlr", "plotly", "corrplot",
               "party", "ipred","dplyr","plot3D","tidyr","purrr","randomForest")
library(broom)



#file import 
Existing_Products<-read.delim("existingproductattributes2017.2.csv",",",header=T)
New_Products<-read.delim("newproductattributes2017.2.csv",",",header=T)


#data discovery stage 
summary(Existing_Products)
table(Existing_Products$ProductType)
str(Existing_Products)


anyNA(Existing_Products)
summary(is.na(Existing_Products))



Existing_Products$BestSellersRank<-NULL
Existing_Products$ProductNum<-NULL


#check for dupliactes 
duplicated(Existing_Products)
duplicated(Existing_Products$Volume)
table(Existing_Products$Volume)



#create dummyfiles for LM 

# dummify the data

dummy <- dummyVars("~.", data = Existing_Products)
dummyData <- data.frame(predict(dummy, newdata = Existing_Products))


str(dummyData)

#plot all attributes 

for (i in 1:(ncol(Existing_Products))){
  if (is.numeric(Existing_Products[,i])){
    boxplot(Existing_Products[,i],main=paste("Boxplot of",colnames(Existing_Products)[i]),ylab=colnames(Existing_Products)[i])
    qqnorm(Existing_Products[,i],main=paste("Normal Q-Q Plot of",colnames(Existing_Products)[i])) #plot qqnorm
    qqline(Existing_Products[,i],col="red") #add qqnormal line in red
    hist(Existing_Products[,i],main=paste("Histogram of",colnames(Existing_Products)[i]), #make the histogram
         xlab=colnames(Existing_Products)[i])
  }
}


#look for outliers 
plot(Existing_Products$Volume,Existing_Products$ProductType)

boxplot(Existing_Products$Volume,main="Boxplot of volume",ylab="Volume")


LookingFor_Ouliers<-plot_ly(Existing_Products, x = ~ProductType, y = ~Volume,  type="scatter", mode = "markers", 
        marker=list( size=10 , opacity=0.5)  )



outliersVolume <- boxplot(Existing_Products$Volume)$out
plot(outliersVolume) # Outliers 11204 and  7036


Existing_Products <- Existing_Products[-which(Existing_Products$Volume %in% outliersVolume),] # Remove outliers in Volume

Gone_Outlier<-plot_ly(Existing_Products, x = ~ProductType, y = ~Volume,  type="scatter", mode = "markers", 
        marker=list( size=10 , opacity=0.5)  )



# dummify the data

dummy <- dummyVars("~.", data = Existing_Products)
dummyData <- data.frame(predict(dummy, newdata = Existing_Products))
str(dummyData)


##############Atribute Selection 

####Corrleaction Matrix 

res1 <- cor.mtest(dummyData,conf.level = .95)



Correlation<-cor(dummyData)

corrplot(Correlation, order="hclust", 
         p.mat = res1$p, sig.level = 0.5,insig = "blank",tl.cex=0.5,method = "number",number.cex = 0.25)

dummyData$x5StarReviews<-NULL

#Decions Tree and Randome Forst to check for the  best attributes 

set.seed(123)
control.tree <- ctree_control(maxdepth = 10)
DecisionTREE <- ctree (Volume ~ ., data=dummyData, 
                        controls = control.tree)
plot(Decision.tree) #4 stars and Positive Serivece 


fit_rf <- randomForest(Volume~.,data=dummyData,importance = TRUE)
varImpPlot(fit_rf,type=2)#4 stars and Positive Serivece 
varImp(fit_rf)

########Multiple Linear Regression


#data normalization for LM 

ggplot(training, aes(x4StarReviews)) + geom_density(fill="blue")
ggplot(training, aes(log(x4StarReviews))) + geom_density(fill="blue")
ggplot(training, aes(sqrt(x4StarReviews))) + geom_density(fill="blue")




dummyData %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()                         # as density

normalized_data <- as.data.frame(apply(dummyData[, 1:25], 2, function(x) (x - min(x))/(max(x)-min(x))))
summary(normalized_data)
normalized_data$Volume<-dummyData$Volume


normalized_data %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()                         # as density



ncol(dummyData)


LM_1_Test <- lm(Volume ~ x4StarReviews + PositiveServiceReview+ 0, data = normalized_data)
summary(LM_1)
LM_1_Diagnostics<-augment(LM_1_Test)
plot(LM_1_Test)


LM_2_Test<-lm(Volume ~.+ 0, data = normalized_data)
summary(LM_2)
LM_2_Diagnostics<-augment(LM_2_Test)
plot(LM_2_Test)

LM_3_Test <-lm(Volume~ x4StarReviews + PositiveServiceReview+ProductDepth+Recommendproduct+0,data=normalized_data)
summary(LM_3)
plot(LM_3_Test)


anova(LM_1_Test,LM_2_Test,LM_3_Test)

#None of the regression models display linear relationship assumption hence linear models, will not be applied 

###################################Prpering Training and Validation for other Models##################################################

inTraining <- createDataPartition(dummyData$Volume, p = .75, list = FALSE)
training <- dummyData[ inTraining,]
validation  <- dummyData[-inTraining,]


training%>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()                         # as density


validation%>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()                         # as density


fitControl <- trainControl(
  method = "repeatedcv",
  predictionBounds = c(0,NA),#?????????????
  number = 10,
  repeats = 3)

#RANDOM FOREST 

set.seed(123)


modelRF1 <- caret::train(Volume~., data=training,
                                  method = "rf", 
                                  trControl = fitControl,
                                  tuneLength = 3)

RF1_prediction<-predict(modelRF1, validation)
MetricsRF1<-postResample(validation$Volume,RF1_prediction)


modelRF2 <- caret::train(Volume~x4StarReviews + PositiveServiceReview, data=training,
                         method = "rf", 
                         trControl = fitControl,
                         tuneLength = 3)

RF2_prediction<-predict(modelRF2, validation)
MetricsRF2<-postResample(validation$Volume,RF2_prediction)


modelRF3 <- caret::train(Volume~x4StarReviews + PositiveServiceReview+ProductDepth, data=training,
                         method = "rf", 
                         trControl = fitControl,
                         tuneLength = 3)

RF3_prediction<-predict(modelRF3, validation)
MetricsRF3<-postResample(validation$Volume,RF3_prediction)




modelXGBT1 <- caret::train(Volume~., data=training,
                         method = "xgbTree", 
                         trControl = fitControl,
                         tuneLength = 3)

XGBT1_prediction<-predict(modelXGBT1, validation)
MetricsXGBT1<-postResample(validation$Volume,XGBT1_prediction)



modelXGBT2 <- caret::train(Volume~x4StarReviews + PositiveServiceReview, data=training,
                           method = "xgbTree", 
                           trControl = fitControl,
                           tuneLength = 3)

XGBT2_prediction<-predict(modelXGBT2, validation)
MetricsXGBT2<-postResample(validation$Volume,XGBT2_prediction)





######Linear Regression ,Gaussian Models, Baysen


inTrainingG <- createDataPartition(normalized_data$Volume, p = .75, list = FALSE)
trainingG <- normalized_data[ inTrainingG,]
validationG  <- normalized_data[-inTrainingG,]


LM_1 <- caret::train(Volume~., data=trainingG,
              method = "lm", 
              trControl = fitControl,
              tuneLength = 3,
              verboseIter = TRUE)



LM_1_prediction<-predict(LM_1 , validationG)
MetricsLM_1<-postResample(validation$Volume,LM_1_prediction)



LM_2 <- caret::train(Volume~x4StarReviews + PositiveServiceReview, data=trainingG,
                     method = "lm", 
                     trControl = fitControl,
                     tuneLength = 3,
                     verboseIter = TRUE)

LM_2_prediction<-predict(LM_2 , validationG)
MetricsLM_2<-postResample(validation$Volume,LM_2_prediction)



##################################Comparing Models#########################################
ModelSampels<- resamples(list(RForrest_All=modelRF1,
                           RForrest_2var=modelRF2,
                           RForrest_3var=modelRF3,
                           XGBT_All=modelXGBT1,
                           XGBT_2var=modelXGBT2,
                           LM_All=LM_1,
                           LM_2_var=LM_2))
ModelSampels$values
summary(ModelSampels)
bwplot(ModelSampels,metric="RMSE",
       main="Models RMSE")

bwplot(ModelSampels,metric="Rsquared",
       main="Models Rsquared")

bwplot(ModelSampels,metric="MAE",
       main="Models MAE")


#Randome Forest with 3 var , performs best from all the models 


#############################Testing stage######################################################



dummyNew <- dummyVars("~.", data = New_Products)
dummyData_New <- data.frame(predict(dummyNew, newdata = New_Products))
str(dummyData)
summary(dummyData_New)


dummyData_New$BestSellersRank<-NULL
dummyData_New$ProductNum<-NULL
dummyData_New$x5StarReviews<-NULL


FinalPrediction <- predict(modelRF3, newdata =dummyData_New) 



New_Products$Volume<-FinalPrediction
summarise(New_Products)



Four_catgeories<-   dplyr::select(New_Products,ProductType,ProductNum,ProfitMargin,Volume)
Four_catgeories <- filter(Four_catgeories,ProductType %in% c("PC","Laptop","Smartphone","Netbook"))
Four_catgeories<-filter(Four_catgeories,ProductNum%in% c("171","173","180","194"))

Four_catgeories$ProductNum<-as.factor(c("Dell PC","Apple Laptop","Acer Netbook","Samsung Phone"))
levels(Four_catgeories$ProductNum)
table(Four_catgeories$Volume,Four_catgeories$ProductNum)

New<-plot_ly(New_Products, x = ~ProductType, y = ~Volume,  type="scatter", mode = "markers", 
             marker=list( size=10 , opacity=0.5)  )

New2<-plot_ly(Four_catgeories, x = ~ProductNum, y = ~Volume,  type="scatter", mode = "markers", 
             marker=list( size=10 , opacity=0.5)  )


New3<-plot_ly(Four_catgeories, x = ~ProductNum, y = ~Volume,  type="bar", 
              marker=list( size=10 , opacity=0.5))

New4<-plot_ly(Four_catgeories, x = ~ProductNum, y = ~Volume,  type="histogram2dcontour", 
              marker=list( size=10 , opacity=0.5))



# Commands for type=
# scatter', 'bar', 'box', 'heatmap', 'histogram', 'histogram2d', 'histogram2dcontour', 
# 'pie', 'contour', 'scatterternary', 'violin', 'scatter3d', 'surface', 'mesh3d', 
# 'cone', 'streamtube', 'scattergeo', 'choropleth', 'scattergl', 'splom', 
# 'pointcloud', 'heatmapgl', 'parcoords', 'scattermapbox', 'sankey', 'table', 
# 'carpet', 'scattercarpet', 'contourcarpet',
# 'ohlc', 'candlestick', 'scatterpolar', 'scatterpolargl', 'area'

