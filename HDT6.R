getwd()
setwd("D:/UVG/2022/Semestre 1 2022/Mineria de datos/HDT5")
df_test <- read.csv("test.csv")
df_train<- read.csv("train.csv")
df_test2 <- read.csv("sample_submission.csv")

df_
library(ggplot2)
library (dplyr)
library(naivebayes)
library(psych)


library(caret)
library(e1071)
library(klaR)

##limites de var categorica tipo de casa barata/ mediana / cara
summary(df_train$SalePrice)
priceRange <- max(df_train$SalePrice)-min(df_train$SalePrice)
baratoMax<- min(df_train$SalePrice)+(priceRange/3)
medianoMax <- baratomax+(priceRange/3)
caroMax<-max(df_train$SalePrice)
max(df_train$SalePrice)
(medianoMax)
baratoMax
df_train['tipoDeCasa']<- ifelse(df_train$SalePrice<baratoMax,"BARATA",ifelse(df_train$SalePrice>=baratoMax & df_train$SalePrice<medianoMax,"MEDIA","CARA"))
df_train_filtered<-df_train[,c(2,19,20,35,45,48,52,71,82)]
df_train_filtered<-df_train[,c(81,82)]
## test no tiene saleprice entonces debemos unirlo con sample submission.csv
df_test['SalePrice']<-df_test2$SalePrice
df_test['tipoDeCasa']<-ifelse(df_test2$SalePrice<baratoMax,"BARATA",ifelse(df_test2$SalePrice>=baratoMax & df_test2$SalePrice<medianoMax,"MEDIA","CARA"))
df_test_filtered<-df_test[,c(2,19,20,35,45,48,52,71,82)]



## parte de prueba 
df_train_filtered<-df_train[,c(81,82)]
df_train_filtered$tipoDeCasa <-df_train_filtered$tipoDeCasa

df_test_filtered<-df_test[,c(81,82)]
df_test_filtered$tipoDeCasa <-df_test_filtered$tipoDeCasa



modelo<-naiveBayes(tipoDeCasa~., data=df_train_filtered)
predBayes<-predict(modelo, newdata = df_test_filtered[,c(1)])



cm<-caret::confusionMatrix(predBayes, df_test_filtered$tipoDeCasa)
predBayes






levels(df_test_filtered$tipoDeCasa)<-levels(predBayes)
df_test_filtered$tipoDeCasa