#IMPUTATION DE DONNEES MANQUANTES

    #Données train.csv

library("FactoMineR")
library("factoextra")
library("Amelia")
library("VIM")

data0 <- read.csv(file="train.csv", 
                  quote= "\"", dec = ".", header = TRUE)
data<-data0[,c(1,2,3,5,6,7,8,10,12,12)]
head(data)
#Mettre les variables qualitatives sous forme binaire
data$Sex<-ifelse(data$Sex=="male", yes = 1, no = 0)
data$Embarked<-ifelse(data$Embarked=="S", yes = 1, no = 0)
data$Embarked.S<-data$Embarked
data$Embarked.1<-ifelse(data$Embarked.1=="C", yes = 1, no = 0)
data$Embarked.C<-data$Embarked.1
data<-data[,-c(9,10)]
head(data)
dat<-cbind(data[,1:2],scale(data[,3:10]))
head(dat)

#Méthode KNN
dat.kNN=kNN(dat, k=6, imp_var=FALSE)
head(dat.kNN)
#ou Méthode Amelia (EM et Bootstrap)
dat.amelia=amelia(dat,m=1)$imputations$imp1
#ou Utilisation des données complètes
dat.NA<-na.omit(dat)

dat.NA<-round(dat.NA,digits=3)
head(dat.NA)
dat.NA <- data.frame(dat.NA)
colnames(dat.NA)[1]<-'PassengerId'
colnames(dat.NA)[2]<-'Survived'


write.csv(dat.amelia,file='train01.csv',row.names=F)
write.csv(dat.NA,file='trainNA.csv',row.names=F)


    #Données test.csv

library("FactoMineR")
library("factoextra")
library("Amelia")
library("VIM")

data0 <- read.csv(file="test.csv", 
                  quote= "\"", dec = ".", header = TRUE)
data<-data0[,c(1,2,4,5,6,7,9,11,11)]
head(data)
#Mettre les variables qualitatives sous forme binaire
data$Sex<-ifelse(data$Sex=="male", yes = 1, no = 0)
data$Embarked<-ifelse(data$Embarked=="S", yes = 1, no = 0)
data$Embarked.S<-data$Embarked
data$Embarked.1<-ifelse(data$Embarked.1=="C", yes = 1, no = 0)
data$Embarked.C<-data$Embarked.1
data<-data[,-c(8,9)]
head(data)
dat<-cbind(data[,1],scale(data[,2:9]))
head(dat)

#Méthode KNN
dat.kNN=kNN(dat, k=6, imp_var=FALSE)
head(dat.kNN)
#ou Méthode Amelia (EM et Bootstrap)
dat.amelia=amelia(dat,m=1)$imputations$imp1
#ou Utilisation des données complètes
dat.NA<-na.omit(dat)

dat.NA<-round(dat.NA,digits=3)
head(dat.NA)
dat.NA <- data.frame(dat.NA)
colnames(dat.NA)[1]<-'PassengerId'


write.csv(dat.amelia,file='test01.csv',row.names=F)
write.csv(dat.NA,file='testNA.csv',row.names=F)



