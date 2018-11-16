# Régression logistique - Titanic


#Importation des données (avec imputation kNN)
data_train <- read.csv('train01.csv')
data_test <- read.csv('test01.csv')

NA_train0 <- read.csv('trainNA.csv')
NA_test0 <- read.csv('test01.csv')

    ########### Différents modèles ###########

#Utilisation de XGBoost
library("xgboost")
library("Matrix")
NA_train <- as.matrix(NA_train0[,3:10])
NA_train <- as(NA_train, "dgCMatrix")
xgb1 <- xgboost(data = NA_train, label=NA_train0[,2], max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
NA_test <- as.matrix(NA_test0)
NA_test <- as(NA_test, "dgCMatrix")
pred <- predict(xgb1, NA_test[,2:9])
survived.pred <- cbind(NA_test[,1],data.frame(round(pred)))
colnames(survived.pred) <- c('PassengerId','Survived')



#ou Création d'un modèle de forêt aléatoire
library(randomForest)
rndfor <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked.S + Embarked.C, 
                    ntree=5000, mty=1,
                    data = data_train, na.action = na.roughfix)
rndfor
#0.1281 et 45.84
#0.1268 et 46.36
#0.1267 et 46.42
survived.pred <- predict(rndfor, type = "response", newdata = data_test)
head(survived.pred)

survived.pred <- cbind(data_test[,1],data.frame(round(survived.pred)))
colnames(survived.pred) <- c('PassengerId','Survived')



#ou Création du modèle de régression logistique
logreg <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked.S + Embarked.C, 
          data = data_train, family = binomial(logit))
logreg

#On étudie la significativité des variables
drop1(logreg, test = "Chisq")
#Conclusion: les variables Parch, Fare, et Embarked ne sont
#pas significative.

#Sélection du modèle de régression logistique optimal
#par procédure de minimisation de l'AIC:
logreg_opti <- step(logreg)
logreg_opti

#Prédictions réalisées sur le jeu de test
survived.pred <- predict(logreg_opti, type = "response", newdata = data_test)
head(survived.pred)

survived.pred <- cbind(data_test[,1],data.frame(round(survived.pred)))
colnames(survived.pred) <- c('PassengerId','Survived')


    ############## Exporter en .csv ##############

write.csv(survived.pred, file="result.csv", quote=F, row.names=FALSE)



