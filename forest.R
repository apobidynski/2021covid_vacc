set.seed(72685)

##################### LADOWANIE DANYCH
data <- read.csv("data.csv", sep=";", header=TRUE)
sum(is.na(data))

data$RECOVD <- as.factor(data$RECOVD)
data$L_THREAT <- as.factor(data$L_THREAT)
data$HOSPITAL <- as.factor(data$HOSPITAL)
data$DISABLE <- as.factor(data$DISABLE)
data$OFC_VISIT <- as.factor(data$OFC_VISIT)
data$ER_ED_VISIT <- as.factor(data$ER_ED_VISIT)

rand <- sample(1:nrow(data),0.7*nrow(data))
train <- data[rand,]
test <- data[-rand,]
rm(rand)

################################################# RANDOM FOREST

library(randomForest)
library(pROC)
rf_initial = randomForest(RECOVD~., data=train, do.trace=T)
rf_initial

initial_pred <- as.numeric(predict(rf_initial, newdata=test))

initial_cm <-table(initial_pred, test$RECOVD)
initial_acc= (initial_cm[1,1]+initial_cm[2,2])/sum(initial_cm)
initial_prec = initial_cm[2,2]/sum(initial_cm[,2])
initial_rec = initial_cm[2,2]/sum(initial_cm[2,])
initial_tnr =  initial_cm[1,1]/sum(initial_cm[1,])

initial_acc #61,71001%
initial_tnr #64,74106%
auc(test$RECOVD, initial_pred) #0,571

varImpPlot(rf_initial, main = "Ważność zmiennych w modelu lasu startowego")


###########

rf_optimal <- tuneRF(subset(train, select = -c(RECOVD)), train$RECOVD, ntreeTry=1000, 
                     mtryStart=3, stepFactor=2, improve=0.1, trace=T, plot=F, doBest = T)

optimal_pred <- as.numeric(predict(rf_optimal, newdata=test))

optimal_cm <-table(optimal_pred, test$RECOVD)
optimal_acc= (optimal_cm[1,1]+optimal_cm[2,2])/sum(optimal_cm)
optimal_prec = optimal_cm[2,2]/sum(optimal_cm[,2])
optimal_rec = optimal_cm[2,2]/sum(optimal_cm[2,])
optimal_tnr =  optimal_cm[1,1]/sum(optimal_cm[1,])

optimal_acc #63,142% 
optimal_tnr #64,76231
auc(test$RECOVD, optimal_pred) #0,5783

varImpPlot(rf_optimal, main = "Ważność zmiennych w modelu lasu optymalizowanego")

###################################################### 

###EXTRA TREES
install.packages("extraTrees")
library(extraTrees)
library(rJava)
library(pROC)

#options(java.parameters = "-Xmx2g")

start<-Sys.time()
et_model = extraTrees(x=data.matrix(train[,-11]), y = as.factor(data.matrix(train[,11])),ntree=1000, mtry=7)
end<-Sys.time()

et_pred = predict(et_model, newdata=data.matrix(test[,-11]))

et_cm <-table(et_pred, test$RECOVD)
et_acc= (et_cm[1,1]+et_cm[2,2])/sum(et_cm)
et_prec = et_cm[2,2]/sum(et_cm[,2])
et_rec = et_cm[2,2]/sum(et_cm[2,])
et_tnr =  et_cm[1,1]/sum(et_cm[1,])

et_acc 
et_tnr 
auc(test$RECOVD, as.numeric(et_pred))
  
