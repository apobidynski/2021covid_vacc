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

################################################# 

library(nnet)
library(pROC)

nn <- nnet(RECOVD ~ ., data = train, size = 22, maxit = 2000)

nn_pred <- predict(nn, newdata = test, type = 'raw')
pred <- rep('0', length(nn_pred))
pred[nn_pred>=.5] <- '1'

nn_cm <- table(pred, test$RECOVD)
nn_acc= (nn_cm[1,1]+nn_cm[2,2])/sum(nn_cm)
nn_prec = nn_cm[2,2]/sum(nn_cm[,2])
nn_rec = nn_cm[2,2]/sum(nn_cm[2,])
nn_tnr =  nn_cm[1,1]/sum(nn_cm[1,])

nn_acc #60,79522%
nn_tnr #64,8217%
auc(test$RECOVD, as.numeric(pred)) #0.5709

#######################################################

nn <- nnet(RECOVD ~ ., data = train, size = 13, maxit = 1000)

nn_pred <- predict(nn, newdata = test, type = 'raw')
pred <- rep('0', length(nn_pred))
pred[nn_pred>=.5] <- '1'

nn_cm <- table(pred, test$RECOVD)
nn_acc= (nn_cm[1,1]+nn_cm[2,2])/sum(nn_cm)
nn_prec = nn_cm[2,2]/sum(nn_cm[,2])
nn_rec = nn_cm[2,2]/sum(nn_cm[2,])
nn_tnr =  nn_cm[1,1]/sum(nn_cm[1,])

nn_acc #62,03587%
nn_tnr #65,42397%
auc(test$RECOVD, as.numeric(pred)) #0.5812

#######################################################

nn <- nnet(RECOVD ~ ., data = train, size = 3, maxit = 1000)

nn_pred <- predict(nn, newdata = test, type = 'raw')
pred <- rep('0', length(nn_pred))
pred[nn_pred>=.5] <- '1'

nn_cm <- table(pred, test$RECOVD)
nn_acc= (nn_cm[1,1]+nn_cm[2,2])/sum(nn_cm)
nn_prec = nn_cm[2,2]/sum(nn_cm[,2])
nn_rec = nn_cm[2,2]/sum(nn_cm[2,])
nn_tnr =  nn_cm[1,1]/sum(nn_cm[1,])

nn_acc #63,22272%
nn_tnr #65,29372%,
auc(test$RECOVD, as.numeric(pred)) #0.5842
