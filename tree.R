
set.seed(72685)

library(rpart)
library(tidyverse)
library(pROC)
library(dplyr)

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

rpart_initial = rpart(RECOVD~., data=train, cp =-1, method = 'class')

initial_pred = predict(rpart_initial, newdata=test)
initial_pred <- as.data.frame(initial_pred)
initial_pred <- initial_pred %>% 
  rename(
    "v0" = "0",
    "v1" = "1"
  )

initial_pred$predict <- ifelse(initial_pred$v0 > 0.5, 0, 1)

initial_cm <-table(initial_pred$predict, test$RECOVD)
initial_acc= (initial_cm[1,1]+initial_cm[2,2])/sum(initial_cm)
initial_prec = initial_cm[2,2]/sum(initial_cm[,2])
initial_rec = initial_cm[2,2]/sum(initial_cm[2,])
initial_tnr =  initial_cm[1,1]/sum(initial_cm[1,])

initial_acc #53,78176%
initial_tnr #61,20131%
auc(test$RECOVD, initial_pred$predict) #0,5159

initial_imp <- data.frame(imp = rpart_initial$variable.importance)
initial_imp <- initial_imp %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(initial_imp) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw() +
  ylab("Istotność") +
  xlab("Zmienna") + 
  ggtitle("Istotność zmiennych w początkowym drzewie decyzyjnym", subtitle = waiver())

rpart_initial$control
#########################

rpart_param <- rpart(RECOVD~., data=train, method = 'class', cp=0, maxdepth=5, minbucket = 100, minsplit = 300, xval=20)

param_pred = predict(rpart_param, newdata=test)
param_pred <- as.data.frame(param_pred)
param_pred <- param_pred %>% 
  rename(
    "v0" = "0",
    "v1" = "1"
  )

param_pred$predict <- ifelse(param_pred$v0 > 0.5, 0, 1)

param_cm <-table(param_pred$predict, test$RECOVD)
param_acc= (param_cm[1,1]+param_cm[2,2])/sum(param_cm)
param_prec = param_cm[2,2]/sum(param_cm[,2])
param_rec = param_cm[2,2]/sum(param_cm[2,])
param_tnr =  param_cm[1,1]/sum(param_cm[1,])

param_acc #63,07623%
param_tnr #66,34281%
auc(test$RECOVD, param_pred$predict) #59,37%

param_imp <- data.frame(imp = rpart_param$variable.importance)
param_imp <- param_imp %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(param_imp) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw() +
  xlab("Istotność") +
  ylab("Zmienna") + 
  ggtitle("Istotność zmiennych w ręcznie strojonym drzewie decyzyjnym", subtitle = waiver())

rpart_param$control
###########################

cp_optimal = rpart_initial$cptable[which(rpart_initial$cptable[,"xerror"]==min(rpart_initial$cptable[,"xerror"])),"CP"]
cp_optimal
rpart_optimal = prune(rpart_initial, cp_optimal)

optimal_pred = predict(rpart_optimal, newdata=test)
optimal_pred <- as.data.frame(optimal_pred)
optimal_pred <- optimal_pred %>% 
  rename(
    "v0" = "0",
    "v1" = "1"
  )

optimal_pred$predict <- ifelse(optimal_pred$v0 > 0.5, 0, 1)
optimal_cm <-table(optimal_pred$predict, test$RECOVD)
optimal_acc= (optimal_cm[1,1]+optimal_cm[2,2])/sum(optimal_cm)
optimal_prec = optimal_cm[2,2]/sum(optimal_cm[,2])
optimal_rec = optimal_cm[2,2]/sum(optimal_cm[2,])
optimal_tnr =  optimal_cm[1,1]/sum(optimal_cm[1,])

optimal_acc #63,429%
optimal_tnr #65,95805%
auc(test$RECOVD, optimal_pred$predict) #0,5916

optimal_imp <- data.frame(imp = rpart_optimal$variable.importance)
optimal_imp <- optimal_imp %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(optimal_imp) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw() +
  xlab("Istotność") +
  ylab("Zmienna") + 
  ggtitle("Istotność zmiennych drzewie decyzyjnym z optymalizowanym cp", subtitle = waiver())
#################################################
tree <- rpart_param

tree.preds <- predict(tree, test, type="prob")[, 2]
tree.roc <- roc(test$RECOVD, tree.preds)
print(tree.roc)
plot(tree.roc, main="Krzywa ROC dla drzewa klasyfikacyjnego",
     xlab = "Specyficzność", ylab="Czułość",
     xlim = c(1,0))
title("ROC", line = 2.5)
plot(x, y, main="title", sub="subtitle",
     xlab="X-axis label", ylab="y-axix label",
     xlim=c(xmin, xmax), ylim=c(ymin, ymax))
######################################################

