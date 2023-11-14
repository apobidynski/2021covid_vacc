set.seed(72685)

##################### LADOWANIE DANYCH
data <- read.csv("data.csv", sep=";", header=TRUE)
sum(is.na(data))

data$RECOVD <- as.numeric(data$RECOVD)
data$L_THREAT <- as.factor(data$L_THREAT)
data$HOSPITAL <- as.factor(data$HOSPITAL)
data$DISABLE <- as.factor(data$DISABLE)
data$OFC_VISIT <- as.factor(data$OFC_VISIT)
data$ER_ED_VISIT <- as.factor(data$ER_ED_VISIT)

rand <- sample(1:nrow(data),0.7*nrow(data))
train <- data[rand,]
test <- data[-rand,]
rm(rand)

##### BUDOWA MODELI
reg_initial = glm(RECOVD ~ ., data = train, family = binomial('logit'))
summary(reg_initial)

reg_base <- glm(RECOVD ~ 1, data = train, family = binomial('logit'))

#AIC JAKO KRYTERIUM
reg_step <- step(reg_initial, trace = T, scope = list(lower=formula(reg_base), upper=formula(reg_initial)), direction = 'both')
reg_step$aic #98829,8
formula(reg_step)

#usuniecie vadminby
reg_test_1 <- glm(RECOVD ~ VAX_MANU + VAX_DOSE_SERIES + VAX_ROUTE + VAX_SITE + 
                    AGE_YRS + SEX + HOSPITAL + HOSPDAYS + DISABLE + 
                    NUMDAYS +  OFC_VISIT + ER_ED_VISIT, data = train, family = binomial('logit'))

reg_test_1$aic #98926,69
anova(reg_step, reg_test_1, test="LRT") #różnica istotna, zostawiamy zmienna

#usuniecie vaxmanu
reg_test_2 <- glm(RECOVD ~  VAX_DOSE_SERIES + VAX_ROUTE + VAX_SITE + 
                    AGE_YRS + SEX + HOSPITAL + HOSPDAYS + DISABLE + 
                    NUMDAYS + V_ADMINBY + OFC_VISIT + ER_ED_VISIT, data = train, family = binomial('logit'))

reg_test_2$aic #98899.29
anova(reg_step, reg_test_2, test="LRT")

#usuniecie vaxdoseseries

reg_test_3 <- glm(RECOVD ~  VAX_MANU + VAX_ROUTE + VAX_SITE + 
                    AGE_YRS + SEX + HOSPITAL + HOSPDAYS + DISABLE + 
                    NUMDAYS + V_ADMINBY + OFC_VISIT + ER_ED_VISIT, data = train, family = binomial('logit'))

reg_test_3$aic #98858.52
anova(reg_step, reg_test_3, test="LRT")

#BIC JAKO KRYTERIUM
reg_bic <- step(reg_initial, trace = T, k = log(nrow(train)), scope = list(lower=formula(reg_base), upper=formula(reg_initial)), direction = 'both')
reg_prob_bic <- predict(reg_bic, test, type = 'response')
reg_prob_bic
reg_pred_bic <- rep(0, length(reg_prob_bic))
reg_pred_bic[reg_prob_bic>=0.5] <- 1

reg_cm_b <- table(reg_pred_bic, test$RECOVD)
reg_acc_b= (reg_cm_b[1,1]+reg_cm_b[2,2])/sum(reg_cm_b)
reg_prec_b = reg_cm_b[2,2]/sum(reg_cm_b[,2])
reg_rec_b = reg_cm_b[2,2]/sum(reg_cm_b[2,])
reg_tnr_b =  reg_cm_b[1,1]/sum(reg_cm_b[1,])

reg_acc_b
#63,04036%
reg_tnr_b
#65.00585%

# PODEJSCIE KOSZTOWE
CalculateCost = function(cut.off, cost.matrix, score, true.y){
  prediction = ifelse(score > cut.off, 1, 0)
  confusion.matrix = prop.table(table(factor(prediction, levels = c(0, 1)),
                                      true.y))
  return(sum(cost.matrix * confusion.matrix))
}
score = costs = list()

CUT_OFFS = seq(0, 1, by = 0.01) 
SEVERE_ADVERSE_COST = 5
LOST_VACCINE_RECIPIENT = 1
COST_MATRIX = matrix(c(0, SEVERE_ADVERSE_COST, LOST_VACCINE_RECIPIENT, 0), 2)

#NA TRENINGOWYM
score[[2]] = predict(reg_step, type = "response")
costs[[2]] = sapply(CUT_OFFS, CalculateCost, cost.matrix = COST_MATRIX,
                    score = score[[2]], true.y = train$RECOVD)

#NA TESTOWYM
score[[1]] = predict(reg_step, newdata = test, type = "response")
costs[[1]] = sapply(CUT_OFFS, CalculateCost, cost.matrix = COST_MATRIX,
                    score = score[[1]], true.y = test$RECOVD)

cost_reg_pred <- rep(0, length(score[[1]]))
cost_reg_pred[score[[1]]>=0.75] <- 1
cost_reg_cm <- table(cost_reg_pred, test$RECOVD)
cost_reg_acc= (cost_reg_cm[1,1]+cost_reg_cm[2,2])/sum(cost_reg_cm)
cost_reg_prec = cost_reg_cm[2,2]/sum(cost_reg_cm[,2])
cost_reg_rec = cost_reg_cm[2,2]/sum(cost_reg_cm[2,])
cost_reg_tnr =  cost_reg_cm[1,1]/sum(cost_reg_cm[1,])

cost_reg_acc
#59,95815%
cost_reg_tnr
#59.95695%

plot(data.frame(CUT_OFFS, 1.1), type = "l", lty = 3, log="y",
     ylim = range(c(1.1, unlist(costs))),
     ylab = "Koszt per pacjent", xlab = "Punkt odcięcia",
     main = "Wybór optymalnego punktu odcięcia w podejściu kosztowym")
for (i in 1:2) {
  lines(CUT_OFFS, costs[[i]], lty = i, lwd = 2)
  points(CUT_OFFS[which.min(costs[[i]])], min(costs[[i]]),
         pch = 19, cex = 1.3)
}

legend("topright", c("Zbiór testowy", "Zbiór treningowy", "Model losowy"),
       lty = c(1, 2, 3), cex = .7, ncol = 3,
       lwd = c(2, 2, 1))

##################################################################################################

reg_prob <- predict(reg_test_3, test, type = 'response')
reg_prob
reg_pred <- rep(0, length(reg_prob))
reg_pred[reg_prob>=0.5] <- 1


reg_cm <- table(reg_pred, test$RECOVD)
reg_acc= (reg_cm[1,1]+reg_cm[2,2])/sum(reg_cm)
reg_prec = reg_cm[2,2]/sum(reg_cm[,2])
reg_rec = reg_cm[2,2]/sum(reg_cm[2,])
reg_tnr =  reg_cm[1,1]/sum(reg_cm[1,])

reg_acc
reg_tnr
reg_test_3$aic
library(pROC)
auc(test$RECOVD, cost_reg_pred)
cost_reg_pred
###################################################################################################
reg <- reg_test_3
library(ROCR)
score.or.class = gain = lift = roc = auc = prediction.object = list()
score.or.class[[1]] = list(test$RECOVD, test$RECOVD)
score.or.class[[2]] = list(predict(reg, type = "response"),
                           train$RECOVD)
score.or.class[[3]] = list(predict(reg, new = test, "response"),
                           test$RECOVD)
class.average = mean(test$RECOVD)
random.class = 1
for (i in 1:(nrow(test) - 1)) {
  random.class = c(random.class, mean(random.class) < class.average)
}
score.or.class[[4]] = list(seq(0, 1, len = nrow(test)), random.class)
for (i in 1:length(score.or.class)) {
  prediction.object[[i]] = prediction(score.or.class[[i]][[1]],
                                      score.or.class[[i]][[2]])
  gain[[i]] = performance(prediction.object[[i]], "tpr", "rpp")
  lift[[i]] = performance(prediction.object[[i]], "lift", "rpp")
  roc[[i]] = performance(prediction.object[[i]], "tpr", "fpr")
  auc[[i]] = performance(prediction.object[[i]], "auc")
}
LEGEND_LABELS = c("Model idealny", "Zbiór treningowy", "Zbiór testowy", "Model losowy")
#Creating plots
ShowCurve = function(list, name, AUC = FALSE, legend.position = "right") {
  for (i in 1:length(list)) {
    plot(list[[i]], main = paste(name, " curve"),
         col = gray((i - 1) / 4), lwd = 2, add = (i != 1), xlim = c(0, 1))
    if (AUC) {
      text(.2, 0.9 - i * 0.1, pos = 4, col = gray((i - 1) / 4), cex = .9,
           paste("AUC =", round(auc[[i]]@y.values[[1]], digit = 2)))
    }
  }
  legend(legend.position, lty = 2, lwd = 2, col = gray(0:3 / 4),
         y.intersp = .6, legend = LEGEND_LABELS, seg.len = 0.6, bty = "n")
}
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
ShowCurve(gain, "Gain")
ShowCurve(lift, "Lift", legend.position = "topright")
ShowCurve(roc, "ROC", AUC = TRUE)
#
#Conditional density function
class0.score.density = class1.score.density = list()
max.density = 0
for(i in 1:(length(prediction.object))) {
  predictions = prediction.object[[i]]@predictions[[1]]
  labels = prediction.object[[i]]@labels[[1]]
  class0.score.density[[i]] = density(predictions[labels == "0"],
                                      kernel =  "epanechnikov", bw = 0.05)
  class1.score.density[[i]] = density(predictions[labels == "1"],
                                      kernel =  "epanechnikov", bw = 0.05)
  max.density = max(class0.score.density[[i]]$y,
                    class1.score.density[[i]]$y, max.density)
}   
plot(0, 0, type = "n", xlim = c(-0.1, 1.1), ylim = c(0, max.density),
     xlab = "Score", ylab = "Density function value",
     main = "Conditional score density functions")
for(i in 1:length(prediction.object)) {
  lines(class0.score.density[[i]], col = gray((i - 1) / 4), lwd = 2)
  lines(class1.score.density[[i]], col = gray((i - 1) / 4), lwd = 2, lty = 2)
}
legend("top", lty = 1, lwd = 2, col = gray(0:3 / 4), y.intersp = .6,
       legend = LEGEND_LABELS, seg.len = 0.5, bty = "n")

summary(reg)
