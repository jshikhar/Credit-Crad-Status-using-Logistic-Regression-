data_CC<-rbind(ds1,ds2,ds3,ds4)
View(data_CC)

write.csv(data_CC,"data_CC")
getwd()


summary(data_CC)
## missing value tretament for card-offer
library(modeest)
mode<-mlv(data_CC$card_offer)
mode

library(Hmisc)
data_CC$card_offer<-as.logical(impute(data_CC$card_offer,mode)) 
summary(data_CC)


## checking for OT
boxplot(data_CC[,-c(1,2,3,4,5,12)])


## It is identified that we have OT in v(6,7,9)
#hold_bal
x <-data_CC$hold_bal
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- (qnt[1] - H)
x[x > (qnt[2] + H)] <- (qnt[2] + H)
data_CC$hold_bal<-x
#pref_cust_prob
x <-data_CC$pref_cust_prob
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- (qnt[1] - H)
x[x > (qnt[2] + H)] <- (qnt[2] + H)
data_CC$pref_cust_prob<-x
#RiskScore
x <-data_CC$RiskScore
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- (qnt[1] - H)
x[x > (qnt[2] + H)] <- (qnt[2] + H)
data_CC$RiskScore<-x


## CONVERTUNG RESPONSE VARIKE TO FACTOR TYPE 
data_CC$card_offer<-factor(data_CC$card_offer)
class(data_CC$card_offer)

##data partition 


## 75% of the sample size
smp_size <- floor(0.75 * nrow(data_CC))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_CC)), size = smp_size)

train <- data_CC[train_ind, ]
test <- data_CC[-train_ind, ]

## creating logistic model 

#logit_model <- glm(card_offer ~ ., data = train, family = "binomial")
summary(logit_model)
ncol(train)
logit_model2 <- glm(card_offer ~ ., data = train[,-c(1,4,9,10,11)], family = "binomial")
#logit_model3 <- glm(card_offer ~ ., data = train[,-c(1,2,4,9,10,11)], family = "binomial")
## logit_model2 is the best model 
## predicting values 

predictions<-predict(logit_model2, test,type = "response" )
head(predictions)
View(predictions)


combined_data<-cbind(test,predictions)


combined_data$response <- as.factor(ifelse(combined_data$predictions>0.1, TRUE, FALSE))
str(combined_data)
View(combined_data)



## confusion matix 
library(caret)
conf_matrix<-confusionMatrix(combined_data$response,combined_data$card_offer)


conf_matrix


## ROC 
library(ROCR)
logit_scores <- prediction(predictions=combined_data$predictions, labels=combined_data$card_offer)
logit_perf <- performance(logit_scores, "tpr", "fpr")

#plotting the ROC curve
plot(logit_perf,col = "darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA, main="ROC Curve")
box()
abline(0,1, lty = 300, col = "green")
grid(col="aquamarine")

# Calculating the Area Under Curve (AUC)

logit_auc <- performance(logit_scores, "auc")
as.numeric(logit_auc@y.values)  ##AUC Value

#Calculating the KS Values

logit_ks <- max(logit_perf@y.values[[1]]-logit_perf@x.values[[1]])
logit_ks

## LCalculating the Lift

# Since the base is 0,creating the probablity for class 1
combined_data$predictions_1<-1-combined_data$predictions
str(combined_data)

table(combined_data$y)
trellis.par.set(caretTheme())
caretLift = caret::lift(y ~ predictions_1,
                        data = combined_data, cuts = 101)
plot(caretLift, values = 60, auto.key = list(columns = 3,
                                             lines = TRUE,
                                             points = FALSE))




#Finally exporting the prediction data 

write.csv(combined_data,"final_probs_test.csv")



