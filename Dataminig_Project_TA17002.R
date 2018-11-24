#setwd("D:\\Rajib\\XLRI\\datamining\\Project\\")
setwd("C:\\Software\\xlri\\Datamining\\Resource\\Project\\WIP")
getwd()
#install.packages("party")
library(MASS)
library(corrplot)
library(car)
library(caTools)
library(ROCR)
library(rpart) # load libraries
library("rpart.plot")
library(ggplot2)
library(randomForest)
library(party)
library(gridExtra)


consumercomplain<-read.csv("Consumer_Complaints.csv",header=TRUE, na.strings=c("","NA"))
str(consumercomplain)



#Explanatory Data analysis -Generate bar plot

p1 <- ggplot(consumercomplain, aes(x=consumercomplain$Tags)) + ggtitle("Tags") + xlab("Tags") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(consumercomplain, aes(x=consumercomplain$Submitted.via)) + ggtitle("Submitted.via") + xlab("Submitted.via") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(consumercomplain, aes(x=consumercomplain$ Company.response.to.consumer)) + ggtitle("Company.response.to.consumer") + xlab("Company.response.to.consumer") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(consumercomplain, aes(x=consumercomplain$Timely.response.)) + ggtitle("Timely.response.") + xlab("Timely.response.") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p5 <- ggplot(consumercomplain, aes(x=consumercomplain$ Consumer.disputed.)) + ggtitle("Consumer.disputed.") + xlab("Consumer.disputed.") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(consumercomplain, aes(x=consumercomplain$Consumer.consent.provided)) + ggtitle("Consumer.consent.provided") + xlab("Consumer.consent.provided") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4,p5,p6, ncol=2)

consumercomplain[consumercomplain=="N/A"] <- "NA"



#Mode function

Mode <- function (x, na.rm) {
    xtab <- table(x)
    xmode <- names(which(xtab == max(xtab)))
    if (length(xmode) > 1) xmode <- ">1 mode"
    return(xmode)
}

for (var in 1:ncol(consumercomplain))
  {
   if (class(consumercomplain[,var])=="numeric")
     {
       consumercomplain[is.na(consumercomplain[,var]),var] <- mean(consumercomplain[,var], na.rm = TRUE)
    
      } else if (class(consumercomplain[,var]) %in% c("character", "factor")) 
        {
          consumercomplain[is.na(consumercomplain[,var]),var] <- Mode(consumercomplain[,var], na.rm = TRUE)
      }
}


#Generate dummy variables

consumercomplain$Consumer.consent.provided.[consumercomplain$Consumer.consent.provided.=="NA"] <- "Consent provided"
consumercomplain$Timely.response<- ifelse(consumercomplain$Timely.response. == "Yes", 1, 0)
consumercomplain$Consumer.disputed_Y<-ifelse(consumercomplain$Consumer.disputed.=='Yes',1,0)
consumercomplain$Tags_Service <- ifelse(consumercomplain$Tags == "Older American", 1, 0)
consumercomplain$Consumer.consent.provided <- ifelse(consumercomplain$Consumer.consent.provided. == "Consent provided", 1, 0)
consumercomplain$Submitted.via[consumercomplain$Submitted.via=="Web"]<-"Email"




for(level in unique(consumercomplain$Submitted.via)){
  
  consumercomplain[paste("Submitted.via",   gsub("-","_", gsub(" ","_",level, fixed=TRUE), fixed=TRUE), sep = "_")] <- ifelse(consumercomplain$Submitted.via == level, 1, 0)
  
}



#Remove unwanted columns

consumercomplain$Timely.response. <- NULL
consumercomplain$Date.received<-NULL
consumercomplain$Company.response.to.consumer<-NULL
consumercomplain$Submitted.via<-NULL
consumercomplain$Consumer.consent.provided.<-NULL
consumercomplain$Tags<-NULL
consumercomplain$Consumer.disputed.<-NULL
consumercomplain$Sub.product<-NULL
consumercomplain$Sub.issue<-NULL
consumercomplain$Consumer.complaint.narrative<-NULL
consumercomplain$ZIP.code<-NULL
consumercomplain$Date.sent.to.company<-NULL

####Split data
set.seed(200)
splitdata<-sample.split(consumercomplain,SplitRatio=.7)
consumercomplaintrainig<-subset(consumercomplain,splitdata==TRUE)
consumercomplaintesting<-subset(consumercomplain,splitdata==FALSE)
#Logistic regression

res <-glm(Consumer.disputed_Y~
          Product         
          +Company.public.response
          +Timely.response
          +Tags_Service
          +Consumer.consent.provided
          +Submitted.via_Referral
          +Submitted.via_Postal_mail
          +Submitted.via_Phone
          +Submitted.via_Fax
          ,data=consumercomplain,family = "binomial")

#the linearly dependent variables

ld.vars <- attributes(alias(res)$Complete)$dimnames[[1]]
ld.vars
print(summary(res))
#Check for multicollinearity
vif(res)
#gc()

# Model Definition
churn_logistic<-glm(Consumer.disputed_Y~
                    Product         
                     +Company.public.response
                    +Timely.response
                    +Tags_Service
                    +Consumer.consent.provided
                    +Submitted.via_Referral
                    +Submitted.via_Postal_mail
                    +Submitted.via_Phone
                    +Submitted.via_Fax
                    ,data=consumercomplaintrainig,family = "binomial")

summary(churn_logistic)
#Feature Analysis
anova(churn_logistic, test="Chisq")
#find the accuracy of the logistic model

prediction_data<-predict(churn_logistic,consumercomplaintesting,type="response",se.fit=FALSE)
predicted_outcome<-ifelse(prediction_data>0.5,1,0)
churn<-data.frame(consumercomplaintesting,prediction_data,predicted_outcome)
tb<-table(predicted_outcome,consumercomplaintesting$Consumer.disputed_Y)
acc<-sum(diag(tb))/sum(tb)
acc
miss_class<-1-acc

#ROC CURVE & performance of the model

roc_pred<-predict(churn_logistic,consumercomplaintesting,type="response")
roc_obj<-prediction(roc_pred,consumercomplaintesting$Consumer.disputed_Y)
roc_performance<-performance(roc_obj,"tpr","fpr")
plot(roc_performance,col=2, lwd=3, main="ROC curve")
abline(a=0,b=1)
accuracy<-performance(roc_obj,"auc")
accuracy


#Decission Tree
tree <- ctree(Consumer.disputed_Y~Timely.response+Product, consumercomplaintrainig)
plot(tree, type='simple')


fit <-rpart(Consumer.disputed_Y~
            Product 
            +Issue 
            +Company.public.response 
            +Company 
            #+State 
            +Complaint.ID 
            +Timely.response 
            +Tags_Service 
            +Consumer.consent.provided 
            +Submitted.via_Email 
            +Submitted.via_Referral 
            +Submitted.via_Postal_mail 
            +Submitted.via_Phone 
            +Submitted.via_Fax,
            data=consumercomplaintrainig
            ,method="class"
          
            )
summary(fit)
# plot tree
rpart.plot(fit,type=4,extra=2,clip.right.labs=FALSE,varlen = 0,faclen = 0)

#find the accuracy of the Decission Tree  model
aabpredict<-predict(fit,consumercomplaintesting,type="class")
head(aabpredict)
tb<-table(consumercomplaintesting$Consumer.disputed_Y,aabpredict)
sum(diag(tb))/sum(tb)
#ROC CURVE & performance of the model
aoc<-predict(fit,consumercomplaintesting,type="prob")
aad<-aoc[,2]
head(aad)
aoe<-prediction(aad,consumercomplaintesting$Consumer.disputed_Y)
rocc<-performance(aoe,"tpr","fpr")
plot(rocc,col=3, lwd=4, main="ROC curve")
abline(a=0,b=1)
performance(aoe,"auc")
gc()

#### Random Forest


consumercomplain$Consumer.disputed_Y<-ifelse(consumercomplain$Consumer.disputed_Y==1,"Yes","No")
#####Split data

set.seed(200)
splitdata<-sample.split(consumercomplain,SplitRatio=.7)
#consumercomplain$Consumer.disputed_Y<-ifelse(consumercomplain$Consumer.disputed_Y.==1,"Yes","No")

consumercomplaintrainig<-subset(consumercomplain,splitdata==TRUE)
consumercomplaintesting<-subset(consumercomplain,splitdata==FALSE)

consumercomplaintrainig$Issue<-NULL
consumercomplaintrainig$Company<-NULL
consumercomplaintrainig$State<-NULL
#Initial random model
str(consumercomplain)
rfModel<-randomForest(as.factor(Consumer.disputed_Y)~
                        
                        Product
                      +Company.public.response
                      +Complaint.ID
                      +Timely.response
                      +Tags_Service
                      +Consumer.consent.provided
                      +Submitted.via_Email
                      +Submitted.via_Referral
                      +Submitted.via_Postal_mail
                      +Submitted.via_Phone
                      +Submitted.via_Fax
                      ,data=consumercomplaintrainig
                      ,ntree=500
                      ,mtry=4
                     , method="class")

print(rfModel)
plot(rfModel,col=3)
#tune the model
tuneRandomForest <- tuneRF(consumercomplaintrainig[, -5], as.factor(consumercomplaintrainig[, 5]), stepFactor = 0.2, plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)
#plot(tuneRandomForest)

# Fitted Random Forest
rfModel<-randomForest(as.factor(Consumer.disputed_Y)~
                        
                        Product
                      +Company.public.response
                      +Complaint.ID
                      +Timely.response
                      +Tags_Service
                      +Consumer.consent.provided
                      +Submitted.via_Email
                      +Submitted.via_Referral
                      +Submitted.via_Postal_mail
                      +Submitted.via_Phone
                      +Submitted.via_Fax
                      ,data=consumercomplaintrainig
                      ,ntree=250
                      ,mtry=3
                      ,importance = TRUE
                      , method="class")
#print(rfModel)

set.seed(200)

consumercomplain$Consumer.disputed_Y<-ifelse(consumercomplain$Consumer.disputed_Y=="Yes",1,0)
splitdata<-sample.split(consumercomplain,SplitRatio=.7)
consumercomplaintrainig<-subset(consumercomplain,splitdata==TRUE)
consumercomplaintesting<-subset(consumercomplain,splitdata==FALSE)



rfModel<-randomForest(Consumer.disputed_Y ~
                        
                        Product
                      +Company.public.response
                      +Complaint.ID
                      +Timely.response
                      +Tags_Service
                      +Consumer.consent.provided
                      +Submitted.via_Email
                      +Submitted.via_Referral
                      +Submitted.via_Postal_mail
                      +Submitted.via_Phone
                      +Submitted.via_Fax
                      ,data=consumercomplaintrainig
                      
                      , method="class")

aapr1<-predict(rfModel,consumercomplaintesting,type="class")
aapr2<-ifelse(aapr1>=.5,1,0)
tb1<-table(consumercomplaintesting$Consumer.disputed_Y,aapr2)
sum(diag(tb1))/sum(tb1)

aapr3<-prediction(aapr1,consumercomplaintesting$Consumer.disputed_Y)
aapr4<-performance(aapr3,"tpr","fpr")
plot(aapr4, lwd=2, main="ROC curve")
abline(a=0,b=1)
aapr5<-performance(aapr3,"auc")
aapr5
abline(a=0,b=1)
alpha<-round(as.numeric(unlist(aapr4@alpha.values)),4)
fpr<-round(as.numeric(unlist(aapr4@x.values)),4)
tpr<-round(as.numeric(unlist(aapr4@y.values)),4)
i<-which(round(alpha,2)==0.30)
paste("Threshold=",(alpha[i]),"TPR=", tpr[i], "FPR=", fpr[i])
