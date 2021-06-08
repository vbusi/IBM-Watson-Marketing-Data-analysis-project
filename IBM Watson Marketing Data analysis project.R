install.packages('stringr')
library(stringr)

Watson <-read.csv('C:/Users/vinee/Google Drive/DAE/Spring 2020/SYST568/Project/IBM_watson.csv')
dim(Watson)
head(Watson)
colSums(is.na(Watson))
str(Watson)

Watson <- Watson[,-c(1)]

colnames(Watson)
colnames(Watson) <- str_replace_all(colnames(Watson),"[.]","")
colnames(Watson)
head(Watson)
#Replacing Null values with Mean in each of its column
for(i in 1:ncol(Watson)) {
  Watson[ , i][is.na(Watson[ , i])] <- mean(Watson[ , i], na.rm = TRUE)
}

colSums(is.na(Watson))

#Finding the outliers in the dataset.

outvals=boxplot(Watson$MonthsSincePolicyInception)$out

outvals

which(Watson$MonthsSincePolicyInception %in% outvals)

Watson<- Watson[-which(Watson$MonthsSincePolicyInception %in% outvals),]

dim(Watson)

# Found some -1 values instead of 1 might got mistyped so replacing them with 1

unique(Watson$NumberofOpenComplaints)

Watson$NumberofOpenComplaints[Watson$NumberofOpenComplaints=="-1"] <- 1

#Watson$Response[Watson$Response=="No"] <- 0

head(Watson)

levels(Watson$Response) <- 0:1

table(Watson$Response)

#Data analysis
names(Watson)

#States with response
install.packages("ggplot2")
library(ggplot2)
ggplot(Watson, aes(x = State, fill = Response))+
  geom_bar() +
  facet_wrap(~SalesChannel,nrow = 1) + 
  ggtitle("View of State, SalesChannel  and Response") +
  xlab("State") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Response") 


ggplot(Watson,aes(x = State,fill=Response)) +
  geom_bar() +
  ggtitle("State V/S Response rate")+
  xlab("State") +
  ylab("Total Count") +
  labs(fill = "Response") 


ggplot(Watson,aes(x = SalesChannel,fill=Response)) +
  geom_bar() +
  ggtitle("SalesChannel V/S Response rate")+
  xlab("SalesChannel") +
  ylab("Total Count") +
  labs(fill = "Response")

#  Employment status and renewOffer
ggplot(Watson, aes(x = EmploymentStatus, fill = Response))+
  geom_bar() +
  facet_wrap(~RenewOfferType,nrow = 1) + 
  ggtitle("View of Employment, renewOfferType  and Response") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Response")  +
  theme(axis.text.x = element_text(angle = 45))


# Create Training Data
n <- nrow(Watson)
f <- ncol(Watson)
# Number of rows for the training set (80% of the dataset)
n_train <- round(0.80 * n)
# Create a vector of indices which is an 80% random sample
set.seed(123)
train_indices <- sample(1:n, n_train)
# Subset the Diabetes data frame to training indices only
train <- Watson[train_indices, ]
# Exclude the training indices to create the test set
test <- Watson[-train_indices, ]


#continuous
names(train)
train<-train[,-c(6)]
mylogit<-glm(Response~.,data=train,family = "binomial")
summary(mylogit)

library(caret)
library(MASS)

stepAIC(mylogit,direction="backward")

names(Watson)

mylogit2<-glm(Response ~ Education + EmploymentStatus + Income + 
                LocationCode + MaritalStatus + MonthlyPremiumAuto + NumberofOpenComplaints + 
                RenewOfferType + SalesChannel + TotalClaimAmount + VehicleSize, family = "binomial", data = train)
summary(mylogit2 )
pred <- predict(mylogit2,test,type = "response")
x<-table(round(pred), test$Response)
sum(diag(x))/sum(x)
#after backwar regression 0.88122

#cv
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <-train(Response ~ Education + EmploymentStatus + Income + 
                  LocationCode + MaritalStatus + MonthlyPremiumAuto + NumberofOpenComplaints + 
                  RenewOfferType + SalesChannel + TotalClaimAmount + VehicleSize ,data = train, method="glm", family="binomial",
                trControl = ctrl, tuneLength = 5)

pred <- predict(mod_fit,test,type = "raw")
x<-table((pred), test$Response)
sum(diag(x))/sum(x)


#random forest(
library(randomForest)
rf<-randomForest(Response ~ Education + EmploymentStatus + Income + 
                   LocationCode + MaritalStatus + MonthlyPremiumAuto + NumberofOpenComplaints + 
                   RenewOfferType + SalesChannel + TotalClaimAmount + VehicleSize,data=train)
summary(rf)
pred <- predict(rf,test,type = "response")
x<-table((pred), test$Response)
sum(diag(x))/sum(x)
#accuracy 99.4

#svm
names(train)
df<- train[,c(3,5,6,8,9,10,11,14,18,19,20,22)]
df[] <- lapply(df,as.integer)

#Naive bais
nB_model <- naiveBayes(Response ~ Education + EmploymentStatus + Income + 
                         LocationCode + MaritalStatus + MonthlyPremiumAuto + NumberofOpenComplaints + 
                         RenewOfferType + SalesChannel + TotalClaimAmount + VehicleSize, 
                       data = train)
pred <- predict(nB_model, test, type="class") 
N<-table(pred, test$Response)
sum(diag(N))/sum(N)
#Accuracy:0.8653

#gbm
library(gbm)
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
gbmFit1 <- train(Response ~ Education + EmploymentStatus + Income + 
                   LocationCode + MaritalStatus + MonthlyPremiumAuto + NumberofOpenComplaints + 
                   RenewOfferType + SalesChannel + TotalClaimAmount + VehicleSize,
                 data = train, method = "gbm", trControl = fitControl,verbose = FALSE)
gbm_ITV2 <- predict(gbmFit1, test ,type= "raw")
N<-table(gbm_ITV2, test$Response)
sum(diag(N))/sum(N)
#accracy:0.8702