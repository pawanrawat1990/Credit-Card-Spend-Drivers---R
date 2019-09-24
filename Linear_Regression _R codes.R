
library(readxl)
library(dplyr)
library(caret)
Mydata <- read_excel("D:/R Analytics/Analytixlabs/Linear/Linear Regression Case.xlsx", sheet = 1)
View(Mydata)
str(Mydata)

colnames(Mydata)



####Creating Target Variable and combining common variable of primary and secndary cards #####################

Mydata$total_spend <- Mydata$cardspent+Mydata$card2spent


####### Droping variables which are already been used or not required for now#####
train_data <- dplyr::select(Mydata,-c("cardspent","card2spent","custid","birthmonth","lninc",	"lncreddebt",	"lnothdebt",	"lnlongmon",	"lnlongten",	"lntollmon",	"lntollten",	"lnequipmon",	"lnequipten",	"lncardmon",	"lncardten",	"lnwiremon",	"lnwireten"))

# set.seed(1234)
# 
# custom <- trainControl(method="cv",
#                        number = 10)
# 
# grid <- expand.grid(maxdepth=12, n.tree=1000)
# 
# 
# fit1 <- train(total_spend~.,method="rf",tunegird=grid,trControl=custom,metric="RMSE",maximise=F,na.action = na.omit,importance=TRUE,data=train_data)
# varImp(fit1)
# fit1



mystats <- function(x){
  nmiss <- sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p90 <- quantile(a,0.90)
  p95 <- quantile(a,0.95)
  p99 <- quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag <- max>UC|min<LC
  return(c(n=n ,nmiss = nmiss,mean=m,stdev=s,min = min,UC =UC, LC=LC,p90=p90,
            p95=p95,p99=p99,outlier_flag= outlier_flag, max = max))
}


##### Selecting important variables based on business understanding and RF #################

vars <- c("total_spend","townsize",	"gender",	"age",	"employ",	"income",	"inccat",	"creddebt",	"othdebt",	"marital",	"address",	"addresscat",	"card2",	"carvalue",	"carcatvalue",	
          "card",	"cardtenure",	"card2tenure",	"carditems",	"card2items",	"tenure",	"longmon",	"longten",	"equipten",	"cardten")


train_data <- dplyr::select(train_data,vars)

diag_stats <- t(data.frame(apply(train_data[vars],2,mystats)))
View(diag_stats)

write.csv(diag_stats, file = "D:/R Analytics/Analytixlabs/Linear/diag_stats.csv")

########### outliers Capping with 99 Percentile####################

for(i in vars){
  train_data[[i]][train_data[[i]]>quantile(train_data[[i]],0.99,na.rm=T)] <- quantile(train_data[[i]],0.99,na.rm = T)
}

##### Outlier capping less than 1 Percentile


for(i in vars){
  train_data[[i]][train_data[[i]]<quantile(train_data[[i]],0.01,na.rm=T)] <- quantile(train_data[[i]],0.01,na.rm = T)
}



################ Missing Value Imputation-mean ########################################
train_data$townsize[is.na(train_data$townsize)] <- 3
train_data$longten[is.na(train_data$longten)] <- mean(train_data$longten,na.rm=TRUE)
train_data$cardten[is.na(train_data$cardten)] <- mean(train_data$cardten,na.rm=TRUE)



### Changing ordinal variables to factors to use it as dummy variable, order of levels are not consistent

## In some of the variable 5 is highest and in some 5 is low.

Vars2 <- c("townsize","inccat","card","card2","addresscat")

for(i in Vars2){
  train_data[[i]] <- as.factor(train_data[[i]])
}

library(fastDummies)

train_data2 <- dummy_cols(train_data,remove_first_dummy = TRUE)

train_data2 <- dplyr::select(train_data2,-c("townsize","inccat","card","card2","addresscat"))

numeric_var <- sapply(train_data2, is.numeric)


corrm <- cor(train_data2[numeric_var]) ### correlation matrix


# write.csv(corrm, file = "D:/R Analytics/Analytixlabs/Linear/corrm.csv")


###### Variable Transformation for better predictions ##########################################

hist(train_data2$total_spend)
train_data2$ln_total_spend <- log(train_data2$total_spend)
hist(train_data2$income)
train_data2$ln_income <- log(train_data2$income)


#Splitting data into Training and Testing Dataset

set.seed(123)
train_ind <- sample(1:nrow(train_data2), size = floor(0.70 * nrow(train_data2)))

training<-train_data2[train_ind,]
testing<-train_data2[-train_ind,]


fit2 <- lm(ln_total_spend ~gender+age+ln_income+	marital+	carditems+	card2items+	equipten+	
             cardten+	townsize_2+	townsize_3+	townsize_4+	townsize_5+	inccat_2+	inccat_3+	inccat_4+	card2_2+	
             card2_3+	card2_4+	card2_5+	card_2+	card_3+	card_4+	card_5,data= training)

summary(fit2)

library(car)
library(MASS)

step3<- stepAIC(fit2,direction="both")
ls(step3)
step3$anova

###  Model after Stepwise regression ###############################

fit3 <- lm(ln_total_spend ~ gender + age + ln_income + carditems + card2items + 
              + card2_2 + card2_3 + card2_4+card2_5 + card_2 + card_3 + card_4 + card_5,data= training)


summary(fit3)

######## Cross Validation Approach - Just to check the stablity of Model ##########################

#Randomly shuffle the data
training_validation<-training[sample(nrow(training)),]

#Create 5 equally size folds
folds <- cut(seq(1,nrow(training_validation)),breaks=5,labels=FALSE)

#Perform 5 fold cross validation
modelNumber <- c()
modelRMSE <- c()
modelRsquare <- c()
for(i in 1:5){
  #Segement  data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testdata <- training_validation[testIndexes, ]
  traindata <- training_validation[-testIndexes, ]
  model <- lm(ln_total_spend ~ gender + age + ln_income + carditems + card2items + 
                + card2_2 + card2_3 + card2_4 + 
                card2_5 + card_2 + card_3 + card_4 + card_5,data= traindata)
  summary(model)

  predictions <- exp(predict(model,testdata))
  error <- predictions- testdata$total_spend
  RMSE <- c(sqrt(mean(error^2)))
  modelNumber <- c(modelNumber, i)
  modelRMSE <- c(modelRMSE, RMSE)
  modelRsquare <- c(modelRsquare,summary(model)$r.squared)
  #Use the test and train data partitions however you desire...
}

results <- data.frame(modelNumber = modelNumber, modelRMSE = modelRMSE, modelRsquare= modelRsquare)

### Model is stable #########################
### Higher accuracy can be achieved by using other Modelling Approach like ridge/Lasso Regression or Boosting Algo ######

####################### Prediction on testing data ##################################

t1 <- cbind(testing, pred_spend = exp(predict(fit3,testing)))

error <- t1$pred_spend- t1$total_spend
RMSE <- c(sqrt(mean(error^2)))
RMSE

####################### END ##########################################