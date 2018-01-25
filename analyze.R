# IBM Employee Attrition analysis
# by Zaixing Mao

# Prediction of Attrition by using XGBoost

library("xgboost")
#library("caret")
library("pROC")
#library("corrplot")
#library("fmsb")


# Importing data
d<-read.csv("data/WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Data cleaning
# change categorical to numerical
d$Attrition= as.integer(as.factor(d$Attrition))-1
d$BusinessTravel= as.integer(as.factor(d$BusinessTravel))
d$Department= as.integer(as.factor(d$Department))
d$Gender= as.integer(as.factor(d$Gender))
d$JobRole= as.integer(as.factor(d$JobRole))
d$MaritalStatus= as.integer(as.factor(d$MaritalStatus))
d$OverTime= as.integer(as.factor(d$OverTime))
d$EducationField= as.integer(as.factor(d$EducationField))
d$StandardHours<-NULL
d$Over18<-NULL
d$EmployeeCount<-NULL
d$JobLevel<-NULL


# vif_func(in_frame=d,thresh=5,trace=T)
# str(d)
#M <- cor(d)
#corrplot(M, method="circle")

# Classification
# Creat data for training and test
set.seed(0)
tr.number<-sample(nrow(d),nrow(d)*2/3) 
train<-d[tr.number,]
test<-d[-tr.number,]

# split dataset
train_Y = train$Attrition
train$Attrition<-NULL
test_Y = test$Attrition
test$Attrition<-NULL


nround <- 1000
param <- list(max_depth=3, 
              silent=1, 
              eta = 0.3,
              objective='binary:logistic',
              eval_metric = 'auc')

cat('running cross validation\n')

# do cross validation, this will print result out as
# [iteration]  metric_name:mean_value+std_value
# std_value is standard deviation of the metric
dtrain <- xgb.DMatrix(as.matrix(train), label = train_Y)
cv <- xgb.cv(param, dtrain, nround, nfold=5, early_stopping_rounds=100)
bst <- xgb.train(param, nrounds =cv$best_ntreelimit, dtrain)

pred <- predict(bst, as.matrix(test))

plot(roc(test_Y,pred))

#pred <- ifelse(pred > 0.5,1,0)

#CM = table(test_Y, pred)
#print(CM)

#accu <- mean(as.numeric(pred > 0.5) == test_Y)
#print(paste("test-accuracy=", accu))

#importance <- xgb.importance(feature_names = names(test), model = bst)
#print(xgb.plot.importance(importance_matrix = importance, top_n = 10))