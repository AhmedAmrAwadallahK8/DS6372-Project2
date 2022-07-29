library(aplore3)
library(car)
library(glmnet)
library(epitools)
library(randomForest)
library(MASS)
library(mvtnorm)

#Redo Model 2 and Model 3


#Change working directory to this source file directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Load my custom functions
source("Utility.r")

glow_bonemed_raw = glow_bonemed

#Don't need to do much cleaning we know this data is overall fine
#Remove features that should not be used in the model
#Choose variables
variablesToRemove = c("sub_id", "site_id")
glow_bonemed_raw = glow_bonemed_raw %>% select(-variablesToRemove)

#Split into training and test set
set.seed(1234)
train_test_list = train_test_split(glow_bonemed_raw , 0.8)
raw_train = train_test_list[[1]]
raw_test = train_test_list[[2]]

#Through our EDA we know the data is mostly clean we just need to handle the
#data imbalance

raw_train %>% ggplot(aes(x=fracture)) + geom_bar()
raw_train = get_df_with_upsampled_class(raw_train, "fracture", "Yes", 3)
raw_train %>% ggplot(aes(x=fracture)) + geom_bar()

#Lasso for feature selection

#Lasso Prefers Standardized Data
cts_vars = c("phy_id", "age", "weight", "height", "bmi", "fracscore")
train = get_standardized_df(raw_train, cts_vars)
test = get_standardized_df(raw_test, cts_vars)

#Lasso Protocol
TrainFeatures = train
TrainFeatures = model.matrix(fracture~.,TrainFeatures)[,-1]
TrainTarget = train$fracture

TestFeatures = test
TestFeatures = model.matrix(fracture~.,TestFeatures)[,-1]
TestTarget = test$fracture

grid=10^seq(10,-2, length =100)
lasso.mod=glmnet(TrainFeatures,TrainTarget, family = "binomial", alpha=1, lambda =grid)
cv.out=cv.glmnet(TrainFeatures,TrainTarget, family = "binomial",alpha=1) #alpha=1 performs LASSO
plot(cv.out)

bestlambda<-cv.out$lambda.min
lasso.pred=predict (lasso.mod ,s=bestlambda ,newx=TestFeatures)

results = prediction(lasso.pred, TestTarget)

coef(lasso.mod,s=bestlambda)

roc.lasso = performance(results, measure = "tpr", x.measure = "fpr")
plot(roc.lasso,colorize = TRUE)
abline(a=0, b= 1)
auc_holder = performance(results, measure = "auc")
auc = auc_holder@y.values[[1]]
auc


#Weight and Fracscore was dropped by Lasso

#Start Model Building Process
model_compare_df = data.frame(model_num = c(0), model_description = c("Null Model"), auc=c(0), accuracy=c(0), balanced_accuracy=c(0)
                              , sensitivity=c(0), specificity=c(0), f1_score=c(0))
model_compare_df
model_num = 0

pos_class = "Yes"

#Model 1: Simple Log From Obj 1
model_description = "Simple Log"

#Var Selection based on our EDA and Lasso
variablesToSelect = c("priorfrac", "age", "height", "momfrac", 
                      "armassist", "raterisk", "bonemed", "bonemed_fu", 
                      "bonetreat", "fracture")
train = raw_train %>% select(variablesToSelect)
test = raw_test %>% select(variablesToSelect)

#Standardize Data
cts_vars = c("age", "height")
train = get_standardized_df(train, cts_vars)
test = get_standardized_df(test, cts_vars)

#Model
model_num = model_num + 1
model=glm(fracture~.,family="binomial",data=train)
summary(model)

#Roc and AUC
auc = plot_roc_and_get_auc(model, test, "fracture")

#Best Threshold using Balanced Acc
preds = unname(predict(model, test, type="response"))
best_threshold = get_and_plot_best_threshold(preds, test$fracture, pos_class)
best_threshold
class_preds = ifelse(preds > best_threshold,"Yes","No")
CM_rep = confusionMatrix(table(class_preds, test$fracture), positive = pos_class)
CM_rep
acc = CM_rep$overall[1]
balanced_acc = CM_rep$byClass[11]
sens = CM_rep$byClass[1]
spec = CM_rep$byClass[2]
precision = CM_rep$byClass[5]
recall = CM_rep$byClass[6]
f1 = get_f1(precision, recall)

#Update Model Comparison Dataframe
model_stats = c(model_num, model_description, auc, acc, balanced_acc, sens, spec, f1)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df



#Model 2: Cts Interaction using stepwise approach
model_description = "Complex Log + Interactions"
#Var Selection
variablesToSelect = c("phy_id", "priorfrac", "age", "height", "bmi", "premeno", "momfrac", 
                      "armassist", "raterisk", "bonemed", "bonemed_fu", 
                      "bonetreat", "fracture")
train = raw_train %>% select(variablesToSelect)
test = raw_test %>% select(variablesToSelect)

#Standardize Data
cts_vars = c("age", "height")
train = get_standardized_df(train, cts_vars)
test = get_standardized_df(test, cts_vars)

#Model
model_num = model_num + 1
model=glm(fracture~
             phy_id +
             priorfrac+
             age  +
             height + height:bonemed_fu  +
             bmi +
             premeno +
             momfrac+
             armassist+
             raterisk+
             bonemed+
             bonemed_fu +
             bonetreat
          ,family="binomial",data=train)
summary(model)

#Roc and AUC
auc = plot_roc_and_get_auc(model, test, "fracture")

#Best Threshold using Balanced Accuracy
preds = unname(predict(model, test, type="response"))
best_threshold = get_and_plot_best_threshold(preds, test$fracture, pos_class)
class_preds = ifelse(preds > best_threshold,"Yes","No")
CM_rep = confusionMatrix(table(class_preds, test$fracture), positive = pos_class)
CM_rep
acc = CM_rep$overall[1]
balanced_acc = CM_rep$byClass[11]
sens = CM_rep$byClass[1]
spec = CM_rep$byClass[2]
precision = CM_rep$byClass[5]
recall = CM_rep$byClass[6]
f1 = get_f1(precision, recall)

#Update Model Comparison Dataframe
model_stats = c(model_num, model_description, auc, acc, balanced_acc, sens, spec, f1)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df

#Model 3: Non Standardized Data (If We have time)
model_description = "Complex Log + Interactions + Transformed Features"
#Var Selection
variablesToSelect = c("priorfrac", "age", "height", "momfrac", 
                      "armassist", "raterisk", "bonemed", "bonemed_fu", 
                      "bonetreat", "fracture")
train = raw_train %>% select(variablesToSelect)
test = raw_test %>% select(variablesToSelect)

#Add New Cts Features
train$harmonic_age = 1/train$age
train$log_age =log(train$age)

train$harmonic_height = 1/train$height
train$log_height =log(train$height)

test$harmonic_age = 1/test$age
test$log_age =log(test$age)

test$harmonic_height = 1/test$height
test$log_height =log(test$height)

#Standardize Data
cts_vars = c("age", "harmonic_age", "log_age", "height", "harmonic_height", "log_height")
train = get_standardized_df(train, cts_vars)
test = get_standardized_df(test, cts_vars)

#Model
model_num = model_num + 1
model=glm(fracture~
            priorfrac+
            age  + harmonic_age +
            height + height:bonemed_fu +
            momfrac+
            armassist+
            raterisk+
            bonemed+
            bonemed_fu +
            bonetreat
          ,family="binomial",data=train)
summary(model)

#Roc and AUC
auc = plot_roc_and_get_auc(model, test, "fracture")

#Best Threshold using Balanced Accuracy
preds = unname(predict(model, test, type="response"))
best_threshold = get_and_plot_best_threshold(preds, test$fracture, pos_class)
class_preds = ifelse(preds > best_threshold,"Yes","No")
CM_rep = confusionMatrix(table(class_preds, test$fracture), positive = pos_class)
CM_rep
acc = CM_rep$overall[1]
balanced_acc = CM_rep$byClass[11]
sens = CM_rep$byClass[1]
spec = CM_rep$byClass[2]
precision = CM_rep$byClass[5]
recall = CM_rep$byClass[6]
f1 = get_f1(precision, recall)

#Update Model Comparison Dataframe
model_stats = c(model_num, model_description, auc, acc, balanced_acc, sens, spec, f1)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df

#Model 4: QDA
model_description = "QDA"
model_num = model_num + 1

#Var Selection
variablesToSelect = c("phy_id", "age", "height", "bmi", "fracscore", "fracture")
train = raw_train %>% select(variablesToSelect)
test = raw_test %>% select(variablesToSelect)

#Standardize Data
cts_vars = c("phy_id", "age", "height", "bmi", "fracscore")
train = get_standardized_df(train, cts_vars)
test = get_standardized_df(test, cts_vars)

model=qda(fracture~
            age+
            height
          ,family="binomial",data=train)
  

#Roc and AUC:
class_preds=predict(model,newdata=test)$class
numeric_preds = ifelse(class_preds == "Yes",1,0)
auc = plot_roc_and_get_auc_generalized(numeric_preds, test, "fracture")

#Best Threshold using Balanced Accuracy
class_preds=predict(model,newdata=test)$class
CM_rep = confusionMatrix(table(class_preds, test$fracture), positive = pos_class)
CM_rep
acc = CM_rep$overall[1]
balanced_acc = CM_rep$byClass[11]
sens = CM_rep$byClass[1]
spec = CM_rep$byClass[2]
precision = CM_rep$byClass[5]
recall = CM_rep$byClass[6]
f1 = get_f1(precision, recall)

#Update Model Comparison Dataframe
model_stats = c(model_num, model_description, auc, acc, balanced_acc, sens, spec, f1)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df

#Model 5: Nonparametric (Random Forest)
model_description = "Random Forest"
model_num = model_num + 1

#Var Selection
train = raw_train
test = raw_test

#Standardize Data
cts_vars = c("phy_id", "age", "weight", "height", "bmi", "fracscore")
train = get_standardized_df(train, cts_vars)
test = get_standardized_df(test, cts_vars)

#Specify Features and Target
features = c("phy_id", "priorfrac", "age", "weight", "bmi", "permeno", 
             "momfrac", "armassist", "smoke", "raterisk", "fracscore", 
             "bonemed", "bonemed_fu", "bonetreat", "height")
target = c("fracture")

#Find Optimal Depth
optimal_d = find_optimal_depth(train, test, features, target, d_max=30)

#Setup train and test data
train_fea = train %>% select(contains(features))
train_tar = train %>% select(contains(target))
test_fea = test %>% select(contains(features))
test_tar = test %>% select(contains(target))

#Build Final Model
model = randomForest(x=train_fea, y=train_tar$fracture,
                      ntree = 1000, maxnodes = optimal_d)

#Roc and AUC: Have to make custom func for this
class_preds = unname(predict(model, test_fea))
numeric_preds = ifelse(class_preds == "Yes",1,0)
auc = plot_roc_and_get_auc_generalized(numeric_preds, test, "fracture")

#Best Threshold using Balanced Accuracy
pred_forest = predict(model, test_fea)
CM_rep = confusionMatrix(table(pred_forest, test$fracture), positive = pos_class)
CM_rep
acc = CM_rep$overall[1]
balanced_acc = CM_rep$byClass[11]
sens = CM_rep$byClass[1]
spec = CM_rep$byClass[2]
precision = CM_rep$byClass[5]
recall = CM_rep$byClass[6]
f1 = get_f1(precision, recall)

#Update Model Comparison Dataframe
model_stats = c(model_num, model_description, auc, acc, balanced_acc, sens, spec, f1)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df

#Remove Useless row
final_model_compare_df = model_compare_df[-1,-1]
final_model_compare_df


































