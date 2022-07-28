library(aplore3)
library(car)
library(glmnet)
library(epitools)
library(randomForest)

#Change working directory to this source file directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Load my custom functions
source("Utility.r")

glow_bonemed_raw = glow_bonemed

#Variables we chose from our EDA process
variablesToSelect = c("priorfrac", "age", "momfrac", "armassist", "smoke", 
                      "raterisk", "fracscore", "bonemed", "bonemed_fu", 
                      "bonetreat", "height", "fracture")

glow_bonemed_raw = glow_bonemed %>% select(variablesToSelect)

str(glow_bonemed_raw)

#Standardize Data
glow_bonemed_std = get_standardized_df(glow_bonemed_raw, c("age", "fracscore", "height"))
str(glow_bonemed_std)

#Split into training and test set
set.seed(2)
train_test_list = train_test_split(glow_bonemed_std , 0.8)
train = train_test_list[[1]]
test = train_test_list[[2]]

#Through our EDA we know the data is mostly clean we just need to handle the
#data imbalance

train %>% ggplot(aes(x=fracture)) + geom_bar()
train = get_df_with_upsampled_class(train, "fracture", "Yes", 3)
train %>% ggplot(aes(x=fracture)) + geom_bar()

#Lasso for feature selection
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


#No feature was dropped out so keep them all

#Start Model Building Process
model_compare_df = data.frame(model_num = c(0), auc=c(0), balanced_accuracy=c(0))
model_compare_df
model_num = 0

#Model 1: Simple
model_num = model_num + 1
model=glm(fracture~.,family="binomial",data=train)
summary(model)

#Roc and AUC
auc = plot_roc_and_get_auc(model, test, "fracture")

#Best Threshold using Balanced Acc
preds = unname(predict(model, test, type="response"))
best_threshold = get_and_plot_best_threshold(preds, test$fracture)
class_preds = ifelse(preds > best_threshold,"Yes","No")
CM_rep = confusionMatrix(table(class_preds, test$fracture))
CM_rep
balanced_acc = CM_rep$byClass[11]

#Update Model Comparison Dataframe
model_stats = c(model_num, auc, balanced_acc)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df

#Model 2: Remove values that were not significant
model_num = model_num + 1
model=glm(fracture~
             priorfrac+
             age+
             momfrac+
             armassist+
             raterisk+
             bonemed+
             bonetreat+
             height,
           family="binomial",data=train)
summary(model)

#Roc and AUC
auc = plot_roc_and_get_auc(model, test, "fracture")

#Best Threshold using Balanced Acc
preds = unname(predict(model, test, type="response"))
best_threshold = get_and_plot_best_threshold(preds, test$fracture)
class_preds = ifelse(preds > best_threshold,"Yes","No")
CM_rep = confusionMatrix(table(class_preds, test$fracture))
CM_rep
balanced_acc = CM_rep$byClass[11]

#Update Model Comparison Dataframe
model_stats = c(model_num, auc, balanced_acc)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df

#Model 3: Cts Interaction using stepwise approach
model_num = model_num + 1
model=glm(fracture~
             priorfrac+
             age + tan(age) + age:priorfrac + age:raterisk + age:bonemed + age:bonetreat +
             momfrac+
             armassist+
             raterisk+
             bonemed+
             bonetreat+
             height
          ,family="binomial",data=train)
summary(model)

#Roc and AUC
auc = plot_roc_and_get_auc(model, test, "fracture")

#Best Threshold using Balanced Accuracy
preds = unname(predict(model, test, type="response"))
best_threshold = get_and_plot_best_threshold(preds, test$fracture)
class_preds = ifelse(preds > best_threshold,"Yes","No")
CM_rep = confusionMatrix(table(class_preds, test$fracture))
CM_rep
balanced_acc = CM_rep$byClass[11]

#Update Model Comparison Dataframe
model_stats = c(model_num, auc, balanced_acc)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df

#Model 4: QDA
model_num = model_num + 1
model=glm(fracture~
            priorfrac+
            age + tan(age) + age:priorfrac + age:raterisk + age:bonemed + age:bonetreat +
            momfrac+
            armassist+
            raterisk+
            bonemed+
            bonetreat+
            height
          ,family="binomial",data=train)
summary(model)

#Roc and AUC
auc = plot_roc_and_get_auc(model, test, "fracture")

#Best Threshold using Balanced Accuracy
preds = unname(predict(model, test, type="response"))
best_threshold = get_and_plot_best_threshold(preds, test$fracture)
class_preds = ifelse(preds > best_threshold,"Yes","No")
CM_rep = confusionMatrix(table(class_preds, test$fracture))
CM_rep
balanced_acc = CM_rep$byClass[11]

#Update Model Comparison Dataframe
model_stats = c(model_num, auc, balanced_acc)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df

#Model 5: Nonparametric (Random Forest)
model_num = model_num + 1

features = c("priorfrac", "age", "momfrac", "armassist", "smoke", 
                      "raterisk", "fracscore", "bonemed", "bonemed_fu", 
                      "bonetreat", "height")
target = c("fracture")

#Find Optimal Depth
optimal_d = find_optimal_depth(train, test, features, target, d_max=60)

#Setup train and test data
train_fea = train %>% select(contains(features))
train_tar = train %>% select(contains(target))
test_fea = test %>% select(contains(features))
test_tar = test %>% select(contains(target))

#Build Final Model
model = randomForest(x=train_fea, y=train_tar$fracture,
                      ntree = 1000, maxnodes = optimal_d)

#Roc and AUC: Have to make custom func for this
auc = -1

#Best Threshold using Balanced Accuracy
pred_forest = predict(model, test_fea)
CM_rep = confusionMatrix(table(pred_forest, test$fracture))
CM_rep
balanced_acc = CM_rep$byClass[11]

#Update Model Comparison Dataframe
model_stats = c(model_num, auc, balanced_acc)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df

#Model 6: Nonparametric (Random Forest) All Variables In Dataset
#NOT DONE YET
#phy_id: Physician ID code (128 unique codes)
#priorfrac: History of Prior Fracture (no, Yes)
#age: age at enrollment (Years)
#height: height at enrollment (centimeter)
#weight: Weight at enrollment (kg)
#bmi: body mass index (kg/m^2)
#premeno: Menopause before age 45 (no, Yes)
#momfrac: Mother had hip fracture (no, yes)
#armassist: Arms are needed to stand frm chair (no, yes)
#smoke: form or current smoker (no, yes)
#raterisk: self-reported risk of frac (Less than others of same, same as others of the same age, greater than others of the same age)
#fracscore: Fracture Risk Score (Composite Risk Score)
#bonemed: bone medications at enroll (no, yes)
#bonemed_fu: bone meds at follow up (no, yes)
#bonetreat: bone med both at enroll and follow (no, yes)
#fracture: any fracture in first year (no, yes) TARGET
model_num = model_num + 1

features = c("phy_id", "priorfrac", "age", "momfrac", "armassist", "smoke", 
             "raterisk", "fracscore", "bonemed", "bonemed_fu", 
             "bonetreat", "height")
target = c("fracture")

#Find Optimal Depth
optimal_d = find_optimal_depth(train, test, features, target, d_max=60)

#Setup train and test data
train_fea = train %>% select(contains(features))
train_tar = train %>% select(contains(target))
test_fea = test %>% select(contains(features))
test_tar = test %>% select(contains(target))

#Build Final Model
model = randomForest(x=train_fea, y=train_tar$fracture,
                     ntree = 1000, maxnodes = optimal_d)

#Roc and AUC
auc = -1

#Best Threshold using Balanced Accuracy
pred_forest = predict(model, test_fea)
CM_rep = confusionMatrix(table(pred_forest, test$fracture))
CM_rep
balanced_acc = CM_rep$byClass[11]

#Update Model Comparison Dataframe
model_stats = c(model_num, auc, balanced_acc)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df


































