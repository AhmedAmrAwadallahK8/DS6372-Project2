#Random Code
#Previous model 3
variablesToSelect = c("priorfrac", "age", "momfrac", "armassist", "smoke", 
                      "raterisk", "fracscore", "bonemed", "bonemed_fu", 
                      "bonetreat", "height", "fracture")

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

#Model 2: Remove values that were not significant
model_description = "Simple Log + Significant Features"

#Var Selection
variablesToSelect = c("priorfrac", "age", "momfrac", "armassist", "smoke", 
                      "raterisk", "fracscore", "bonemed", "bonemed_fu", 
                      "bonetreat", "height", "fracture")
train = raw_train %>% select(variablesToSelect)
test = raw_test %>% select(variablesToSelect)

#Standardize Data
cts_vars = c("age", "height")
train = get_standardized_df(train, cts_vars)
test = get_standardized_df(test, cts_vars)

#Model
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
sens = CM_rep$byClass[1]
spec = CM_rep$byClass[2]
precision = CM_rep$byClass[5]
recall = CM_rep$byClass[6]
f1 = get_f1(precision, recall)

#Update Model Comparison Dataframe
model_stats = c(model_num, model_description, auc, balanced_acc, sens, spec, f1)
model_compare_df = rbind(model_compare_df, model_stats)
model_compare_df

anova(model, test="Chisq")

#Simple Model
model_num = model_num + 1
simple_log<-glm(fracture~.,family="binomial",data=train)
summary(simple_log)

par(mfrow=c(2,2))
plot(simple_log)
par(mfrow=c(1,1))
vif(simple_log) #How to interpret this?

anova(simple_log, test="Chisq")

train_preds = predict(model, train, type="response")
train_pred_class_num = unname(ifelse(train_preds > 0.5,1,0))
train_pred_class = as.factor(unname(ifelse(train_preds > 0.5,"Yes","No")))
train_targ = ifelse(train$fracture == "Yes",1,0)
plot(train_preds, train_targ, col = c("red", "blue")[train_pred_class])

plot_df = data.frame(train_preds, train_targ, train_pred_class)
plot_df %>% ggplot(aes(x=train_preds, y=train_targ, col=train_pred_class)) + geom_smooth() + geom_point()

data("ROCR.simple")
str(ROCR.simple)
simple_log_preds = predict(simple_log, test, type="response")
threshold = 0.5
simple_log_preds = ifelse(simple_log_preds > threshold,"Yes","No")
simple_log_preds
test$fracture
simple_log_preds == test$fracture
misClasificError = mean(simple_log_preds != test$fracture)
print(paste('Accuracy',1-misClasificError))


p <- unname(predict(simple_log, test, type="response"))
p
pr <- prediction(p, test$fracture)
pr
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize=TRUE)
abline(a=0, b= 1)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

plot(simple_log_preds)
?predict
str(simple_log_preds)
table(,simple_log_preds)
roc = performance(simple_log_preds, measure = "tpr", x.measure = "fpr")
#Lasso doesn't punish any feature so keep them all

#Probably Useless Code
#eval_pred<-table(TestTarget,lasso.pred)
#eval_pred Only works with class setting

#testMSE_LASSO<-mean((TestTarget-lasso.pred)^2)
#testMSE_LASSO
#Probably Useless Code

#fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
#fitted.results <- ifelse(fitted.results > 0.5,1,0)
#misClasificError <- mean(fitted.results != test$Survived)
#print(paste('Accuracy',1-misClasificError))

#p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
#pr <- prediction(p, test$Survived)
#prf <- performance(pr, measure = "tpr", x.measure = "fpr")
#plot(prf)
#auc <- performance(pr, measure = "auc")
#auc <- auc@y.values[[1]]
#auc