library(dplyr)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(caret)
library(ROCR)

#Gives back a plot of the percentage of rows NA per feature
percent_na_plot = function(df){
  row_count = dim(df)[1]
  row_count
  df %>% 
    summarise(across(everything(), ~ sum(is.na(.x)))/row_count*100) %>%
    gather(Column, NA_Count) %>%
    ggplot(aes(x=NA_Count, y=Column, fill = Column)) + geom_col() + ylab("Feature") + xlab("Na Value Percent")
}

#Gives basic cts-cts compare plots
cts_cts_compare = function(df, fea1, fea2){
  smooth_regression_plot = df%>%ggplot(aes_string(x=fea1, y=fea2)) + geom_smooth()
  scatter_plot = df%>%ggplot(aes_string(x=fea1, y=fea2)) + geom_point(position="dodge")
  plot_count = 2
  grid.arrange(smooth_regression_plot, scatter_plot, ncol=plot_count)
}

#Gives basic cts-categ compare plots
cts_categ_compare = function(df, cts_fea, categ_fea){
  boxplot = df %>% ggplot(aes_string(x=cts_fea, y=categ_fea)) + geom_boxplot()
  density_plot = df %>% ggplot(aes_string(x=cts_fea, fill=categ_fea)) + geom_density(alpha=0.8)
  plot_count = 2
  grid.arrange(boxplot, density_plot, ncol=plot_count)
}

#Gives basic categ-categ compare plots
categ_categ_compare = function(df, fea1, fea2){
  df %>% ggplot(aes_string(x=fea1, fill=fea2)) + geom_bar(position="fill") + ylab("Proportion")
}

get_train_test_list = function(df, splitPercent){
  dfRowIndices = 1:dim(df)[1]
  dfRowSize = dim(df)[1]
  sampleSize = round(splitPercent * dfRowSize)
  trainIndices = sample(dfRowIndices, sampleSize)
  testIndices = -trainIndices
  train = df[trainIndices,]
  test = df[testIndices,]
  return(list(train, test))
}

get_standardized_df = function(df, variablesToStandardize){
  columns = colnames(df)
  for(col in columns){
    if(col %in% variablesToStandardize){
      df[,col] = get_standardized_feature(df[,col])
    }
  }
  return(df)
}

get_normalized_df = function(df, variablesToNormalize){
  columns = colnames(df)
  for(col in columns){
    if(col %in% variablesToNormalize){
      df[,col] = get_normalized_feature(df[,col])
    }
  }
  return(df)
}

get_standardized_feature = function(feature){
  standardized_feature = (feature - mean(feature))/sd(feature)
  return(standardized_feature)
}

get_normalized_feature = function(feature){
  normalized_feature = (feature - min(feature))/(max(feature) - min(feature))
  return(normalized_feature)
}

get_df_with_upsampled_class = function(df, class_feature, class_value, up_factor){
  class_value_indices = (df[,class_feature] == class_value)
  specified_class_df = df[class_value_indices,]
  upsampled_class_df = get_upsampled_df(specified_class_df, up_factor)
  out_df = rbind(df, upsampled_class_df)
  return(out_df)
}

get_upsampled_df = function(df, up_factor){
  row_count = dim(df)[1]
  up_row_count = row_count*up_factor - row_count
  new_df = df[0,]
  for(i in 1:up_row_count){
    rand_row_ind = sample(1:row_count, 1)
    new_row = df[rand_row_ind,]
    new_df = rbind(new_df, new_row)
  }
  return(new_df)
}

plot_roc_and_get_auc = function(model, test_data, target_feature){
  preds = predict(model, test_data, type="response")
  preds_and_target = prediction(preds, test_data[,target_feature])
  roc = performance(preds_and_target, measure = "tpr", x.measure = "fpr")
  auc_perf = performance(preds_and_target, measure = "auc")
  auc = auc_perf@y.values[[1]]
  plot(roc, colorize=TRUE)
  abline(a=0, b= 1)
  return(auc)
}

plot_roc_and_get_auc_generalized = function(preds, test_data, target_feature){
  preds_and_target = prediction(preds, test_data[,target_feature])
  roc = performance(preds_and_target, measure = "tpr", x.measure = "fpr")
  auc_perf = performance(preds_and_target, measure = "auc")
  auc = auc_perf@y.values[[1]]
  plot(roc, colorize=TRUE)
  abline(a=0, b= 1)
  return(auc)
}

get_f1 = function(precision, recall){
  return(2*((precision*recall)/(precision + recall)))
}


get_and_plot_best_threshold = function(preds, target, pos_class, metric = "f1"){
  balanced_acc_holder = c()
  f1_holder = c()
  threshold_holder = c()
  best_balanced_acc = 0
  best_threshold = 0
  best_f1 = 0
  for(i in 1:100){
    threshold = i/100
    tryCatch(
      expr = {
        class_preds = ifelse(preds > threshold,"Yes","No")
        CM_report = confusionMatrix(table(class_preds, target), positive = pos_class)
        threshold_holder = c(threshold_holder, threshold)
        if(metric == "balanced_acc"){
          balanced_acc = unname(CM_report$byClass[11])
          balanced_acc_holder = c(balanced_acc_holder, balanced_acc)
          if(balanced_acc > best_balanced_acc){
            best_balanced_acc = balanced_acc
            best_threshold = threshold
          }
        }
        if(metric == "f1"){
          precision = CM_report$byClass[5]
          recall = CM_report$byClass[6]
          f1 = get_f1(precision, recall)
          f1_holder = c(f1_holder, f1)
          if(f1 > best_f1){
            best_f1 = f1
            best_threshold = threshold
          }
          
        }
        
      },
      error = function(e){ 
        # (Optional)
        # Do this if an error is caught...
      },
      warning = function(w){
        # (Optional)
        # Do this if an warning is caught...
      },
      finally = {
        # (Optional)
        # Do this at the end before quitting the tryCatch structure...
      }
    )
  }
  if(metric == "balanced_acc"){
    plot(threshold_holder, balanced_acc_holder, xlab="Threshold", ylab="Balanced Accuracy", type="l")
    abline(v=best_threshold, col="red")
  }
  if(metric == "f1"){
    plot(threshold_holder, f1_holder, xlab="Threshold", ylab="F1 Score", type="l")
    abline(v=best_threshold, col="red")
  }
  return(best_threshold)
}

#Find optimal depth that maximizes balanced accuracy
find_optimal_depth = function(train, test, fea, tar, pos_class, d_min=2, d_max=20,  
                              s_num=10, avg_loop=5, title = "Depth versus Accuracy for Forest Model"){
  #Setup Data Structures for loop logic and output
  d_loop = d_max-d_min+1
  acc_overall = numeric(d_loop)
  d_vals = c(d_min:d_max)
  
  train_fea = train %>% select(contains(fea))
  train_tar = train %>% select(contains(tar))
  test_fea = test %>% select(contains(fea))
  test_tar = test %>% select(contains(tar))
  #Loop for every d in range specified by parameters dmax-dmin
  for(j in 1:d_loop){
    print(j)
    d_current = d_vals[j]
    acc_vals_sample = c()
    acc = 0
    
    #Loop multiple times for specified d and take average
    for(d in 1:avg_loop){
      forest = randomForest(x=train_fea, y=as.factor(train_tar[,]),
                            ntree = 1000, maxnodes = d_current)
      pred_forest = predict(forest, test_fea)
      CM_rep = confusionMatrix(table(pred_forest, test_tar[,]), positive = pos_class)
      acc = acc + CM_rep$byClass[11]
    }
    #Average across multiple trains
    acc_avg = acc/avg_loop
    #Add the vector to the overall accruacy vector for all ds
    acc_overall[j] = acc_overall[j] + acc_avg
  }
  
  #Find Optimal d
  optimal_d = d_min + which.max(acc_overall) - 1
  
  #Plot 
  plot(d_vals, acc_overall, main = title, type = 'l', col = "black", 
       xlab = "d", ylab = "Balanced Accuracy", lwd=3)
  abline(v = optimal_d, col="#DD1731", lwd=2)
  
  return(optimal_d)
}

#Find optimal depth that maximizes f1
find_optimal_depth_f1 = function(train, test, fea, tar, pos_class, d_min=2, d_max=20,  
                              s_num=10, avg_loop=5, title = "Depth versus F1 for Forest Model"){
  #Setup Data Structures for loop logic and output
  d_loop = d_max-d_min+1
  f1_overall = numeric(d_loop)
  d_vals = c(d_min:d_max)
  
  train_fea = train %>% select(contains(fea))
  train_tar = train %>% select(contains(tar))
  test_fea = test %>% select(contains(fea))
  test_tar = test %>% select(contains(tar))
  #Loop for every d in range specified by parameters dmax-dmin
  for(j in 1:d_loop){
    d_current = d_vals[j]
    f1_vals_sample = c()
    f1_tot = 0
    
    #Loop multiple times for specified d and take average
    for(d in 1:avg_loop){
      forest = randomForest(x=train_fea, y=as.factor(train_tar[,]),
                            ntree = 1000, maxnodes = d_current)
      pred_forest = predict(forest, test_fea)
      CM_rep = confusionMatrix(table(pred_forest, test_tar[,]), positive = pos_class)
      precision = CM_rep$byClass[5]
      recall = CM_rep$byClass[6]
      f1 = get_f1(precision, recall)
      f1_tot = f1_tot + f1
    }
    #Average across multiple trains
    f1_avg = f1_tot/avg_loop
    #Add the vector to the overall accruacy vector for all ds
    f1_overall[j] = f1_overall[j] + f1_avg
  }
  
  #Find Optimal d
  optimal_d = d_min + which.max(f1_overall) - 1
  
  #Plot 
  plot(d_vals, f1_overall, main = title, type = 'l', col = "black", 
       xlab = "d", ylab = "F1 Score", lwd=3)
  abline(v = optimal_d, col="#DD1731", lwd=2)
  
  return(optimal_d)
}