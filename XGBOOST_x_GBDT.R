#定义损失函数
logloss<-function(pred,true){
  less<-sum(true*log(pred)+(1-true)*log(1-pred))/(-length(pred))      #测试Logless
  return(less)
}   
#--------------------------------------------------------模型训练-----------------------------------------------------
#训练集的训练
xgb_train_response<-data.frame(rbind(test_feature_label_1,test_feature_label_2,test_feature_label_3))$is_trade
xgb_train_data<-data.frame(rbind(test_feature_label_1,test_feature_label_2,test_feature_label_3)) %>%
  select(-c(instance_id:item_city_id,user_id,context_id,shop_id,almm_date:time_class,user_gender_id,user_occupation_id,is_trade))
xgb_test_data<-select(data.frame(test_feature_label_4),
                       -c(instance_id:item_city_id,user_id,user_gender_id,context_id,shop_id,user_occupation_id,almm_date:time_class))
library(xgboost)

xgb_train_model<-xgboost(data.matrix(xgb_train_data),xgb_train_response,nrounds=1500,
                         objective="binary:logistic",eval_metric="logloss",lambda=3,
                         subsample=0.7,colsample_bytree=0.3,gamma=5,max_depth=4,min_child_weight=5,eta=0.01)
#logless(rep(0.01,nrow(almm_train_data)),almm_train_is_trade)
colnames_xgb_train_data<-dimnames(data.matrix(xgb_train_data))[[2]]
importance_matrix <- xgb.importance(colnames_xgb_train_data, model =xgb_train_model)
xgb.plot.importance(importance_matrix[1:20,],cex=1.1)


#------------------------------------------------------xgboost提交------------------------------------------------
xgb_test_data_value<-predict(xgb_train_model,data.matrix(xgb_test_data))
xgb_test_data_p2<-data.frame(cbind(almm_test$instance_id,xgb_test_data_value))
xgb_test_data_submssion<-xgb_test_data_p2[which(almm_test$instance_id %in% almm_test_part2$instance_id),]
names(xgb_test_data_submssion)<-c("instance_id","predicted_score")
summary(as.numeric(as.character(xgb_test_data_submssion$predicted_score)))
write.table(xgb_test_data_submssion,"F:/SUE/天池/xgboost_submission_0422.txt",quote = F,row.names = F,eol = "\r",sep=" ")

#用test1 测试test2
xgb_train_12_data<-select(data.frame(rbind(test_feature_label_1,test_feature_label_2)),
               -c(instance_id:item_city_id,user_id,context_id,user_gender_id,
                  user_occupation_id,shop_id,almm_date:time_class,is_trade))
xgb_train_12_response<-data.frame(rbind(test_feature_label_1,test_feature_label_2))$is_trade

xgb_train_12_model<-xgboost(data.matrix(xgb_train_12_data),xgb_train_12_response,nrounds=500,
                   objective="binary:logistic",eval_metric="logloss",lambda=3,
                   subsample=0.7,colsample_bytree=0.3,gamma=5,max_depth=4,min_child_weight=5,eta=0.02)

#xgb_train_3_data<-select(data.frame(test_feature_label_3),-c(instance_id:item_city_id,user_id,context_id,
#                                              user_occupation_id,shop_id,user_gender_id,is_trade,almm_date:time_class))
xgb_test_3_value<-predict(xgb_train_12_model,data.matrix(xgb_train_3_data))
logloss(xgb_test_3_value,test_feature_label_3$is_trade)
#logloss(rep(0.01,length(xgb_test_3_value)),test_feature_label_3$is_trade)
#colnames_xgb_train_12_data<-dimnames(data.matrix(xgb_train_12_data))[[2]]
importance_matrix_12 <- xgb.importance(colnames_xgb_train_12_data, model =xgb_train_12_model)
xgb.plot.importance(importance_matrix_12[1:30,],cex=1.05)
head(importance_matrix_12,n=30)[,1:2]




#---------------------------------------------GBDT建模------------------------------------------
library(gbm)
gbm_train_data<-data.frame(rbind(test_feature_label_1,test_feature_label_2,test_feature_label_3)) %>%
  select(-c(instance_id:item_city_id,user_id,context_id,shop_id,almm_date:time_class,user_gender_id,user_occupation_id))
for(i in 1:ncol(gbm_train_data)){
  gbm_train_data[which(is.na(gbm_train_data[,i])),i]<-NA
}


gbm_train_model<-gbm(is_trade~.,data=gbm_train_data,distribution = "adaboost",n.trees = 200,shrinkage =0.05,
                     bag.fraction = 0.7,cv.folds = 4,interaction.depth=3)
best.iter <- gbm.perf(gbm_train_model,method="cv")

gbm_train_test<-predict(gbm_train_model,gbm_train_data,type="response")
logloss(gbm_train_test,gbm_train_data$is_trade)

#----------------------------------------------------------GBDT提交------------------------------------------------

gbm_test_data<-select(data.frame(test_feature_label_4),
                      -c(instance_id:item_city_id,user_id,user_gender_id,context_id,shop_id,
                         user_occupation_id,almm_date:time_class))
gbm_test_data_value<-predict(gbm_train_model,gbm_test_data,type="response")
gbm_test_data_p2<-data.frame(cbind(almm_test$instance_id,gbm_test_data_value))
gbm_test_data_submssion<-gbm_test_data_p2[which(almm_test$instance_id %in% almm_test_part2$instance_id),]
names(gbm_test_data_submssion)<-c("instance_id","predicted_score")  
write.table(gbm_test_data_submssion,"F:/SUE/天池/GBDT_submission_0422.txt",quote = F,row.names = F,eol = "\r",sep=" ")


#-----------------------------------------------------test1->test2  GBDT-----------------------------
library(gbm)
gbm_test_12_data<-select(data.frame(rbind(test_feature_label_1,test_feature_label_2)),
                         -c(instance_id:item_city_id,user_id,context_id,user_gender_id,
                            user_occupation_id,shop_id,almm_date:time_class))

for(i in 1:ncol(gbm_test_12_data)){
  gbm_test_12_data[which(is.na(gbm_test_12_data[,i])),i]<-NA
}    #训练集
gbm_test_3_data<-select(data.frame(test_feature_label_3),-c(instance_id:item_city_id,user_id,context_id,
                                 user_occupation_id,shop_id,user_gender_id,is_trade,almm_date:time_class))
for(i in 1:ncol(gbm_test_3_data)){
  gbm_test_3_data[which(is.na(gbm_test_3_data[,i])),i]<-NA
}   #测试集

gbm_test_12_model<-gbm(is_trade~.,data=gbm_test_12_data,distribution ="adaboost",n.trees = 1000,shrinkage =0.02,
                    bag.fraction = 0.7,cv.folds = 4,interaction.depth=3)
                  

best.iter <- gbm.perf(gbm_test_12_model,method="cv")
gbm_test_3_data_value<-predict(gbm_test_12_model,gbm_test_3_data,type="response")
logloss(gbm_test_3_data_value,test_feature_label_3$is_trade)

gbm_test_12_model2<-gbm(is_trade~.,data=gbm_test_12_data,distribution ="bernoulli",n.trees = 100,shrinkage =0.05,
                       bag.fraction = 0.7,cv.folds = 4,interaction.depth=3)


best.iter <- gbm.perf(gbm_test_12_model2,method="cv")
gbm_test_3_data_value2<-predict(gbm_test_12_model2,gbm_test_3_data,type="response")
logloss(gbm_test_3_data_value2,test_feature_label_3$is_trade)



##########GBDT x  XGBOOST
#p1--------------------------------用两组来找到最佳组合
logloss_combine<-NULL
for(i in 0:10){
  predict_value<-i*gbm_train_test2/10+(10-i)*xgb_test2_value/10
  logloss_combine[i]<-logless(predict_value,gbm_test_2$is_trade)
}
which.min(logloss_combine)

#-----------------------------------------------------------mix 提交结果------------------------------------
xgb_gbm_mix_data<-0.4*gbm_test_data_value+0.6*xgb_test_data_value
mix_test_data_p2<-data.frame(cbind(almm_test$instance_id,xgb_gbm_mix_data))
mix_test_data_submssion<-mix_test_data_p2[which(almm_test$instance_id %in% almm_test_part2$instance_id),]
names(mix_test_data_submssion)<-c("instance_id","predicted_score")  
write.table(mix_test_data_submssion,"F:/SUE/天池/xgb_gbdt_submission_0422.txt",quote = F,row.names = F,eol = "\r",sep=" ")
