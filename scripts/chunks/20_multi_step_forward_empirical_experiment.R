# Source chunk: multi-step-forward empirical experiment
# Original Rmd lines: 1671-1965

#重复上面的，但是要改成逐步向前，不能直接用valid_residual_matrix中kansas的residual来算MSE了
#先做least square
#direct multi-forward(all four covariates)-------------

state<-state_all
set.seed(103)
SE_LM<-lm(Kansas~. , data = as.data.frame(residual_matrix))
lm_coef<-coef(SE_LM)[-1]
alasso_SE_model<-glmnet(x=as.matrix(residual_matrix[,-16]), y=as.matrix(residual_matrix[,16]), alpha=1, penalty.factor = 1/abs(lm_coef))

plot(alasso_SE_model, xvar="lambda")

alasso_SE_model_cv<-cv.glmnet(x=as.matrix(residual_matrix[,-16]), y=as.matrix(residual_matrix[,16]), type.measure = "mse", nfold=10,alpha=1, penalty.factor = 1/abs(lm_coef), keep=TRUE)

plot(alasso_SE_model_cv)

best_lambda<-alasso_SE_model_cv$lambda.min

best_alasso_coef<-coef(alasso_SE_model_cv, s=best_lambda)

#查看训练集误差MSE
alasso_predictors_train<-predict(alasso_SE_model, s=best_lambda, newx = as.matrix(residual_matrix[,-16]))
#计算估计的potential outcome

Kansas_step1_model<-lm(y~., data=Kansas_training_data[,-c(1,2,3,5,8:13)])
Kansas_step1_model<-step(Kansas_step1_model, trace = 0)
Kansas_step1_1forward_pred<-predict(Kansas_step1_model, newdata = Kansas_validation_data[1,-c(1,2,3,5,8:13)])
Kansas_step2_1forward_pred<-predict(alasso_SE_model, s=best_lambda, newx = valid_residual_matrix[1,-16])
Kansas_1forward_pred<-Kansas_setp1_1forward_pred+Kansas_step2_1forward_pred
Kansas_step1_forward_est_error<-Kansas_1forward_pred-Kansas_validation_data[1,'y']
#现在开始进行多步向前更新
#先备份一下pre_treat_data
for(s in state){
  assign(paste(s,'_pre_treat_data_mod', sep=''),get(paste(s,'_pre_treat_data', sep='')))
}


validation_length=16
kansas_se_validation<-rep(0,validation_length)
for(current_row in c(72:83)){
 #browser()
  temp_step1_Kansas_MF_pred<-predict(Kansas_step1_model, newdata = Kansas_pre_treat_data_mod[current_row,-c(1,2,3,5,8:13)])#这里和skip中的数据不一致
  temp_pred_residual<-predict(alasso_SE_model, s=best_lambda, newx = valid_residual_matrix[(current_row-71),-16])
  temp_kansas_se_pred<-temp_step1_Kansas_MF_pred+temp_pred_residual
   #browser()
  update_pred_dataframe1(temp_kansas_se_pred, y_lag_order = 5, ysl_lag_order = 5, current_row = current_row)
}

for(s in state){
  assign(paste(s,'_validation_data_mod', sep=''), get(paste(s,'_pre_treat_data_mod', sep=''))[72:83,])
}

STPSEM_valdiation_MF_error_Kansas<-Kansas_validation_data[,'y']-Kansas_validation_data_mod[,'y']

MEE_validation_MF<-mean(STPSEM_valdiation_MF_error_Kansas[1:10]^2)
MEE_validation_MF

#-----------direct multi-step forward(selected covariates)----------------
set.seed(100)

covariates<-c( "emplvlcapita_dif", 'popestimate_dif', 'qtrly_estabs_count_dif')#"emplvlcapita_dif", 'totalwagescapita_dif', 'popestimate_dif', 'qtrly_estabs_count_dif'
temp_lag_parameters<-c(
      paste0("y_lag", (1:5)),
      paste0("ysl_lag", (1:5))#emplvl记号
    )
    
temp_parameters<-c(temp_lag_parameters, covariates)
#residual_matrix from selected parameters是在这生成的
residual_list<-gen_residual_matrix(temp_parameters)
residual_matrix_NC<-residual_list[[1]]
valid_residual_matrix_NC<-residual_list[[2]]


#如果要排除covaraites就得把residual_matrix给换了
SE_LM<-lm(Kansas~. , data = as.data.frame(residual_matrix_NC))
lm_coef<-coef(SE_LM)[-1]
alasso_SE_model<-glmnet(x=as.matrix(residual_matrix_NC[,-16]), y=as.matrix(residual_matrix_NC[,16]), alpha=1, penalty.factor = 1/abs(lm_coef))

plot(alasso_SE_model, xvar="lambda")

alasso_SE_model_cv<-cv.glmnet(x=as.matrix(residual_matrix_NC[,-16]), y=as.matrix(residual_matrix_NC[,16]), type.measure = "mse", nfold=10,alpha=1, penalty.factor = 1/abs(lm_coef), keep=TRUE)

plot(alasso_SE_model_cv)

best_lambda<-alasso_SE_model_cv$lambda.min

best_alasso_coef<-coef(alasso_SE_model_cv, s=best_lambda)

#查看训练集误差MSE
alasso_predictors_train<-predict(alasso_SE_model, s=best_lambda, newx = as.matrix(residual_matrix_NC[,-16]))
#计算估计的potential outcome

Kansas_step1_model<-lm(y~., data=Kansas_training_data[,c('y', temp_parameters)])
#Kansas_step1_model<-step(Kansas_step1_model, trace = 0)
Kansas_step1_1forward_pred<-predict(Kansas_step1_model, newdata = Kansas_validation_data[1,temp_parameters])
Kansas_step2_1forward_pred<-predict(alasso_SE_model, s=best_lambda, newx = valid_residual_matrix_NC[1,-16])
Kansas_1forward_pred<-Kansas_setp1_1forward_pred+Kansas_step2_1forward_pred
Kansas_step1_forward_est_error<-Kansas_1forward_pred-Kansas_validation_data[1,'y']
#现在开始进行多步向前更新
#先备份一下pre_treat_data
for(s in state){
  assign(paste(s,'_pre_treat_data_mod', sep=''),get(paste(s,'_pre_treat_data', sep='')))
}



kansas_se_validation<-rep(0,16)
for(current_row in c(72:83)){
 #browser()
  temp_step1_Kansas_MF_pred<-predict(Kansas_step1_model, newdata = Kansas_pre_treat_data_mod[current_row,temp_parameters])#这里和skip中的数据不一致
  temp_pred_residual<-predict(alasso_SE_model, s=best_lambda, newx = valid_residual_matrix_NC[(current_row-71),-16])
  temp_kansas_se_pred<-temp_step1_Kansas_MF_pred+temp_pred_residual
   #browser()
  update_pred_dataframe1(temp_kansas_se_pred, y_lag_order = 5, ysl_lag_order = 5, current_row = current_row)
}

for(s in state){
  assign(paste(s,'_validation_data_mod', sep=''), get(paste(s,'_pre_treat_data_mod', sep=''))[72:83,])
}

STPSEM_valdiation_MF_error_Kansas_NC<-Kansas_validation_data[,'y']-Kansas_validation_data_mod[,'y']

MEE_validation_MF_NC<-mean(STPSEM_valdiation_MF_error_Kansas_NC[1:9]^2)
MEE_validation_MF_NC
#写个循环(包含所有covariates)----------------------
MSE_table<-NULL
for (seed_idx in c(100:120)){
  set.seed(seed_idx)
SE_LM<-lm(Kansas~. , data = as.data.frame(residual_matrix))
lm_coef<-coef(SE_LM)[-1]
alasso_SE_model<-glmnet(x=as.matrix(residual_matrix[,-16]), y=as.matrix(residual_matrix[,16]), alpha=1, penalty.factor = 1/abs(lm_coef))

plot(alasso_SE_model, xvar="lambda")

alasso_SE_model_cv<-cv.glmnet(x=as.matrix(residual_matrix[,-16]), y=as.matrix(residual_matrix[,16]), type.measure = "mse", nfold=10,alpha=1, penalty.factor = 1/abs(lm_coef), keep=TRUE)

plot(alasso_SE_model_cv)

best_lambda<-alasso_SE_model_cv$lambda.min

best_alasso_coef<-coef(alasso_SE_model_cv, s=best_lambda)

#查看训练集误差MSE
alasso_predictors_train<-predict(alasso_SE_model, s=best_lambda, newx = as.matrix(residual_matrix[,-16]))
#计算估计的potential outcome

Kansas_step1_model<-lm(y~., data=Kansas_training_data[,-c(1,2,3,5,8:13)])
#Kansas_step1_model<-step(Kansas_step1_model, trace = 0)
Kansas_step1_1forward_pred<-predict(Kansas_step1_model, newdata = Kansas_validation_data[1,-c(1,2,3,5,8:13)])
Kansas_step2_1forward_pred<-predict(alasso_SE_model, s=best_lambda, newx = valid_residual_matrix[1,-16])
Kansas_1forward_pred<-Kansas_setp1_1forward_pred+Kansas_step2_1forward_pred
Kansas_step1_forward_est_error<-Kansas_1forward_pred-Kansas_validation_data[1,'y']
#现在开始进行多步向前更新
#先备份一下pre_treat_data
for(s in state){
  assign(paste(s,'_pre_treat_data_mod', sep=''),get(paste(s,'_pre_treat_data', sep='')))
}



kansas_se_validation<-rep(0,16)
for(current_row in c(72:83)){
 #browser()
  temp_step1_Kansas_MF_pred<-predict(Kansas_step1_model, newdata = Kansas_pre_treat_data_mod[current_row,-c(1,2,3,5,8:13)])#这里和skip中的数据不一致
  temp_pred_residual<-predict(alasso_SE_model, s=best_lambda, newx = valid_residual_matrix[(current_row-71),-16])
  temp_kansas_se_pred<-temp_step1_Kansas_MF_pred+temp_pred_residual
   #browser()
  update_pred_dataframe1(temp_kansas_se_pred, y_lag_order = 5, ysl_lag_order = 5, current_row = current_row)
}

for(s in state){
  assign(paste(s,'_validation_data_mod', sep=''), get(paste(s,'_pre_treat_data_mod', sep=''))[72:83,])
}

STPSEM_valdiation_MF_error_Kansas<-Kansas_validation_data[,'y']-Kansas_validation_data_mod[,'y']

MSE_temp_row<-NULL
for(ps in c(1:11)){
  MEE_validation_MF<-mean(STPSEM_valdiation_MF_error_Kansas[ps:11]^2)
  MSE_temp_row<-cbind(MSE_temp_row,MEE_validation_MF)
  
}
MSE_table<-rbind(MSE_table,MSE_temp_row)
colMeans(MSE_table)
ts.plot(colMeans(MSE_table))
# MEE_validation_MF<-mean(STPSEM_valdiation_MF_error_Kansas[11:12]^2)
# MEE_validation_MF
# MSE_table<-rbind(MSE_table,MEE_validation_MF)
# print(paste('seed=', seed_idx, 'MSE=', MEE_validation_MF))
}




#写个循环（包含部分covariates）----------------
covariates<-c("emplvlcapita_dif",  'popestimate_dif', 'qtrly_estabs_count_dif')#"emplvlcapita_dif", 'totalwagescapita_dif', 'popestimate_dif', 'qtrly_estabs_count_dif'
temp_lag_parameters<-c(
      paste0("y_lag", (1:5)),
      paste0("ysl_lag", (1:5))#emplvl记号
    )
    
temp_parameters<-c(temp_lag_parameters, covariates)

#重新做一个residual_NC_matrix, 加入第15列emplvlcapita_diff的
residual_matrix_NC<-NULL
valid_residual_matrix_NC<-NULL

for(s in state){
# print(state)
  temp_data<-get(paste(s, "_training_data",sep = ""))

  
  temp_model<-lm(y~., data=temp_data[,c('y',temp_parameters)])
  #temp_tstep<-temp_model
  temp_tstep<-step(temp_model, trace = 0)
  assign(paste0(s, '_linear_model'), temp_tstep)
  temp_residuals<-temp_tstep$residuals
  residual_matrix_NC<-cbind(residual_matrix_NC,temp_residuals)#先全放在一起
  #再做一个rbind的符合syntheti格式的dataframe
  
  assign(paste(s,"_residual", sep=""), temp_residuals)
  temp_data_valid<-get(paste(s,'_validation_data', sep=''))
  #计算模型在验证集中的预测误差
  temp_valid_resid<-temp_data_valid[,4]-predict(temp_tstep,temp_data_valid[,temp_parameters])
  assign(paste(s,'valid_resid', sep=''),temp_valid_resid)
  valid_residual_matrix_NC<-cbind(valid_residual_matrix_NC, temp_valid_resid)
}
colnames(residual_matrix_NC)<-state
colnames(valid_residual_matrix_NC)<-state
mean(residual_matrix_NC[,16]^2)




MF_NC_MSE_table<-NULL
for (seed_idx in c(100:120)){
  set.seed(seed_idx)
#如果要排除covaraites就得把residual_matrix给换了
SE_LM<-lm(Kansas~. , data = as.data.frame(residual_matrix_NC))#对于不同的covariates选择这个resial_matrix也不一样，所以每次也要重新算一下
lm_coef<-coef(SE_LM)[-1]
alasso_SE_model<-glmnet(x=as.matrix(residual_matrix_NC[,-16]), y=as.matrix(residual_matrix_NC[,16]), alpha=1, penalty.factor = 1/abs(lm_coef))

plot(alasso_SE_model, xvar="lambda")

alasso_SE_model_cv<-cv.glmnet(x=as.matrix(residual_matrix_NC[,-16]), y=as.matrix(residual_matrix_NC[,16]), type.measure = "mse", nfold=10,alpha=1, penalty.factor = 1/abs(lm_coef), keep=TRUE)

plot(alasso_SE_model_cv)

best_lambda<-alasso_SE_model_cv$lambda.min

best_alasso_coef<-coef(alasso_SE_model_cv, s=best_lambda)

#查看训练集误差MSE
alasso_predictors_train<-predict(alasso_SE_model, s=best_lambda, newx = as.matrix(residual_matrix_NC[,-16]))
#计算估计的potential outcome

Kansas_step1_model<-lm(y~., data=Kansas_training_data[,c('y',temp_parameters)])
#Kansas_step1_model<-step(Kansas_step1_model, trace = 0)
Kansas_step1_1forward_pred<-predict(Kansas_step1_model, newdata = Kansas_validation_data[1,temp_parameters])
Kansas_step2_1forward_pred<-predict(alasso_SE_model, s=best_lambda, newx = valid_residual_matrix_NC[1,-16])
Kansas_1forward_pred<-Kansas_setp1_1forward_pred+Kansas_step2_1forward_pred
Kansas_step1_forward_est_error<-Kansas_1forward_pred-Kansas_validation_data[1,'y']
#现在开始进行多步向前更新
#先备份一下pre_treat_data
for(s in state){
  assign(paste(s,'_pre_treat_data_mod', sep=''),get(paste(s,'_pre_treat_data', sep='')))
}



kansas_se_validation<-rep(0,16)
for(current_row in c(72:83)){
 #browser()
  temp_step1_Kansas_MF_pred<-predict(Kansas_step1_model, newdata = Kansas_pre_treat_data_mod[current_row,temp_parameters])#这里和skip中的数据不一致
  temp_pred_residual<-predict(alasso_SE_model, s=best_lambda, newx = valid_residual_matrix_NC[(current_row-71),-16])
  temp_kansas_se_pred<-temp_step1_Kansas_MF_pred+temp_pred_residual
   #browser()
  update_pred_dataframe1(temp_kansas_se_pred, y_lag_order = 5, ysl_lag_order = 5, current_row = current_row)
}

for(s in state){
  assign(paste(s,'_validation_data_mod', sep=''), get(paste(s,'_pre_treat_data_mod', sep=''))[72:83,])
}

STPSEM_valdiation_MF_error_Kansas_NC<-Kansas_validation_data[,'y']-Kansas_validation_data_mod[,'y']

MEE_validation_MF_NC<-mean(STPSEM_valdiation_MF_error_Kansas_NC[1:11]^2)
MEE_validation_MF_NC
MF_NC_MSE_table<-rbind(MF_NC_MSE_table, MEE_validation_MF_NC)
}
mean(MF_NC_MSE_table)
var(MF_NC_MSE_table)

