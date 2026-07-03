# Source chunk: direct skip backup
# Original Rmd lines: 3473-3618

#state_all<-state
#mean(MF_NC_MSE_table)=1.102247
#----------------skip_step_multi_step_forward------------------
#skip_lag_order<-5
# fit_model_with_lags <- function(data, y_var, y_lag_range, ysl_lag_range) {
#   # data: 原始数据
#   # y_var: 因变量列名
#   # y_lag_range, ysl_lag_range: 各自的 lag 范围，例如 1:5
#   
#   df <- data
#   
#   # 动态生成 y_lag
#   for (k in y_lag_range) {
#     if(k<=5){
#       next
#     }else{
#       df[[paste0("y_lag", k)]] <- dplyr::lag(df[[y_var]], k)
#     }
#   }
#   
#   # 动态生成 ysl_lag
#   for (k in ysl_lag_range) {
#         if(k<=5){
#       next
#     }else{
#     df[[paste0("ysl_lag", k)]] <- dplyr::lag(df[["ysl"]], k)
#     }
#   }
#   
#   
#   #browser()
#   df_clean<-na.omit(df)
#   #拟合公式字符串
#   formula_str <- paste(
#     y_var,
#     "~",
#     paste(c(
#       paste0("y_lag", y_lag_range),
#       paste0("ysl_lag", ysl_lag_range),
#       'totalwagescapita_dif',
#       'emplvlcapita_dif',
#       'popestimate_dif',
#       'qtrly_estabs_count_dif'
#     ), collapse = " + ")
#   )
#   
#   # lm 回归
# 
#   model <- lm(as.formula(formula_str), data = df_clean)
#   model<-step(model)
#   return(model)
# }
# #build model
# for(s in state){
#   temp_training_data<-get(paste0(s,'_training_data'))
#   for(forward_step in c(1:12)){
#     temp_skip_model<-fit_model_with_lags(temp_training_data, 'y', ((forward_step):(forward_step+skip_lag_order-1)), ((forward_step):(forward_step+skip_lag_order-1)))
#     assign(paste0(s,'_skip_model_step',forward_step), temp_skip_model)
#   }
# }
# #predict
# set.seed(150)
# for(forward_step in c(1:12)){
#   #linear predict
#   for(s in state){
#     #validation data要有足够的lag，可能得用pre_treat_data来弄一下
#     temp_pre_treat_data<-get(paste0(s, '_pre_treat_data'))
#     for(j in c(6:(skip_lag_order+forward_step))){
#       temp_pre_treat_data[,paste0('y_lag', j)]<-dplyr::lag(temp_pre_treat_data[['y_lag5']], (j-5))
#       temp_pre_treat_data[,paste0('ysl_lag', j)]<-dplyr::lag(temp_pre_treat_data[['ysl_lag5']], (j-5))
#     }
#     # for(j in c(1:forward_step)){
#     #   temp_pre_treat_data[,paste0('y_lag', j+skip_lag_order-1)]<-dplyr::lag(temp_pre_treat_data[['y_lag5']], j)
#     #   temp_pre_treat_data[,paste0('ysl_lag', j+skip_lag_order-1)]<-dplyr::lag(temp_pre_treat_data[['ysl_lag5']], j)
#     # }
#     temp_validation_data<-temp_pre_treat_data[72:83,]
#     assign(paste0(s, '_skip_validation_data_step', forward_step),temp_validation_data)
#     
# 
#     
#     temp_LM_pred<-predict(get(paste0(s,'_skip_model_step',forward_step)), newdata=temp_validation_data[forward_step,-c(1,2,3,5,8:13) ])
#     assign(paste0(s,'_step',forward_step, '_LM_pred'), temp_LM_pred)
#   }
#   #extract residuals and synthetic residuals
#   temp_skip_residual_matrix<-NULL
#   for(s in state){
#     temp_training_residuals<-get(paste0(s,'_skip_model_step',forward_step))$residuals
#     temp_skip_residual_matrix<-cbind(temp_skip_residual_matrix,temp_training_residuals)
#   }
#   colnames(temp_skip_residual_matrix)<-state
#   assign(paste0('skip_residual_matrix_step',forward_step), temp_skip_residual_matrix)
#   #对所有forward_step对应的residual_matrix建立synthetic error model
#   #由于数据量小，参数大，用lm回归不行，然后我用了ridge之后结果更离谱了
#   SE_ridge_skip<-glmnet(x=as.matrix(temp_skip_residual_matrix[,-16]), y=as.matrix(temp_skip_residual_matrix[,16]), alpha=0)
#   SE_ridge_skip_cv<-cv.glmnet(x=as.matrix(temp_skip_residual_matrix[,-16]), y=as.matrix(temp_skip_residual_matrix[,16]),type.measure="mse", nfold=10, alpha=0)
#   best_ridge_lambda<-SE_ridge_skip_cv$lambda.min
#   lm_coef_skip<-coef(SE_ridge_skip, s=best_ridge_lambda)
#   lm_coef_skip<-lm_coef_skip[-1,]
# 
#   
#   # SE_LM_skip<-lm(Kansas~. , data = as.data.frame(temp_skip_residual_matrix))
#   # lm_coef_skip<-coef(SE_LM_skip)[-1]
#   
# temp_alasso_skip_SE_model<-glmnet(x=as.matrix(temp_skip_residual_matrix[,-16]), y=as.matrix(temp_skip_residual_matrix[,16]), alpha=1, penalty.factor = 1/abs(lm_coef_skip))
# 
# temp_alasso_skip_SE_model_cv<-cv.glmnet(x=as.matrix(temp_skip_residual_matrix[,-16]), y=as.matrix(temp_skip_residual_matrix[,16]),type.measure="mse", nfold=10, alpha=1, penalty.factor = 1/abs(lm_coef_skip), keep=TRUE)
# 
# temp_alasso_skip_SE_model_cv<-cv.glmnet(x=as.matrix(temp_skip_residual_matrix[,-16]), y=as.matrix(temp_skip_residual_matrix[,16]), type.measure = "mse", nfold=10,alpha=1, penalty.factor = 1/abs(lm_coef_skip), keep=TRUE)
# 
# plot(temp_alasso_skip_SE_model_cv)
# #browser()
# temp_best_lambda_skip<-temp_alasso_skip_SE_model_cv$lambda.min
# coef(temp_alasso_skip_SE_model, s=temp_best_lambda_skip)
# 
# #计算当前forwardstep的预测误差
# # for(s in state){
# #   get(paste0(s,'_step',forward_step, '_LM_pred'))
# # }
# #browser()
# skip_validation_errors<-NULL
# for(s in state){
#   temp_validation_data<-get(paste0(s, '_validation_data'))
#   temp_LM_pred<-get(paste0(s,'_step',forward_step, '_LM_pred'))
#   temp_error<-temp_validation_data[forward_step,'y']-temp_LM_pred
#   skip_validation_errors<-cbind(skip_validation_errors, temp_error)
# }
# colnames(skip_validation_errors)<-state
# 
# 
# temp_skip_SE_error<-predict(temp_alasso_skip_SE_model_cv, s=temp_best_lambda_skip, newx = skip_validation_errors[,-16])
# 
# temp_Kansas_LM_pred<-get(paste0('Kansas_step',forward_step, '_LM_pred'))
# temp_Kansas_skip_STPSEM_pred<-temp_Kansas_LM_pred+temp_skip_SE_error
# assign(paste0('Kansas_skip_STPSEM_forwardstep_pred', forward_step), temp_Kansas_skip_STPSEM_pred)
# }
# 
# 
# Kansas_skip_valid_pred<-rep(0,12)
# for(i in c(1:12)){
#   temp_kansas_skip_valid_pred<-print(get(paste0('Kansas_skip_STPSEM_forwardstep_pred', i)))
#   Kansas_skip_valid_pred[i]<-temp_kansas_skip_valid_pred
# }
# MEE_validation_skip_model<-mean((Kansas_validation_data[c(1:12),'y']-Kansas_skip_valid_pred)^2)

