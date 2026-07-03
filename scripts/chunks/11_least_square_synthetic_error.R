# Source chunk: least square synthetic error
# Original Rmd lines: 603-619


linear_syn_model<-lm(Kansas~., data=as.data.frame(residual_matrix))
summary(linear_syn_model)

linear_syn_model_resid<-resid(linear_syn_model)
#训练集MSE
linear_syn_MSE_train<-mean(linear_syn_model_resid^2)
residual_matrix_valid<-valid_residual_matrix
control_valid_residual<-residual_matrix_valid[, -16]
#验证集MSE
LSM_predictors_valid<-predict(linear_syn_model, newdata =as.data.frame(control_valid_residual))

linear_syn_MSE_valid<-mean((residual_matrix_valid[,16]-LSM_predictors_valid)^2)


linear_syn_MSE_train
linear_syn_MSE_valid#3.338
