# Source chunk: simulation main
# Original Rmd lines: 2676-2715

residual_matrix_simu<-NULL
for(i in c(1:50)){
  temp_data<-get(paste('unit_',i, "_pre",sep = ""))

  
  temp_model<-lm(y~., data=temp_data[,-c( 17:24)])
  temp_tstep<-step(temp_model, trace = 0)
  assign(paste('simu_unit', i, '_LST_model', sep=''), temp_tstep)
  temp_residuals<-temp_tstep$residuals
  residual_matrix_simu<-cbind(residual_matrix_simu,temp_residuals)#先全放在一起
  #再做一个rbind的符合syntheti格式的dataframe
  assign(paste(s,"_residual_simu", sep=""),temp_residuals)

}

colnames(residual_matrix_simu)<-state



SE_LM_simu<-lm(Kansas~. , data = as.data.frame(residual_matrix_simu))
lm_coef_simu<-coef(SE_LM_simu)[-1]
alasso_SE_pred_model_simu<-glmnet(x=as.matrix(residual_matrix_simu[,-16]), y=as.matrix(residual_matrix_simu[,16]), alpha=1, penalty.factor = 1/abs(lm_coef_simu))

plot(alasso_SE_pred_model_simu, xvar="lambda")



alasso_SE_pred_model_simu_cv<-cv.glmnet(x=as.matrix(residual_matrix_simu[,-16]), y=as.matrix(residual_matrix_simu[,16]), type.measure = "mse", nfold=10,alpha=1, penalty.factor = 1/abs(lm_coef_simu), keep=TRUE)

plot(alasso_SE_pred_model_simu_cv)

best_lambda_simu<-alasso_SE_pred_model_simu_cv$lambda.min

best_alasso_coef_simu<-coef(alasso_SE_pred_model_simu_cv, s=best_lambda_simu)

best_alasso_coef_simu
#不行现在这个kansas(unit16)的simulation模型根本就不需要x，我得扩大x的影响力

