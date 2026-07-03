# Source chunk: residual from LST
# Original Rmd lines: 544-601

#------------generate residual matrix----
residual_matrix<-NULL
valid_residual_matrix<-NULL

for(s in state){
# print(state)
  temp_data<-get(paste(s, "_training_data",sep = ""))[,-1]

  
  temp_model<-lm(y~., data=temp_data[,-c(1,2,4,7:12)])
  temp_tstep<-step(temp_model, trace = 0)
  assign(paste0(s, '_linear_model'), temp_tstep)
  temp_residuals<-temp_tstep$residuals
  residual_matrix<-cbind(residual_matrix,temp_residuals)#先全放在一起
  #再做一个rbind的符合syntheti格式的dataframe
  
  assign(paste(s,"_residual", sep=""), temp_residuals)
  temp_data_valid<-get(paste(s,'_validation_data', sep=''))[,-1]
  #计算模型在验证集中的预测误差
  temp_valid_resid<-temp_data_valid[,3]-predict(temp_tstep,temp_data_valid[,-c(1,2,4,7:12)])
  assign(paste(s,'valid_resid', sep=''),temp_valid_resid)
  valid_residual_matrix<-cbind(valid_residual_matrix, temp_valid_resid)
}
colnames(residual_matrix)<-state
colnames(valid_residual_matrix)<-state
mean(residual_matrix[,16]^2)

# residual_matrix1<-read.csv('data/total_matrix/diff_using_60train_data/residual_matrix.csv')[,-1]
# mean(residual_matrix1[,16]^2)

#------------generate no/selected covaraites residual_matrix_NC---------------------------
residual_matrix_NC<-NULL
valid_residual_matrix_NC<-NULL

for(s in state){
# print(state)
  temp_data<-get(paste(s, "_training_data",sep = ""))

  
  temp_model<-lm(y~., data=temp_data[,-c(1,2,3,5,8:17)])
  temp_tstep<-step(temp_model, trace = 0)
  assign(paste0(s, '_linear_model'), temp_tstep)
  temp_residuals<-temp_tstep$residuals
  residual_matrix_NC<-cbind(residual_matrix_NC,temp_residuals)#先全放在一起
  #再做一个rbind的符合syntheti格式的dataframe
  
  assign(paste(s,"_residual", sep=""), temp_residuals)
  temp_data_valid<-get(paste(s,'_validation_data', sep=''))
  #计算模型在验证集中的预测误差
  temp_valid_resid<-temp_data_valid[,4]-predict(temp_tstep,temp_data_valid[,-c(1,2,3,5,8:17)])
  assign(paste(s,'valid_resid', sep=''),temp_valid_resid)
  valid_residual_matrix_NC<-cbind(valid_residual_matrix_NC, temp_valid_resid)
}
colnames(residual_matrix_NC)<-state
colnames(valid_residual_matrix_NC)<-state
mean(residual_matrix_NC[,16]^2)
