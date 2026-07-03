# Source chunk: empirical results by adaptive LASSO--prediction part
# Original Rmd lines: 771-976

#Spatial temporal linear model for each location
pre_and_post_residual_matrix<-NULL
pre_and_post_pred_residual_matrix<-NULL
for(s in state){
  temp_data<-get(paste(s, "_pre_treat_data",sep = ""))[,-1]

  temp_data_pred<-read.csv(paste("data/total_matrix/diff_using_60train_data/",s,"_post.csv", sep = ""))[,-1]
  
  temp_model<-lm(y~., data=temp_data[,-c(1,2,4, 7:12)])
  temp_tstep<-step(temp_model, trace = 0)
  temp_residuals<-temp_tstep$residuals
  pre_and_post_residual_matrix<-cbind(pre_and_post_residual_matrix,temp_residuals)#先全放在一起
  #再做一个rbind的符合syntheti格式的dataframe
  
  assign(paste(s,"_total_residual", sep=""),temp_residuals)
  assign(paste(s,"_matrix_pre_and_post_mod", sep=""),temp_data)
  assign(paste(s,"_pred_data", sep=""),temp_data_pred)
  assign(paste(s,"_LST_model", sep=""), temp_tstep)

  #计算模型在预测集中的预测误差
  temp_pred_resid<-predict(temp_tstep,temp_data_pred[,-c(1,2,7:12)])-temp_data_pred[,3]
  assign(paste(s,'real_pred_resid', sep=''),temp_pred_resid)
  pre_and_post_pred_residual_matrix<-cbind(pre_and_post_pred_residual_matrix, temp_pred_resid)
}
colnames(pre_and_post_residual_matrix)<-state
colnames(pre_and_post_pred_residual_matrix)<-state
#-------------------------------------------------------------------------------------
outpath<-'data/total_matrix/diff_using_60train_data/'
write.csv(pre_and_post_residual_matrix, file=paste(outpath, 'pre_treatment_residual_matrix.csv', sep=""))
write.csv(pre_and_post_pred_residual_matrix, file=paste(outpath, 'post_treatment_residual_matrix.csv', sep=""))

#估计post-treatment的potential outcomes
Kansas_temp<-Kansas_pre_treat_data[,-1]
kansas_st_pred_model<-lm(y~., data=Kansas_temp[,-c(1,2,7:12)])
kansas_st_pred_model_tstep<-step(kansas_st_pred_model, trace = 0)
#基于LST进行预测
Kansas_post<-read.csv("data/total_matrix/diff_using_60train_data/Kansas_post.csv")[,-1]
LST_pred_Kansas<-predict(kansas_st_pred_model_tstep, Kansas_post[,-c(1,2, 7:12)])

#开始adaptive lasso
set.seed(150)
SE_LM<-lm(Kansas~. , data = as.data.frame(pre_and_post_residual_matrix))
lm_coef<-coef(SE_LM)[-1]
alasso_SE_pred_model<-glmnet(x=as.matrix(pre_and_post_residual_matrix[,-16]), y=as.matrix(pre_and_post_residual_matrix[,16]), alpha=1, penalty.factor = 1/abs(lm_coef))

plot(alasso_SE_pred_model, xvar="lambda")



alasso_SE_pred_model_cv<-cv.glmnet(x=as.matrix(pre_and_post_residual_matrix[,-16]), y=as.matrix(pre_and_post_residual_matrix[,16]), type.measure = "mse", nfold=10,alpha=1, penalty.factor = 1/abs(lm_coef), keep=TRUE)

plot(alasso_SE_pred_model_cv)

best_lambda<-alasso_SE_pred_model_cv$lambda.min

best_alasso_coef<-coef(alasso_SE_pred_model_cv, s=best_lambda)

best_alasso_coef
#计算合成residual--adaptivelasso
pred_residual<-predict(alasso_SE_pred_model, s=best_lambda, newx = as.matrix(pre_and_post_pred_residual_matrix[,-16]))
#计算合成residual--LSE
pred_residual_LST<-predict(SE_LM, newdata =as.data.frame(pre_and_post_pred_residual_matrix[,-16]) )

#post_treatment数据中的延迟项要改，每预测一步就要改一次
#第一步的预测是
kansas_pred_step1<-LST_pred_Kansas[1]+pred_residual[1]

#给一个pred_data_mod作为pred_data的副本
for(s in state){
  assign(paste(s,'_pred_data_mod', sep=''),get(paste(s,'_pred_data', sep='')))
}

update_pred_dataframe(kansas_pred_step1, 2)


LST_pred_Kansas<-predict(kansas_st_pred_model_tstep, Kansas_post[,-c(1,2, 7:12)])

#我这再初始化一遍以免以后整片跑代码的时候出错
#给一个pred_data_mod作为pred_data的副本
for(s in state){
  assign(paste(s,'_pred_data_mod', sep=''),get(paste(s,'_pred_data', sep='')))
}

kansas_se_pred<-rep(0,16)
kansas_LST_pred<-rep(0,16)
kansas_LSE_se_pred<-rep(0,16)
for(fstep in c(1:16)){
  temp_LST_pred<-predict(kansas_st_pred_model_tstep, Kansas_pred_data_mod[,-c(1,2, 7:12)])
  kansas_se_pred[fstep]<-temp_LST_pred[fstep]+pred_residual[fstep]
  kansas_LST_pred[fstep]<-temp_LST_pred[fstep]
  kansas_LSE_se_pred[fstep]<-temp_LST_pred[fstep]+pred_residual_LST[fstep]
  update_pred_dataframe(kansas_se_pred[fstep], fstep+1)
}

Kansas_post[,3]

#观测值
Kansas_post[,3]
obs_kansas_diff_post<-Kansas_post[,3]/100
#synthetic error 结果
kansas_se_pred
#LST结果
kansas_LST_pred
#没有penalty的synthetic error 结果
kansas_LSE_se_pred

#将差分数据恢复成原始数据--- 观测值(记得要除100)
last_pre_lngdpcapita<-kansas[1664,'lngdpcapita']
post_obs_lngdpcapita<-rep(0,16)
last_value<-last_pre_lngdpcapita
for(i in c(1:16)){
  post_obs_lngdpcapita[i]<-last_value+obs_kansas_diff_post[i]/100
  last_value<-post_obs_lngdpcapita[i]
}
post_obs_lngdpcapita_series<-ts(post_obs_lngdpcapita,start = 90)





#将差分数据恢复成原始数据--- adaptive lasso synthetic error model(一定要注意这个数据集里的数据乘100了，最后还原的时候要除回来)
last_pre_lngdpcapita<-kansas[1664,'lngdpcapita']

post_se_lngdpcapita<-rep(0,16)
last_value<-last_pre_lngdpcapita
for(i in c(1:16)){
  post_se_lngdpcapita[i]<-last_value+kansas_se_pred[i]/100
  last_value<-post_se_lngdpcapita[i]
}
post_se_lngdpcapita_series<-ts(post_se_lngdpcapita,start = 90)



#将差分数据恢复成原始数据---LST(一定要注意这个数据集里的数据乘100了，最后还原的时候要除回来)
post_LST_lngdpcapita<-rep(0,16)
last_value<-last_pre_lngdpcapita
for(i in c(1:16)){
  post_LST_lngdpcapita[i]<-last_value+kansas_LST_pred[i]/100
  last_value<-post_LST_lngdpcapita[i]
}
post_LST_lngdpcapita_series<-ts(post_LST_lngdpcapita,start = 90)



#将差分数据恢复成原始数据---no penalty synthetic error(一定要注意这个数据集里的数据乘100了，最后还原的时候要除回来)
post_LSE_se_lngdpcapita<-rep(0,16)
last_value<-last_pre_lngdpcapita
for(i in c(1:16)){
  post_LSE_se_lngdpcapita[i]<-last_value+kansas_LSE_se_pred[i]/100
  last_value<-post_LSE_se_lngdpcapita[i]
}
post_LSE_se_lngdpcapita_series<-ts(post_LSE_se_lngdpcapita,start = 90)



obs_kansas_lngdpcapita<-kansas[kansas$state=='Kansas',]$lngdpcapita[60:105]

obs_kansas_lngdpcapita<-ts(obs_kansas_lngdpcapita, start=60)


#计算 causal effect adaptive lasso se
post_obs_lngdpcapita<-kansas[kansas$state=='Kansas',]$lngdpcapita[90:105]


causal_effect_alasso_se<-exp(post_obs_lngdpcapita)-exp(post_se_lngdpcapita_series)
#1 step effect -1780.873
#4 step mean effect
mean(causal_effect_alasso_se[1:4])


#计算 causal effect LST
causal_effect_alasso_LST<-exp(post_obs_lngdpcapita)-exp(post_LST_lngdpcapita_series)
#1 step effect -1780.873
#4 step mean effect
mean(causal_effect_alasso_LST[1:4])

#计算causal effect no penalty se
causal_effect_alasso_LSE_se<-exp(post_obs_lngdpcapita)-exp(post_LSE_se_lngdpcapita_series)
#1 step effect -1985.278
#4 step mean effect
mean(causal_effect_alasso_LSE_se[1:4])





#原始版本，现在写成函数update_pred_dataframe了
# #修改pred_data_mod 中的ysl以及kansas的ylag
# for(s in state){
#   temp_df<-get(paste(s,'_pred_data', sep=''))
#   if(s=='Kansas'){
#     for(i in c(2:6)){
#       temp_df[i,paste('y_lag', i-1, sep = '')]<-kansas_pred_step1
#     }
#   } else{
#     temp_mod_ysl_step1<-temp_df[1,'ysl']+(kansas_pred_step1-Kansas_pred_data[1,'y'])*W[16,s]
#     temp_df[1,'ysl']<-temp_mod_ysl_step1
#     for(j in c(2:6)){
#       temp_df[j,paste('ysl_lag',j-1, sep='')]<-temp_mod_ysl_step1
#     }
#   }
#   assign(paste(s,'_pred_data_mod', sep=''),temp_df)
# }

