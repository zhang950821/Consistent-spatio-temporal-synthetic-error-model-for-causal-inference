# Source chunk: Robust synthetic control
# Original Rmd lines: 1299-1491

#validation-------------------------
#step1:
X1_training<-Kansas_pre_treat_data[1:71,4]
X1_validation<-Kansas_pre_treat_data[72:83,4]
X0_training<-NULL
X0_validation<-NULL
for(s in control_states){
  temp_pre_treat_data<-get(paste0(s,"_pre_treat_data"))
  X0_training<-cbind(X0_training,temp_pre_treat_data[1:71,4])
  X0_validation<-cbind(X0_validation,temp_pre_treat_data[72:83,4])
}

#singular decomposition
X0_training_svd<-svd(X0_training)
d<-X0_training_svd$d
u<-X0_training_svd$u
v<-X0_training_svd$v

#set threshold mu
mu=median(d)

d_thres<-d
for(i in c(1:49)){
  if(d[i]<mu){
    d_thres[i]<-0
  }
}
d_mat<-diag(d)
d_thres_mat<-diag(d_thres)

X0_thres<-u%*%d_thres_mat%*%v

#step2:
robust_SC<-glmnet(x=X0_thres, y=X1_training, alpha = 0)


robust_SC_cv<-cv.glmnet(x=X0_thres, y=X1_training, type.measure = "mse", nfold=10, alpha=0)

plot(robust_SC_cv)

best_lambda<-robust_SC_cv$lambda.min

robust_SC_valid_pred<-predict(robust_SC, s=best_lambda, newx = X0_validation)

robust_SC_train_pred<-predict(robust_SC, s=best_lambda, newx = X0_training)


MSE_valid=mean((robust_SC_valid_pred-X1_validation)^2)

MSE_training=mean((robust_SC_train_pred-X1_training)^2)

for(mu in d){
  d_thres<-d
for(i in c(1:49)){
  if(d[i]<mu){
    d_thres[i]<-0
  }
}
d_mat<-diag(d)
d_thres_mat<-diag(d_thres)

X0_thres<-u%*%d_thres_mat%*%v

#step2:
robust_SC<-glmnet(x=X0_thres, y=X1_training, alpha = 0)


robust_SC_cv<-cv.glmnet(x=X0_thres, y=X1_training, type.measure = "mse", nfold=10, alpha=0)


best_lambda<-robust_SC_cv$lambda.min

robust_SC_valid_pred<-predict(robust_SC, s=best_lambda, newx = X0_validation)

robust_SC_train_pred<-predict(robust_SC, s=best_lambda, newx = X0_training)


MSE_valid=mean((robust_SC_valid_pred-X1_validation)^2)


MSE_training=mean((robust_SC_train_pred-X1_training)^2)

print(paste0('mu=', mu))

print(paste0('MSE_training is: ', MSE_training))
print(paste0('MSE_valid is: ', MSE_valid))
}
#best mu="mu=20.7241840884166"

#实际预测部分--------------------------------------------------
#step1:
X1_training<-Kansas_pre_treat_data[,4]
X1_validation<-Kansas_pred_data[,3]
X0_training<-NULL
X0_validation<-NULL
for(s in control_states){
  temp_pre_treat_data<-get(paste0(s,"_pre_treat_data"))
  temp_post_data<-get(paste0(s,"_pred_data"))
  X0_training<-cbind(X0_training,temp_pre_treat_data[,4])
  X0_validation<-cbind(X0_validation,temp_post_data[,3])
}

#singular decomposition
X0_training_svd<-svd(X0_training)
d<-X0_training_svd$d
u<-X0_training_svd$u
v<-X0_training_svd$v

#set threshold mu
mu=median(d)

for(mu in d){
  d_thres<-d
for(i in c(1:49)){
  if(d[i]<mu){
    d_thres[i]<-0
  }
}
d_mat<-diag(d)
d_thres_mat<-diag(d_thres)

X0_thres<-u%*%d_thres_mat%*%v

#step2:
robust_SC<-glmnet(x=X0_thres, y=X1_training, alpha = 0)


robust_SC_cv<-cv.glmnet(x=X0_thres, y=X1_training, type.measure = "mse", nfold=10, alpha=0)


best_lambda<-robust_SC_cv$lambda.min

robust_SC_valid_pred<-predict(robust_SC, s=best_lambda, newx = X0_validation)

robust_SC_train_pred<-predict(robust_SC, s=best_lambda, newx = X0_training)



MSE_training=mean((robust_SC_train_pred-X1_training)^2)

print(paste0('mu=', mu))

print(paste0('MSE_training is: ', MSE_training))

}
#mu=22.2557135018648的时候training MSE最小
mu=22.2557135018648

for(i in c(1:49)){
  if(d[i]<mu){
    d_thres[i]<-0
  }
}
d_mat<-diag(d)
d_thres_mat<-diag(d_thres)

X0_thres<-u%*%d_thres_mat%*%v

#step2:
robust_SC<-glmnet(x=X0_thres, y=X1_training, alpha = 0)


robust_SC_cv<-cv.glmnet(x=X0_thres, y=X1_training, type.measure = "mse", nfold=10, alpha=0)


best_lambda<-robust_SC_cv$lambda.min

robust_SC_pred<-predict(robust_SC, s=best_lambda, newx = X0_validation)

#将预测的return值转化为gdp per capita
post_RSC_lngdpcapita<-0

last_value<-last_pre_lngdpcapita
for(i in c(1:16)){
  post_RSC_lngdpcapita[i]<-last_value+robust_SC_pred[i]/100
  last_value<-post_RSC_lngdpcapita[i]
}



post_RSC_lngdpcapita<-ts(post_RSC_lngdpcapita,start = 90)
#1 step and 4 step average effect
one_step_effect<-exp(kansas[1665,'lngdpcapita'])-exp(post_RSC_lngdpcapita[1])
four_step_effect<-0
for(i in c(1:4)){
  temp_step_effect<-exp(kansas[(1664+i),'lngdpcapita'])-exp(post_RSC_lngdpcapita[i])
  four_step_effect<-four_step_effect+temp_step_effect
}
four_step_ave_effect<-four_step_effect/4


