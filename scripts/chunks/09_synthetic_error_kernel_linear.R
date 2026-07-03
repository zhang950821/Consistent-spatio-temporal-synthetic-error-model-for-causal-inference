# Source chunk: synthetic error: kernel--linear
# Original Rmd lines: 481-538


#先用线性模型试一下
var(Kansas_residual)
linear_residual_model<-lm(Kansas_residual~., data=as.data.frame(residual_matrix))
var(residuals(linear_residual_model))
#将control units 的residual整理到一起
control_group_residual<-NULL
for(s in state){
  if(s!="Kansas"){
    control_group_residual<-cbind(control_group_residual,get(paste(s,"_residual",sep="")))
  }
}
colnames(control_group_residual)<-state[-16]
#使用lasso来拟合residual
lambdas<-seq(0,1, length.out=1000)
lasso_residual_model<-cv.glmnet(as.matrix(control_group_residual), as.matrix(Kansas_residual),  alpha=1, lambda = lambdas, nfolds=5)
lasso_residual_model$cvm
plot(lasso_residual_model)
plot(lasso_residual_model$glmnet.fit, "lambda", label = T)
best_lambda<-lasso_residual_model$lambda.min

best_lasso_residual_model<-glmnet(as.matrix(control_group_residual), as.matrix(Kansas_residual),  alpha=1, lambda =best_lambda )
coef(best_lasso_residual_model)

lasso_pred<-predict(best_lasso_residual_model, s=best_lambda, newx=as.matrix(control_group_residual))
lasso_model_residual<-as.matrix(Kansas_residual)-lasso_pred
#在训练集上的MSE是
MSE_training_synthetic_error_model<-sum(lasso_model_residual^2)/70
#-------------------以上是模型的训练过程以及在训练集中模型的表现---------------
#-------------------下面要看模型在验证集中的表现如何---------------------------
validation_size<-12
#首先对所有的地区验证集中的数据建模，并计算residual
for(s in state){
  temp_model1<-get(paste(s,'_model',sep=""))
  temp_validatex<-get(paste(s,"_validation_data", sep=""))
  temp_predict<-predict(temp_model1, newdata=temp_validatex)#这里预测出来应该是12个值啊，为什么是70个值？
  temp_predict_residual<-get(paste(s,"_validation_data", sep=""))$y-temp_predict
  assign(paste(s,"_validation_residual", sep=""), temp_predict_residual)
}
control_group_residual_validation<-NULL
for(s in state){
  if(s!="Kansas"){
    control_group_residual_validation<-cbind(control_group_residual_validation,get(paste(s,"_validation_residual",sep="")))
  }
}

colnames(control_group_residual_validation)<-state[-16]
#synthetic residual
synthetic_kansas_residual_validation<-predict(best_lasso_residual_model, s=best_lambda, newx=as.matrix(control_group_residual_validation))
#接下来要算kansas synthetic error之后的residual
predicted_Kansas_validation_stage1<-predict(Kansas_model,newdata=Kansas_validation_data)
predicted_Kansas_validation_stage2<-predicted_Kansas_validation_stage1+synthetic_kansas_residual_validation
#validation_MSE
MSE_validation<-sum((Kansas_validation_data$y-predicted_Kansas_validation_stage2)^2)/validation_size
#在验证集的MSE是22.50849
#接下来可以考虑一下在synthetic error 的时候使用非参数带penalization的情形
