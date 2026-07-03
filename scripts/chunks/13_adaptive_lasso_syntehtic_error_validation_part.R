# Source chunk: adaptive lasso syntehtic error--validation part
# Original Rmd lines: 661-765

#先做least square
set.seed(150)
SE_LM<-lm(Kansas~. , data = as.data.frame(residual_matrix))
lm_coef<-coef(SE_LM)[-1]
alasso_SE_model<-glmnet(x=as.matrix(residual_matrix[,-16]), y=as.matrix(residual_matrix[,16]), alpha=1, penalty.factor = 1/abs(lm_coef))

plot(alasso_SE_model, xvar="lambda")

alasso_SE_model_cv<-cv.glmnet(x=as.matrix(residual_matrix[,-16]), y=as.matrix(residual_matrix[,16]), type.measure = "mse", nfold=10,alpha=1, penalty.factor = 1/abs(lm_coef), keep=TRUE)

plot(alasso_SE_model_cv)

best_lambda<-alasso_SE_model_cv$lambda.min

best_alasso_coef<-coef(alasso_SE_model_cv, s=best_lambda)

best_alasso_coef

#查看训练集误差MSE
alasso_predictors_train<-predict(alasso_SE_model, s=best_lambda, newx = as.matrix(residual_matrix[,-16]))

alasso_MSE_train<-mean((residual_matrix[,16]-alasso_predictors_train)^2)

alasso_MSE_train#0.1909(seed=105)

#接下来在验证集误差MSE
#这个是没标准化的residual_valid_matrix
residual_matrix_valid<-read.csv('data/total_matrix/diff_using_60train_data/pred_residual_matrix.csv')[,-1]
write.csv(valid_residual_matrix, file='data/total_matrix/diff_using_60train_data/valid_residual_matrix.csv')
#这个是上面直接得出的valid_residual_matrix
#residual_matrix_valid<-valid_residual_matrix

control_valid_residual<-valid_residual_matrix[, -16]

alasso_predictors_valid<-predict(alasso_SE_model, s=best_lambda, newx = as.matrix(control_valid_residual))

diff<-alasso_predictors_valid-valid_residual_matrix[,16]


alasso_MSE_valid<-mean(diff^2)
                                 
alasso_MSE_valid#有时候1.448， 有时候0.885,都是seed=150 这个问题解决了，因为residual_matrix_valid有点问题，用valid_residual matrix就对了，就是0.88

#LST的MSE_valid
mean((valid_residual_matrix[,16])^2)

#set.seed(101)表现很好
alasso_MSE_train
alasso_MSE_valid
                    
#--------------------写个循环看哪个seed好--

MSE_seed_tab1<-data.frame(matrix(ncol = 3, nrow = 0))
colnames(MSE_seed_tab1)<-c('seed', 'training_MSE', 'valid_MSE')
temp_MSEs<-rep(0,3)


for(seedindx in c(100:150)){
  #先做least square
set.seed(seedindx)
SE_LM<-lm(Kansas~. , data = as.data.frame(residual_matrix))
lm_coef<-coef(SE_LM)[-1]
alasso_SE_model<-glmnet(x=as.matrix(residual_matrix[,-16]), y=as.matrix(residual_matrix[,16]), alpha=1, penalty.factor = 1/abs(lm_coef))

plot(alasso_SE_model, xvar="lambda")

alasso_SE_model_cv<-cv.glmnet(x=as.matrix(residual_matrix[,-16]), y=as.matrix(residual_matrix[,16]), type.measure = "mse", nfold=10,alpha=1, penalty.factor = 1/abs(lm_coef), keep=TRUE)

plot(alasso_SE_model_cv)

best_lambda<-alasso_SE_model_cv$lambda.min

best_alasso_coef<-coef(alasso_SE_model_cv, s=best_lambda)

best_alasso_coef

#查看训练集误差MSE
alasso_predictors_train<-predict(alasso_SE_model, s=best_lambda, newx = as.matrix(residual_matrix[,-16]))

alasso_MSE_train<-mean((residual_matrix[,16]-alasso_predictors_train)^2)

alasso_MSE_train#0.1909(seed=105)


#接下来在验证集误差MSE
#这个是没标准化的residual_valid_matrix
residual_matrix_valid<-read.csv('data/total_matrix/diff_using_60train_data/pred_residual_matrix.csv')[,-1]
#这个是上面直接得出的valid_residual_matrix
#residual_matrix_valid<-valid_residual_matrix
control_valid_residual<-valid_residual_matrix[, -16]

alasso_predictors_valid<-predict(alasso_SE_model, s=best_lambda, newx = as.matrix(control_valid_residual))

diff<-alasso_predictors_valid-valid_residual_matrix[,16]


alasso_MSE_valid<-mean(diff^2)
                                 
alasso_MSE_valid#0.8850447
temp_MSEs<-c(seedindx, alasso_MSE_train, alasso_MSE_valid)
MSE_seed_tab1<-rbind(MSE_seed_tab1, temp_MSEs)
}
#write.csv(MSE_seed_tab1, file='data/empirical_training_valid_MSE_seed_table.csv')
