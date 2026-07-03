# Source chunk: lasso synthetic error
# Original Rmd lines: 621-659

set.seed(150)
lasso_SE_model<-glmnet(x=as.matrix(residual_matrix[,-16]), y=as.matrix(residual_matrix[,16]), alpha=1)

plot(lasso_SE_model, xvar="lambda")

lasso_SE_model_cv<-cv.glmnet(x=as.matrix(residual_matrix[,-16]), y=as.matrix(residual_matrix[,16]), type.measure = "mse", nfold=10,alpha=1, keep=TRUE)

plot(lasso_SE_model_cv)

best_lambda_lasso<-lasso_SE_model_cv$lambda.min

best_lasso_coef<-coef(lasso_SE_model_cv, s=best_lambda_lasso)

best_lasso_coef

#查看训练集误差MSE
lasso_predictors_train<-predict(lasso_SE_model, s=best_lambda_lasso, newx = as.matrix(residual_matrix[,-16]))

lasso_MSE_train<-mean((residual_matrix[,16]-lasso_predictors_train)^2)

lasso_MSE_train#0.05388

#接下来在验证集误差MSE
#这个是没标准化的residual_valid_matrix
#residual_matrix_valid<-read.csv('data/total_matrix/diff_using_60train_data/pred_residual_matrix.csv')[,-1]
#这个是上面直接得出的valid_residual_matrix
residual_matrix_valid<-valid_residual_matrix
control_valid_residual<-residual_matrix_valid[, -16]

lasso_predictors_valid<-predict(lasso_SE_model, s=best_lambda_lasso, newx = as.matrix(control_valid_residual))

lasso_MSE_valid<-mean((residual_matrix_valid[,16]-lasso_predictors_valid)^2)
         

lasso_MSE_train                        
lasso_MSE_valid#1.94907

