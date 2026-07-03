# Source chunk: spatial smooth skip-step-model empirical experiment
# Original Rmd lines: 1966-2431

skip_lag_order<-5

state<-state_all
dyfast_spatial_est<-function(units,y, p, q, S, s, h2, parameters){
  N=length(units)
  T0=nrow(units[[1]])

  #这个Y也许可以拿出来，在kfold之后用
  Y<-NULL
  for(i in c(1:N)){
    Y<-matrix(c(Y,as.numeric(units[[i]][,y])), nrow=1)
  }

  Y<-t(Y)
  param_length<-length(parameters)
  
  e1<-kronecker(c(1, 0, 0), diag(param_length+1))
  
  Wtuta_elements<-NULL
  for(i in c(1:N)){
    temp_L_kernel<-L_2d.gaussian((matrix(as.numeric(S[i,]), ncol=1)-s)/h2)/h2^2
    rep_temp_L_kernel<-rep(temp_L_kernel, T0)
    Wtuta_elements<-c(Wtuta_elements,rep_temp_L_kernel)
  }
  Wtuta<-diag(Wtuta_elements)
  
  
  #browser()
  Ztuta<-NULL
  for(i in c(1:N)){
    for(t in c(1:T0)){
      Zt_temp<-c(1,matrix(as.numeric(units[[i]][t,parameters]), nrow=1))
      Ztuta<-rbind(Ztuta, c(Zt_temp, kronecker((as.matrix((matrix(as.numeric(S[i,]), ncol=1)-s)/h2)), Zt_temp)))
    }
  }
  
  U=t(Ztuta)%*%Wtuta%*%Ztuta/(N*T0)
  V=t(Ztuta)%*%Wtuta%*%Y/(N*T0)

  #browser()
  U_inv <- solve(U)
  beta_hat<-t(e1)%*% U_inv%*%V#这里对于e1的kronecker product有疑问

  return(beta_hat)
}
covariates<-c('emplvlcapita_dif', 'popestimate_dif', 'qtrly_estabs_count_dif')#"emplvlcapita_dif", 'totalwagescapita_dif', 'popestimate_dif', 'qtrly_estabs_count_dif'

states_list<-list()
for(i in c(1:50)){
  states_list[[i]]<-get(paste0(state[i], '_training_data'))
}

# spatial_dyfast_Kansas_param<-dyfast_spatial_est(units=states_list,y='y', p=5, q=5,S=S, s=S[16,], h2=7, parameters=c('y_lag1','y_lag2','y_lag3','y_lag4','y_lag5', 'ysl_lag1', 'ysl_lag2','ysl_lag3','ysl_lag4','ysl_lag5'))

# for(test_state in states_list){
#   print(is.data.frame(test_state))
# }

fit_spatial_model_with_lags <- function(data_list, y_var, y_lag_range, ysl_lag_range, s_idx, covariates=NULL) {
  # data: 原始数据
  # y_var: 因变量列名
  # y_lag_range, ysl_lag_range: 各自的 lag 范围，例如 1:5
  
  df_list <- data_list
  
  for(i in seq_along(df_list)){
    df <- df_list[[i]]
  
  # 动态生成 y_lag
  for (k in y_lag_range) {
    if(k<=5){
      next
    }else{
      df[[paste0("y_lag", k)]] <- dplyr::lag(df[[y_var]], k)
    }
  }
  
  # 动态生成 ysl_lag
  for (k in ysl_lag_range) {
        if(k<=5){
      next
    }else{
    df[[paste0("ysl_lag", k)]] <- dplyr::lag(df[["ysl"]], k)
    }
  }
  
  df_list[[i]] <- na.omit(df)
  }
  #拟合公式字符串

  temp_lag_parameters<-c(
      paste0("y_lag", y_lag_range),
      paste0("ysl_lag", ysl_lag_range)
    )
  temp_parameters<-c(temp_lag_parameters, covariates)

  # spatial-modelling
dyfast_Kansas_betas<-dyfast_spatial_est(units=df_list,y='y', p=length(y_lag_range), q=length(ysl_lag_range),S=S, s=S[s_idx,], h2=7, parameters=temp_parameters)

  return(dyfast_Kansas_betas)
}
hawaii_idx<-which(state=='Florida')
for(s_idx in c(1:(hawaii_idx-1), (hawaii_idx+1):50)){
    for(forward_step in c(1:12)){
    temp_spatial_beta<-fit_spatial_model_with_lags(data_list=states_list, y_var='y', y_lag_range = ((forward_step):(forward_step+skip_lag_order-1)), ((forward_step):(forward_step+skip_lag_order-1)), s_idx=s_idx, covariates=covariates)
    assign(paste0(state[s_idx],'_forwardstep_',forward_step,'_spatial_beta'), temp_spatial_beta)
    print(paste('s_idx:', s_idx, "forward_step:", forward_step))
  }
}


#spatio-skip model predict-------------------
#这一步好像加了covariates之后效果反而不好，还是因为数据长度原因，所以可能考虑和去掉covariates的multi-step forward 对比
#我现在要加一个emelvlcapita
set.seed(109)
#先把temp_parameters设置好

state_all<-state
state<-state[-hawaii_idx]
  
for(forward_step in c(1:12)){
  #spatial linear predict
  for(s in state){
    #validation data要有足够的lag，可能得用pre_treat_data来弄一下
    temp_pre_treat_data<-get(paste0(s, '_pre_treat_data'))
    for(j in c(6:(skip_lag_order+forward_step))){
      temp_pre_treat_data[,paste0('y_lag', j)]<-dplyr::lag(temp_pre_treat_data[['y_lag5']], (j-5))
      temp_pre_treat_data[,paste0('ysl_lag', j)]<-dplyr::lag(temp_pre_treat_data[['ysl_lag5']], (j-5))
    }
    # for(j in c(1:forward_step)){
    #   temp_pre_treat_data[,paste0('y_lag', j+skip_lag_order-1)]<-dplyr::lag(temp_pre_treat_data[['y_lag5']], j)
    #   temp_pre_treat_data[,paste0('ysl_lag', j+skip_lag_order-1)]<-dplyr::lag(temp_pre_treat_data[['ysl_lag5']], j)
    # }
    temp_validation_data<-temp_pre_treat_data[72:83,]
    assign(paste0(s, '_skip_validation_data_step', forward_step),temp_validation_data)
    

    temp_spatial_beta<-get(paste0(s,'_forwardstep_',forward_step,'_spatial_beta'))
    
    temp_lag_parameters<-c(
      paste0("y_lag", ((forward_step):(forward_step+skip_lag_order-1))),
      paste0("ysl_lag", ((forward_step):(forward_step+skip_lag_order-1)))
    )
    
    temp_parameters<-c(temp_lag_parameters, covariates)
    
    temp_spatial_step1_pred<-temp_spatial_beta[1]+as.matrix(temp_validation_data[forward_step,temp_parameters ])%*%as.matrix(temp_spatial_beta[-1,])

    
    assign(paste0(s,'_forwardstep',forward_step, '_spatial_step1_pred'), temp_spatial_step1_pred)
  }
  #extract residuals build synthetic weights and synthetic residuals
  temp_skip_residual_matrix<-NULL
  for(s in state){
      temp_pre_treat_data<-get(paste0(s, '_pre_treat_data'))
    for(j in c(6:(skip_lag_order+forward_step))){
      temp_pre_treat_data[,paste0('y_lag', j)]<-dplyr::lag(temp_pre_treat_data[['y_lag5']], (j-5))
      temp_pre_treat_data[,paste0('ysl_lag', j)]<-dplyr::lag(temp_pre_treat_data[['ysl_lag5']], (j-5))
    }
    temp_training_data<-temp_pre_treat_data[1:71,]
    #browser()
    #变系数模型的training residual不好搞
    temp_spatial_beta<-get(paste0(s,'_forwardstep_',forward_step,'_spatial_beta'))
    temp_lag_parameters<-c(
      paste0("y_lag", ((forward_step):(forward_step+skip_lag_order-1))),
      paste0("ysl_lag", ((forward_step):(forward_step+skip_lag_order-1)))
    )
    temp_parameters<-c(temp_lag_parameters, covariates)
    
    temp_est_training_data<-temp_spatial_beta[1]+as.matrix(temp_training_data[,temp_parameters ])%*%as.matrix(temp_spatial_beta[-1,])
    temp_est_training_data<-na.omit(temp_est_training_data)#bug2
    temp_training_residuals<-temp_training_data[(forward_step:71),'y']-temp_est_training_data
    
    temp_skip_residual_matrix<-cbind(temp_skip_residual_matrix,temp_training_residuals)
  }
  colnames(temp_skip_residual_matrix)<-state
  assign(paste0('spatio_skip_residual_matrix_step',forward_step), temp_skip_residual_matrix)
  mean((temp_skip_residual_matrix[,'Kansas'])^2)
  #对所有forward_step对应的residual_matrix建立synthetic error model
  #***这里还是存在参数量大但是数据量小的问题，先试一下，不行就用
  # SE_ridge_skip<-glmnet(x=as.matrix(temp_skip_residual_matrix[,-16]), y=as.matrix(temp_skip_residual_matrix[,16]), alpha=0)
  # SE_ridge_skip_cv<-cv.glmnet(x=as.matrix(temp_skip_residual_matrix[,-16]), y=as.matrix(temp_skip_residual_matrix[,16]),type.measure="mse", nfold=10, alpha=0)
  # best_ridge_lambda<-SE_ridge_skip_cv$lambda.min
  # lm_coef_skip<-coef(SE_ridge_skip, s=best_ridge_lambda)
  # lm_coef_skip<-lm_coef_skip[-1,]
  
  
  SE_LM_skip<-lm(Kansas~. , data = as.data.frame(temp_skip_residual_matrix))
  lm_coef_skip<-coef(SE_LM_skip)[-1]
  
temp_alasso_skip_SE_model<-glmnet(x=as.matrix(temp_skip_residual_matrix[,colnames(temp_skip_residual_matrix)!='Kansas']), y=as.matrix(temp_skip_residual_matrix[,colnames(temp_skip_residual_matrix)=='Kansas']), alpha=1, penalty.factor = 1/abs(lm_coef_skip))

temp_alasso_skip_SE_model_cv<-cv.glmnet(x=as.matrix(temp_skip_residual_matrix[,colnames(temp_skip_residual_matrix)!='Kansas']), y=as.matrix(temp_skip_residual_matrix[,colnames(temp_skip_residual_matrix)=='Kansas']),type.measure="mse", nfold=10, alpha=1, penalty.factor = 1/abs(lm_coef_skip), keep=TRUE)

plot(temp_alasso_skip_SE_model_cv)
#browser()
temp_best_lambda_skip<-temp_alasso_skip_SE_model_cv$lambda.min
coef(temp_alasso_skip_SE_model, s=temp_best_lambda_skip)

#计算当前forwardstep的预测误差
# for(s in state){
#   get(paste0(s,'_step',forward_step, '_LM_pred'))
# }
#browser()
skip_validation_errors<-NULL
for(s in state){
  temp_validation_data<-get(paste0(s, '_skip_validation_data_step', forward_step))
  temp_spatial_step1_pred<-get(paste0(s,'_forwardstep',forward_step, '_spatial_step1_pred'))
  temp_error<-temp_validation_data[forward_step,'y']-temp_spatial_step1_pred
  skip_validation_errors<-cbind(skip_validation_errors, temp_error)
}
colnames(skip_validation_errors)<-state


temp_skip_SE_error<-predict(temp_alasso_skip_SE_model_cv, s=temp_best_lambda_skip, newx = skip_validation_errors[,colnames(skip_validation_errors)!='Kansas'])

temp_Kansas_spatial_step1_pred<-get(paste0('Kansas_forwardstep',forward_step, '_spatial_step1_pred'))
temp_Kansas_spatial_skip_STPSEM_pred<-temp_Kansas_spatial_step1_pred+temp_skip_SE_error
assign(paste0('Kansas_spatial_skip_forwardstep', forward_step, '_pred'), temp_Kansas_spatial_skip_STPSEM_pred)
}

Kansas_spatial_skip_validation_error<-NULL
for(i in c(1:12)){
  temp_Kansas_validation_error<-Kansas_validation_data[i,'y']-get(paste0('Kansas_spatial_skip_forwardstep', i, '_pred'))
  Kansas_spatial_skip_validation_error<-c(Kansas_spatial_skip_validation_error,temp_Kansas_validation_error)
}
spatial_skip_validation_MSE<-mean(Kansas_spatial_skip_validation_error^2)
spatial_skip_validation_MSE


#写一个循环-------------------------------------------
spatial_skip_MSE_table<-NULL
spatial_skip_MSEs<-NULL
for(seed_idx in c(100:120)){
  set.seed(seed_idx)
for(forward_step in c(1:12)){
  #spatial linear predict
  for(s in state){
    temp_pre_treat_data<-get(paste0(s, '_pre_treat_data'))
    for(j in c(6:(skip_lag_order+forward_step))){
      temp_pre_treat_data[,paste0('y_lag', j)]<-dplyr::lag(temp_pre_treat_data[['y_lag5']], (j-5))
      temp_pre_treat_data[,paste0('ysl_lag', j)]<-dplyr::lag(temp_pre_treat_data[['ysl_lag5']], (j-5))
    }
    # for(j in c(1:forward_step)){
    #   temp_pre_treat_data[,paste0('y_lag', j+skip_lag_order-1)]<-dplyr::lag(temp_pre_treat_data[['y_lag5']], j)
    #   temp_pre_treat_data[,paste0('ysl_lag', j+skip_lag_order-1)]<-dplyr::lag(temp_pre_treat_data[['ysl_lag5']], j)
    # }
    temp_validation_data<-temp_pre_treat_data[72:83,]
    assign(paste0(s, '_skip_validation_data_step', forward_step),temp_validation_data)
    

    temp_spatial_beta<-get(paste0(s,'_forwardstep_',forward_step,'_spatial_beta'))
    
    temp_lag_parameters<-c(
      paste0("y_lag", ((forward_step):(forward_step+skip_lag_order-1))),
      paste0("ysl_lag", ((forward_step):(forward_step+skip_lag_order-1)))#emplvl记号
    )
    temp_parameters<-c(temp_lag_parameters, covariates)
    
    temp_spatial_step1_pred<-temp_spatial_beta[1]+as.matrix(temp_validation_data[forward_step,temp_parameters ])%*%as.matrix(temp_spatial_beta[-1,])

    
    assign(paste0(s,'_forwardstep',forward_step, '_spatial_step1_pred'), temp_spatial_step1_pred)
  }
  #extract residuals build synthetic weights and synthetic residuals
  temp_skip_residual_matrix<-NULL
  for(s in state){
      temp_pre_treat_data<-get(paste0(s, '_pre_treat_data'))
    for(j in c(6:(skip_lag_order+forward_step))){
      temp_pre_treat_data[,paste0('y_lag', j)]<-dplyr::lag(temp_pre_treat_data[['y_lag5']], (j-5))
      temp_pre_treat_data[,paste0('ysl_lag', j)]<-dplyr::lag(temp_pre_treat_data[['ysl_lag5']], (j-5))
    }
    temp_training_data<-temp_pre_treat_data[1:71,]
    #browser()
    #变系数模型的training residual不好搞
    temp_spatial_beta<-get(paste0(s,'_forwardstep_',forward_step,'_spatial_beta'))
    temp_lag_parameters<-c(
      paste0("y_lag", ((forward_step):(forward_step+skip_lag_order-1))),
      paste0("ysl_lag", ((forward_step):(forward_step+skip_lag_order-1)))
    )
    
    temp_parameters<-c(temp_lag_parameters, covariates)
    
    temp_est_training_data<-temp_spatial_beta[1]+as.matrix(temp_training_data[,temp_parameters ])%*%as.matrix(temp_spatial_beta[-1,])
    temp_est_training_data<-na.omit(temp_est_training_data)
    temp_training_residuals<-temp_training_data[(forward_step:71),'y']-temp_est_training_data
    
    temp_skip_residual_matrix<-cbind(temp_skip_residual_matrix,temp_training_residuals)
  }
  colnames(temp_skip_residual_matrix)<-state
  assign(paste0('spatio_skip_residual_matrix_step',forward_step), temp_skip_residual_matrix)
  mean((temp_skip_residual_matrix[,'Kansas'])^2)
  #对所有forward_step对应的residual_matrix建立synthetic error model
  #***这里还是存在参数量大但是数据量小的问题，先试一下，不行就用
  # SE_ridge_skip<-glmnet(x=as.matrix(temp_skip_residual_matrix[,-16]), y=as.matrix(temp_skip_residual_matrix[,16]), alpha=0)
  # SE_ridge_skip_cv<-cv.glmnet(x=as.matrix(temp_skip_residual_matrix[,-16]), y=as.matrix(temp_skip_residual_matrix[,16]),type.measure="mse", nfold=10, alpha=0)
  # best_ridge_lambda<-SE_ridge_skip_cv$lambda.min
  # lm_coef_skip<-coef(SE_ridge_skip, s=best_ridge_lambda)
  # lm_coef_skip<-lm_coef_skip[-1,]
  
  
  SE_LM_skip<-lm(Kansas~. , data = as.data.frame(temp_skip_residual_matrix))
  lm_coef_skip<-coef(SE_LM_skip)[-1]
  
temp_alasso_skip_SE_model<-glmnet(x=as.matrix(temp_skip_residual_matrix[,colnames(temp_skip_residual_matrix)!='Kansas']), y=as.matrix(temp_skip_residual_matrix[,colnames(temp_skip_residual_matrix)=='Kansas']), alpha=1, penalty.factor = 1/abs(lm_coef_skip))

temp_alasso_skip_SE_model_cv<-cv.glmnet(x=as.matrix(temp_skip_residual_matrix[,colnames(temp_skip_residual_matrix)!='Kansas']), y=as.matrix(temp_skip_residual_matrix[,colnames(temp_skip_residual_matrix)=='Kansas']),type.measure="mse", nfold=10, alpha=1, penalty.factor = 1/abs(lm_coef_skip), keep=TRUE)

plot(temp_alasso_skip_SE_model_cv)
#browser()
temp_best_lambda_skip<-temp_alasso_skip_SE_model_cv$lambda.min
coef(temp_alasso_skip_SE_model, s=temp_best_lambda_skip)

#计算当前forwardstep的预测误差
# for(s in state){
#   get(paste0(s,'_step',forward_step, '_LM_pred'))
# }
#browser()
skip_validation_errors<-NULL
for(s in state){
  temp_validation_data<-get(paste0(s, '_skip_validation_data_step', forward_step))
  temp_spatial_step1_pred<-get(paste0(s,'_forwardstep',forward_step, '_spatial_step1_pred'))
  temp_error<-temp_validation_data[forward_step,'y']-temp_spatial_step1_pred
  skip_validation_errors<-cbind(skip_validation_errors, temp_error)
}
colnames(skip_validation_errors)<-state


temp_skip_SE_error<-predict(temp_alasso_skip_SE_model_cv, s=temp_best_lambda_skip, newx = skip_validation_errors[,colnames(skip_validation_errors)!='Kansas'])

temp_Kansas_spatial_step1_pred<-get(paste0('Kansas_forwardstep',forward_step, '_spatial_step1_pred'))
temp_Kansas_spatial_skip_STPSEM_pred<-temp_Kansas_spatial_step1_pred+temp_skip_SE_error
assign(paste0('Kansas_spatial_skip_forwardstep', forward_step, '_pred'), temp_Kansas_spatial_skip_STPSEM_pred)
}

Kansas_spatial_skip_validation_error<-NULL
for(i in c(1:12)){
  temp_Kansas_validation_error<-Kansas_validation_data[i,'y']-get(paste0('Kansas_spatial_skip_forwardstep', i, '_pred'))
  Kansas_spatial_skip_validation_error<-c(Kansas_spatial_skip_validation_error,temp_Kansas_validation_error)
}
temp_spatial_skip_validation_MSE_row<-NULL
for(ps in c(1:11)){
  spatial_skip_validation_MSE<-mean(Kansas_spatial_skip_validation_error[ps:11]^2)
  temp_spatial_skip_validation_MSE_row<-cbind(temp_spatial_skip_validation_MSE_row, spatial_skip_validation_MSE)
}
spatial_skip_MSE_table<-rbind(spatial_skip_MSE_table, temp_spatial_skip_validation_MSE_row)


colMeans(MSE_table)
ts.plot(colMeans(MSE_table))

colMeans(spatial_skip_MSE_table)
ts.plot(colMeans(spatial_skip_MSE_table))


ts.plot(ts(colMeans(MSE_table)[1:11]),ts(colMeans(spatial_skip_MSE_table)[1:11]), col=c('black', 'red'))


 spatial_skip_validation_MSE<-mean(Kansas_spatial_skip_validation_error[1:11]^2)
 print(paste('seed=', seed_idx, 'MSE=', spatial_skip_validation_MSE))
 spatial_skip_MSEs<-rbind(spatial_skip_MSEs, spatial_skip_validation_MSE)
 

}



#画图-------------------------------------
# 数据
mse_mf   <- colMeans(MSE_table)[1:11]
mse_skip <- colMeans(spatial_skip_MSE_table)[1:11]

horizon <- 1:11

# 画图
plot(
  horizon, mse_mf,
  type = "l",
  lwd  = 2.5,
  lty  = 1,                 # 实线
  col  = "black",
  xlab = "Validation Start Time",
  ylab = "Validation Mean Squared Error (MSE)",
  ylim = range(c(mse_mf, mse_skip)),
  bty  = "l"                # 只保留左、下边框（论文常用）
)

lines(
  horizon, mse_skip,
  lwd = 2.5,
  lty = 2,                  # 虚线
  col = "red"
)

legend(
  "topleft",
  legend = c(
    "Direct Multi-step Forward",
    "Spatial-smooth Skip-step"
  ),
  col  = c("black", "red"),
  lty  = c(1, 2),
  lwd  = 2.5,
  bty  = "n",
  cex  = 0.9
)
#带置信区间的图
# 假设：MSE_table 和 spatial_skip_MSE_table 的列对应 step/起点
# 行对应 seed（或重复实验）
idx <- 1:11
horizon <- idx

mf_mat   <- as.matrix(MSE_table)[, idx, drop = FALSE]
skip_mat <- as.matrix(spatial_skip_MSE_table)[, idx, drop = FALSE]

mf_mean   <- colMeans(mf_mat, na.rm = TRUE)
skip_mean <- colMeans(skip_mat, na.rm = TRUE)

mf_sd   <- apply(mf_mat,   2, sd, na.rm = TRUE)
skip_sd <- apply(skip_mat, 2, sd, na.rm = TRUE)

# 95% CI（正态近似；也可用分位数带）
mf_lo <- mf_mean - 1.96 * mf_sd / sqrt(nrow(mf_mat))
mf_hi <- mf_mean + 1.96 * mf_sd / sqrt(nrow(mf_mat))

sk_lo <- skip_mean - 1.96 * skip_sd / sqrt(nrow(skip_mat))
sk_hi <- skip_mean + 1.96 * skip_sd / sqrt(nrow(skip_mat))

ylim <- range(c(mf_lo, mf_hi, sk_lo, sk_hi), na.rm = TRUE)

plot(horizon, mf_mean, type="n",
     xlab="Validation Start Time", ylab="Validation Mean Squared Error (MSE)",
     ylim=ylim, bty="l")

# 误差带（半透明多边形）
polygon(c(horizon, rev(horizon)), c(mf_lo, rev(mf_hi)),
        border = NA, col = adjustcolor("black", alpha.f = 0.15))
polygon(c(horizon, rev(horizon)), c(sk_lo, rev(sk_hi)),
        border = NA, col = adjustcolor("red", alpha.f = 0.15))

# 均值线 + 点
lines(horizon, mf_mean, col="black", lwd=2.5, lty=1)
points(horizon, mf_mean, col="black", pch=16, cex=0.8)

lines(horizon, skip_mean, col="red", lwd=2.5, lty=2)
points(horizon, skip_mean, col="red", pch=17, cex=0.9)

grid(nx = NA, ny = NULL, lty = 3, col = "grey85")

legend("topleft",
       legend = c("Recursive Multi-step-forward STPSEM (mean ± 95% CI)",
                  "Spatial-smooth Skip-step-forward STPSEM (mean ± 95% CI)"),
       col = c("black", "red"),
       lty = c(1, 2),
       pch = c(16, 17),
       lwd = 2.5,
       bty = "n",
       cex = 0.9)



mean(spatial_skip_MSEs[, 1])
var(spatial_skip_MSEs[, 1])

