# Source chunk: functions
# Original Rmd lines: 67-320

#计算距离的函数


distance<-function(u1,u2){
  distance=sqrt((u1$latitude-u2$latitude)^2+(u1$longitude-u2$longitude)^2)
  return(distance)
}


#生成distance matrix函数
distance_matrix_func<-function(m){
  matrix<-matrix(NA,50,50)
  for(i in c(1:50)){
    for(j in c(1:50)){
      matrix[i,j]<-distance(state_dataframe[i,],state_dataframe[j,])
    }
  }
  return(matrix)
}
#写一个更general的update_dataframe
#修改pred_data_mod 中的ysl以及kansas的ylag
update_pred_dataframe1<-function(kansas_pred_last_step, y_lag_order=5, ysl_lag_order=5, current_row=72){
for(s in state){
  temp_df<-get(paste(s,'_pre_treat_data_mod', sep=''))
  if(s=='Kansas'){
    temp_df[current_row, 'y']<-kansas_pred_last_step
    for(i in c(1:y_lag_order)){
      if(current_row+i>nrow(temp_df)) break
        
      temp_df[current_row+i,paste('y_lag', i, sep = '')]<-kansas_pred_last_step
    }
  } else{
    temp_mod_ysl_currentstep<-temp_df[current_row,'ysl']+(kansas_pred_last_step-Kansas_pre_treat_data[current_row,'y'])*W[16,s]
    temp_df[current_row,'ysl']<-temp_mod_ysl_currentstep
    for(j in c(1:ysl_lag_order)){
      if(current_row+j>nrow(temp_df)) break
      
      temp_df[current_row+j,paste('ysl_lag',j, sep='')]<-temp_mod_ysl_currentstep
    }
  }
  assign(paste(s,'_pre_treat_data_mod', sep=''),temp_df, envir = .GlobalEnv)
}
  return(temp_df)
}


#修改pred_data_mod 中的ysl以及kansas的ylag
update_pred_dataframe<-function(kansas_pred_last_step, step){
for(s in state){
  temp_df<-get(paste(s,'_pred_data_mod', sep=''))
  if(s=='Kansas'){
    for(i in c(step:(step+4))){
      temp_df[i,paste('y_lag', i-1, sep = '')]<-kansas_pred_last_step
    }
  } else{
    temp_mod_ysl_currentstep<-temp_df[step-1,'ysl']+(kansas_pred_last_step-Kansas_pred_data[1,'y'])*W[16,s]
    temp_df[step-1,'ysl']<-temp_mod_ysl_currentstep
    for(j in c(step:(step+4))){
      temp_df[j,paste('ysl_lag',j-1, sep='')]<-temp_mod_ysl_currentstep
    }
  }
  assign(paste(s,'_pred_data_mod', sep=''),temp_df, envir = .GlobalEnv)
}
  return(temp_df)
}

#生成simulation数据-----------------------------------------------------------------------------------------
simu_func<-function(seed_index, weight_matrix){

#simulation
set.seed(seed_index)
#时间点数量
n=300
#空间点数量
m=50
#创建时间序列time point
time_points <- seq(from = 1, by = 1, length.out = n)
W<-weight_matrix



#首先生成每个序列各自的covariates x的时间序列， 对于每个地区，文中假设了有四个自变量，下面生成所有地区的四个自变量
sigma<-1
for(i in c(1:m))
  for (j in c(1:4)) {
    term_phi<-get(paste('phi_', i, '_', j, sep = ''))
    term_ar<-list(order=c(1,0,0), ar=term_phi)
    term_x<-arima.sim(term_ar, n=n, sd=sigma)
    assign(paste('x_', i, '_', j, sep = ''), term_x)
  }


#现在开始构建outcome y_{it}=k1*x_i_1+k2*x_i_2+k3*x_i_3+k4*x_i_4+kc1*xc1+kc2*xc2+kc3*xc3+kl1*+y_{i,t-1}+...+epsilon_{it}

for(i in c(1:m)){
  assign(paste('y',i,sep=''), rep(0,n))
  assign(paste('real_y',i,sep=''), rep(0,n))

}


temp_y<-get(paste('y',i,sep=''))
temp_y[1]<-0
assign(paste('y',i,sep=''), temp_y)


for(i in c(1:m)){
  assign(paste('ysl_',i, sep=''), rep(0,n))
}

#其实可以把beta的生成单独拿出来，然后只生成一次



#这里t必须在外面
for(t in c(6:n)){
  for(i in c(1:m)){
    temp_beta<-get(paste(state[i], "_coefficient", sep=''))
    temp_ysl<-get(paste('ysl_',i, sep=''))
    for(k in c(1:m)){
      yk<-get(paste('y',k, sep=''))
      temp_ysl[t-1]<-W[i,k]*yk[t-1]+temp_ysl[t-1]
    }
    
    temp_x1<-get(paste('x_', i, '_1', sep=''))[t]
    temp_x2<-get(paste('x_', i, '_2', sep=''))[t]
    temp_x3<-get(paste('x_', i, '_3', sep=''))[t]
    temp_x4<-get(paste('x_', i, '_4', sep=''))[t]
    
    temp_error<-rnorm(1,mean = 0, sd=0.1)
    temp_y<-get(paste('y',i,sep=''))
    temp_real_y<-get(paste('real_y',i,sep=''))
    
    
    #现在还没有temp_ysl
    temp_y[t]<-temp_beta[1]+temp_beta[2]*temp_ysl[t-1]+temp_beta[3]*temp_ysl[t-2]+temp_beta[4]*temp_ysl[t-3]+temp_beta[5]*temp_ysl[t-4]+temp_beta[6]*temp_ysl[t-5]+temp_beta[7]*temp_y[t-1]+temp_beta[8]*temp_y[t-2]+temp_beta[9]*temp_y[t-3]+temp_beta[10]*temp_y[t-4]+temp_beta[11]*temp_y[t-5]+temp_beta[12]*temp_x1+temp_beta[13]*temp_x2+temp_beta[14]*temp_x3+temp_beta[15]*temp_x4+temp_error

    temp_real_y[t]<-temp_beta[1]+temp_beta[2]*temp_ysl[t-1]+temp_beta[3]*temp_ysl[t-2]+temp_beta[4]*temp_ysl[t-3]+temp_beta[5]*temp_ysl[t-4]+temp_beta[6]*temp_ysl[t-5]+temp_beta[7]*temp_y[t-1]+temp_beta[8]*temp_y[t-2]+temp_beta[9]*temp_y[t-3]+temp_beta[10]*temp_y[t-4]+temp_beta[11]*temp_y[t-5]+temp_beta[12]*temp_x1+temp_beta[13]*temp_x2+temp_beta[14]*temp_x3+temp_beta[15]*temp_x4+temp_error

    
    
    # temp_y[t]<-temp_beta[1]*temp_ysl[t-1]+temp_beta[2]*temp_ysl[t-2]+temp_beta[2]*temp_ysl[t-3]+
    #   temp_beta[4]*temp_y[t-1]+temp_beta[5]*temp_y[t-2]+temp_beta[6]*temp_y[t-3]+
    #   log(1+((temp_beta[7]+temp_x1)^2)^temp_beta[8])+temp_error
    # 
    # temp_real_y[t]<-temp_beta[1]*temp_ysl[t-1]+temp_beta[2]*temp_ysl[t-2]+temp_beta[2]*temp_ysl[t-3]+
    #   temp_beta[4]*temp_y[t-1]+temp_beta[5]*temp_y[t-2]+temp_beta[6]*temp_y[t-3]+
    #   log(1+((temp_beta[7]+temp_x1)^2)^temp_beta[8])
    
    
    assign(paste('y',i, sep=''), temp_y)
    assign(paste('real_y',i, sep=''), temp_real_y)
    assign(paste('ysl_',i, sep=''), temp_ysl)
  }
}


#temp_ysl和temp_y用完之后得赋值还回来
ts.plot(y1)
par(mfrow=c(3,3))
for(i in c(1:m)){
  temp_series<-get(paste('y', i, sep=''))
  ts.plot(temp_series)
}


#接下来要把数据整理成一个个表格，然后建模,每个里面有y, x, ysl

for(i in c(1:m)){
  temp_y<-get(paste('y', i, sep=''))
  temp_unit<-temp_y
  temp_unit<-cbind(temp_unit, lag(get(paste('y', i, sep=''))))
  temp_unit<-cbind(temp_unit, lag(get(paste('y', i, sep='')), n=2) )
  temp_unit<-cbind(temp_unit, lag(get(paste('y', i, sep='')), n=3) )
  temp_unit<-cbind(temp_unit, lag(get(paste('y', i, sep='')), n=4) )
  temp_unit<-cbind(temp_unit, lag(get(paste('y', i, sep='')), n=5) )
  
  temp_unit<-cbind(temp_unit, get(paste('ysl_', i, sep='')))
  temp_unit<-cbind(temp_unit, lag(get(paste('ysl_', i, sep=''))))
  temp_unit<-cbind(temp_unit, lag(get(paste('ysl_', i, sep='')), n=2))
  temp_unit<-cbind(temp_unit, lag(get(paste('ysl_', i, sep='')), n=3))
  temp_unit<-cbind(temp_unit, lag(get(paste('ysl_', i, sep='')), n=4))
  temp_unit<-cbind(temp_unit, lag(get(paste('ysl_', i, sep='')), n=5))
  
  for(j in c(1:4)){
    temp_unit<-cbind(temp_unit,get(paste('x_', i, '_', j, sep = '')) )
  }
  temp_unit<-as.data.frame(temp_unit)
  names(temp_unit)<-c('y','y_lag1', 'y_lag2', 'y_lag3', 'y_lag4', 'y_lag5', 'ysl', 'ysl_lag1', 'ysl_lag2', 'ysl_lag3', 'ysl_lag4', 'ysl_lag5', 'x1', 'x2', 'x3', 'x4')
  temp_unit<-temp_unit[c(51:n),]
  assign(paste('unit_', i, sep=''), temp_unit, envir = .GlobalEnv)
}
return(temp_unit)
}
#--------------------------------------------------------------------------------------------------------------------------------------


#将series分成pre treatment 和post treatment.
sep_series<-function(T0,i){

  temp_unit<-get(paste("unit_",i, sep=''))
  temp_unit_pre<-temp_unit[c(1:T0),]
  temp_unit_post<-temp_unit[(T0+1),]
  temp_unit_post_series<-temp_unit[c(T0+1:T0+20),]
  return(list(temp_unit_pre=temp_unit_pre, temp_unit_post=temp_unit_post, temp_unit_post_series=temp_unit_post_series))
  # assign(paste('unit_',i, '_pre', sep = ''), temp_unit_pre, envir = .GlobalEnv)
  # assign(paste('unit_',i, '_post', sep = ''), temp_unit_post, envir = .GlobalEnv)
  # assign(paste('unit_',i, '_post_series', sep = ''), temp_unit_post_series, envir = .GlobalEnv)

}

#residual matrix_NC
gen_residual_matrix<-function(parameters){
residual_matrix_NC<-NULL
valid_residual_matrix_NC<-NULL

for(s in state){
# print(state)
  temp_data<-get(paste(s, "_training_data",sep = ""))
  
  temp_model<-lm(y~., data=temp_data[,c('y',parameters)])
  temp_tstep<-step(temp_model, trace = 0)
  assign(paste0(s, '_linear_model'), temp_tstep)
  temp_residuals<-temp_tstep$residuals
  residual_matrix_NC<-cbind(residual_matrix_NC,temp_residuals)#先全放在一起
  #再做一个rbind的符合syntheti格式的dataframe
  
  assign(paste(s,"_residual", sep=""), temp_residuals)
  temp_data_valid<-get(paste(s,'_validation_data', sep=''))
  #计算模型在验证集中的预测误差
  temp_valid_resid<-temp_data_valid[,4]-predict(temp_tstep,temp_data_valid[,c('y',parameters)])
  assign(paste(s,'valid_resid', sep=''),temp_valid_resid)
  valid_residual_matrix_NC<-cbind(valid_residual_matrix_NC, temp_valid_resid)
}
colnames(residual_matrix_NC)<-state
colnames(valid_residual_matrix_NC)<-state
result_list<-list(residual_matrix_NC, valid_residual_matrix_NC)
return(result_list)
}

#kernel function
K.gaussian <- function(u) {
  return((1 / sqrt(2 * pi)) * exp(-0.5 * u^2))
}

L_2d.gaussian <- function(s) {
  lon<-s[1]
  lat<-s[2]
  (1 / (2 * pi)) * exp(-0.5 * (lon^2 + lat^2))
}


