# Source chunk: simulation study LSTSEM--data generating
# Original Rmd lines: 2434-2674



m=50
#生成covariates 的ar(1)系数 phi
for(i in c(1:m)){
  for(j in c(1:8)){
    term_phi<-sin(j)*cos(location[i,'longitude']+location[i,'latitude'])/10
    assign(paste('phi_', i, '_', j, sep = ''), term_phi)
  }
}

#生成公共covariates ar系数
for(i in c(1:4)){
  temp_comm_phi<-0.5+0.2*sin(i)
  assign(paste('comm_phi_', i, sep=''), temp_comm_phi)
}

#提取上述拟合模型中所有拟合系数beta
#包含的参数有intercept, ysl_lag1,  ysl_lag2,  ysl_lag3,  ysl_lag4,  ysl_lag5, y_lag1, y_lag2, y_lag3, y_lag4, y_lag5, x_1t, x_2t, x_3t, x_4t
#补充：要加上4个对kansas有显著影响的unobs_covariates，同时这些covariates对于其他的部分地区的outcome有显著影响
#再次补充：再加上4个common covariates
#所以目前一共23个变量
parameter_list<-c('(Intercept)', 'ysl_lag1', 'ysl_lag2',  'ysl_lag3',  'ysl_lag4',  'ysl_lag5', 'y_lag1', 'y_lag2', 'y_lag3', 'y_lag4', 'y_lag5', 'totalwagescapita_dif', 'emplvlcapita_dif', 'popestimate_dif', 'qtrly_estabs_count_dif')
temp_beta<-rep(0,23)
for(i in c(1:50)){
  temp_s<-state[i]
  temp_LST_model<-get(paste(temp_s, '_LST_model', sep=''))
  temp_beta<-rep(0,23)
  for(j in c(1:13)){
    temp_param<-parameter_list[j]
    temp_coef<-as.data.frame(coef(get(paste(temp_s, '_LST_model', sep=''))))
    if(is.na(temp_coef[temp_param,1])){
      temp_beta[j]<-0
    } else{
      temp_beta[j]<-temp_coef[temp_param,1]
    }
  }
  # for(j in c(16,17)){
  #   if(i<=30){
  #     temp_beta[j]<-sin(i*j)
  #   }
  # }
  
  for(j in c(14:23)){
        temp_beta[j]<-sin(i*j)*cos(i+j)
    
  }
  assign(paste(temp_s, '_coefficient', sep=''),temp_beta)
}




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



#生成公用covariates
sigma_comm<-0.1
  for (j in c(1:4)) {
    term_phi<-get(paste('comm_phi_', j, sep = ''))
    term_ar<-list(order=c(1,0,0), ar=term_phi)
    term_x<-arima.sim(term_ar, n=n, sd=sigma_comm)
    assign(paste('comm_x_', j, sep = ''), term_x)
  }


#生成相互有相关性的50个平稳covariates时间序列(for each j)

generate_x<-function(j){
  #均值向量
mu<-rep(0,50)
#定义协方差矩阵
Sigma<-matrix(rep(0,2500), 50, 50)
for(i1 in c(1:50)){
  for(i2 in c(1:50)){
    if(i1==i2){
      Sigma[i1, i2]=1
    } else{
      Sigma[i1, i2]<-sin(1/W[i1, i2]*j)#这里后面循环加j的时候记得乘一下
    }
  }
}
#tanh(Sigma%*%Sigma/30)
Sigma_PD<-Sigma%*%Sigma/500
# for(i in c(1:50)){
#   Sigma_PD[i,i]=1
# }
temp_x<-mvrnorm(n, mu=mu, Sigma=Sigma_PD)
for(i in c(1:50)){
  assign(paste('x_',i,'_', j, sep = ''), arima.sim(n=n, list(ar=get(paste('phi_',i,'_', j, sep=''))), innov = temp_x[,i]), envir = .GlobalEnv)
}
}

for(j in c(1:8)){
  generate_x(j)
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
    temp_x5<-get(paste('x_', i, '_5', sep=''))[t]
    temp_x6<-get(paste('x_', i, '_6', sep=''))[t]
    temp_x7<-get(paste('x_', i, '_7', sep=''))[t]
    temp_x8<-get(paste('x_', i, '_8', sep=''))[t]
    
    temp_error<-rnorm(1,mean = 0, sd=0.01)
    temp_y<-get(paste('y',i,sep=''))
    temp_real_y<-get(paste('real_y',i,sep=''))
    
    

    temp_y[t]<-temp_beta[1]+temp_beta[2]*temp_ysl[t-1]+temp_beta[3]*temp_ysl[t-2]+temp_beta[4]*temp_ysl[t-3]+temp_beta[5]*temp_ysl[t-4]+temp_beta[6]*temp_ysl[t-5]+temp_beta[7]*temp_y[t-1]+temp_beta[8]*temp_y[t-2]+temp_beta[9]*temp_y[t-3]+temp_beta[10]*temp_y[t-4]+temp_beta[11]*temp_y[t-5]+temp_beta[12]*temp_x1+temp_beta[13]*temp_x2+temp_beta[14]*temp_x3+temp_beta[15]*temp_x4+temp_beta[16]*temp_x5+temp_beta[17]*temp_x6+temp_beta[18]*temp_x7+temp_beta[19]*temp_x8+temp_beta[20]*comm_x_1[t]+temp_beta[21]*comm_x_2[t]+temp_beta[22]*comm_x_3[t]+temp_beta[23]*comm_x_4[t]+temp_error

    temp_real_y[t]<-temp_beta[1]+temp_beta[2]*temp_ysl[t-1]+temp_beta[3]*temp_ysl[t-2]+temp_beta[4]*temp_ysl[t-3]+temp_beta[5]*temp_ysl[t-4]+temp_beta[6]*temp_ysl[t-5]+temp_beta[7]*temp_y[t-1]+temp_beta[8]*temp_y[t-2]+temp_beta[9]*temp_y[t-3]+temp_beta[10]*temp_y[t-4]+temp_beta[11]*temp_y[t-5]+temp_beta[12]*temp_x1+temp_beta[13]*temp_x2+temp_beta[14]*temp_x3+temp_beta[15]*temp_x4+temp_beta[16]*temp_x5+temp_beta[17]*temp_x6+temp_beta[18]*temp_x7+temp_beta[19]*temp_x8+temp_beta[20]*comm_x_1[t]+temp_beta[21]*comm_x_2[t]+temp_beta[22]*comm_x_3[t]+temp_beta[23]*comm_x_4[t]+temp_error

    
    
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
  
  #非公共covariates
  for(j in c(1:8)){
    temp_unit<-cbind(temp_unit,get(paste('x_', i, '_', j, sep = '')) )
  }
  #公共covariates
  for(j in c(1:4)){
    temp_unit<-cbind(temp_unit,get(paste('comm_x_', j, sep = '')))
  }

  
  temp_unit<-as.data.frame(temp_unit)
  names(temp_unit)<-c('y','y_lag1', 'y_lag2', 'y_lag3', 'y_lag4', 'y_lag5', 'ysl', 'ysl_lag1', 'ysl_lag2', 'ysl_lag3', 'ysl_lag4', 'ysl_lag5', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'comm_x1', 'comm_x2','comm_x3', 'comm_x4')
  temp_unit<-temp_unit[c(51:n),]
  assign(paste('unit_', i, sep=''), temp_unit, envir = .GlobalEnv)
}
return(temp_unit)
}


test_return<-simu_func(150, W)

#给Kansas找一个T_0然后加一个causal effect
T0=100



for (i in c(1:50)) {
  temp_split <- sep_series(T0, i)
  assign(paste0('unit_', i, '_pre'), temp_split$temp_unit_pre, envir = .GlobalEnv)
  assign(paste0('unit_', i, '_post'), temp_split$temp_unit_post, envir = .GlobalEnv)
  assign(paste0('unit_', i, '_post_series'), temp_split$temp_unit_post_series, envir = .GlobalEnv)
}
#数据生成完了，接下来可以算了应该


