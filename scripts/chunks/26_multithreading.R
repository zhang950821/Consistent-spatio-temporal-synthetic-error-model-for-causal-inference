# Source chunk: Multithreading
# Original Rmd lines: 3135-3463

# 设置并行计算的核心数
cores <- 4  # 你可以根据你的计算机配置调整核心数
cl <- makeCluster(cores)
registerDoParallel(cl)

seednum<-101:105

result_table<- foreach(k = seednum, .combine = rbind) %dopar% {
  library(haven)
  library(dplyr)
  library(forecast)
  library(tseries)
  library(FinTS)
  library(Synth)
  library(gsynth)
  library(tidyverse)
  library(glmnet)
  library(spacetime)
  library(xts)
  library(spdep)
  library(gstat)
  library(augsynth)
  
  library(iterators)
  library(parallel)
  library(foreach)
  library(doParallel)
  #library(np)
  library(mgcv)
  library(MASS)
  
  
  load('20241024_adaptive_synthetic_pred.RData')
  
  
  total_process<-function(seed){
    m=50
    #生成covariates 的ar(1)系数 phi
    for(i in c(1:m)){
      for(j in c(1:8)){
        term_phi<-sin(j)*cos(location[i,'longitude']+location[i,'latitude'])/10
        assign(paste('phi_', i, '_', j, sep = ''), term_phi)
      }
    }
    
    
    #提取上述拟合模型中所有拟合系数beta
    #包含的参数有intercept, ysl_lag1,  ysl_lag2,  ysl_lag3,  ysl_lag4,  ysl_lag5, y_lag1, y_lag2, y_lag3, y_lag4, y_lag5, x_1t, x_2t, x_3t, x_4t
    #补充：要加上几个对kansas有显著影响的unobs_covariates，同时这些covariates对于其他的部分地区的outcome有显著影响
    parameter_list<-c('(Intercept)', 'ysl_lag1', 'ysl_lag2',  'ysl_lag3',  'ysl_lag4',  'ysl_lag5', 'y_lag1', 'y_lag2', 'y_lag3', 'y_lag4', 'y_lag5', 'totalwagescapita_dif', 'emplvlcapita_dif', 'popestimate_dif', 'qtrly_estabs_count_dif')
    temp_beta<-rep(0,19)
    for(i in c(1:50)){
      temp_s<-state[i]
      temp_LST_model<-get(paste(temp_s, '_LST_model', sep=''))
      temp_beta<-rep(0,19)
      for(j in c(1:15)){
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
      
      for(j in c(16:19)){
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
          
          
          
          temp_y[t]<-temp_beta[1]+temp_beta[2]*temp_ysl[t-1]+temp_beta[3]*temp_ysl[t-2]+temp_beta[4]*temp_ysl[t-3]+temp_beta[5]*temp_ysl[t-4]+temp_beta[6]*temp_ysl[t-5]+temp_beta[7]*temp_y[t-1]+temp_beta[8]*temp_y[t-2]+temp_beta[9]*temp_y[t-3]+temp_beta[10]*temp_y[t-4]+temp_beta[11]*temp_y[t-5]+temp_beta[12]*temp_x1+temp_beta[13]*temp_x2+temp_beta[14]*temp_x3+temp_beta[15]*temp_x4+temp_beta[16]*temp_x5+temp_beta[17]*temp_x6+temp_beta[18]*temp_x7+temp_beta[19]*temp_x8+temp_error
          
          temp_real_y[t]<-temp_beta[1]+temp_beta[2]*temp_ysl[t-1]+temp_beta[3]*temp_ysl[t-2]+temp_beta[4]*temp_ysl[t-3]+temp_beta[5]*temp_ysl[t-4]+temp_beta[6]*temp_ysl[t-5]+temp_beta[7]*temp_y[t-1]+temp_beta[8]*temp_y[t-2]+temp_beta[9]*temp_y[t-3]+temp_beta[10]*temp_y[t-4]+temp_beta[11]*temp_y[t-5]+temp_beta[12]*temp_x1+temp_beta[13]*temp_x2+temp_beta[14]*temp_x3+temp_beta[15]*temp_x4+temp_beta[16]*temp_x5+temp_beta[17]*temp_x6+temp_beta[18]*temp_x7+temp_beta[19]*temp_x8+temp_error
          
          
          
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
        
        for(j in c(1:8)){
          temp_unit<-cbind(temp_unit,get(paste('x_', i, '_', j, sep = '')) )
        }
        temp_unit<-as.data.frame(temp_unit)
        names(temp_unit)<-c('y','y_lag1', 'y_lag2', 'y_lag3', 'y_lag4', 'y_lag5', 'ysl', 'ysl_lag1', 'ysl_lag2', 'ysl_lag3', 'ysl_lag4', 'ysl_lag5', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8')
        temp_unit<-temp_unit[c(51:n),]
        assign(paste('unit_', i, sep=''), temp_unit, envir = .GlobalEnv)
      }
      return(temp_unit)
    }
    
    
    test_return<-simu_func(seed, W)
    
    #给Kansas找一个T_0然后加一个causal effect
    T0=100
    
    
    
    sep_series(100)
    #数据生成完了，接下来可以算了应该
    
    
    
    residual_matrix_simu<-NULL
    for(i in c(1:50)){
      temp_data<-get(paste('unit_',i, "_pre",sep = ""))
      
      
      temp_model<-lm(y~., data=temp_data[,-c( 15:20)])
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
    
    #prediction
    #Kansas是第16个unit
    obs_Kansas_post1<-unit_16_post[1,'y']
    #step1: LST预测 Kansas
    sim_LST_pred_Kansas<-predict(simu_unit16_LST_model, unit_16_post[1,])
    
    #计算各地区在T_0+1的误差
    sim_control_residual_post1<-rep(0,50)#还是带了Kansas的位置但是值不给
    names(sim_control_residual_post1)<-state
    for(i in c(1:15, 17:50)){
      temp_obs_post1<-get(paste('unit_', i, '_post', sep=''))[1,'y']
      temp_LST_pred1<-predict(get(paste('simu_unit', i, '_LST_model', sep='')), get(paste('unit_', i, '_post', sep=''))[1,])
      temp_LST_resid1<-temp_obs_post1-temp_LST_pred1
      assign(paste('unit', i, '_resid_post1', sep=''), temp_LST_resid1)
      sim_control_residual_post1[i]<-temp_LST_resid1
    }
    #step2: synthetic error
    
    synteitic_kansas_resid1<-predict(alasso_SE_pred_model_simu_cv, s=best_lambda_simu, newx = as.matrix(sim_control_residual_post1)[-16])
    #final result
    sim_pred_se_post1<-sim_LST_pred_Kansas+synteitic_kansas_resid1
    est_error<-obs_Kansas_post1-sim_pred_se_post1
    LST_error<-obs_Kansas_post1-sim_LST_pred_Kansas
    
    # print('obs outcome')
    # obs_Kansas_post1
    # print('LST outcome')
    # sim_LST_pred_Kansas
    # print('se_outcome')
    # sim_pred_se_post1
    
    result<-rep(0,5)
    result[1]<-obs_Kansas_post1
    result[2]<-sim_LST_pred_Kansas
    result[3]<-sim_pred_se_post1
    result[4]<-est_error
    result[5]<-LST_error
    names(result)<-c('obs', 'LST', 'SE', 'SE_error', 'LST_error')
    print(result)
    return(result)
  }
  
  seed_num<-seednum
  total_process(k)
  
} 
stopCluster(cl)

