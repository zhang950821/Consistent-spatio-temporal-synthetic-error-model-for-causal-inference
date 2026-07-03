# Source chunk: 1step prediction result
# Original Rmd lines: 2717-2748


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

print('obs outcome')
obs_Kansas_post1
print('LST outcome')
sim_LST_pred_Kansas
print('se_outcome')
sim_pred_se_post1

