# Source chunk: synthetic control and ASCM
# Original Rmd lines: 978-1297

# #先把matrix_total_mod数据和在一个表里---------(这里会报错我先注释了)
# diff_kansas_total<-NULL
# temp_matrix_total<-NULL
# for(s in state){
#   temp_matrix_total<-get(paste(s,'_matrix_pre_and_pots', sep = ''))
#   diff_kansas_total<-rbind(diff_kansas_total,temp_matrix_total)
# }
# 
# diff_kansas_total$treated<-0
# 
# 
# diff_kansas_total[diff_kansas_total$state=='Kansas'& diff_kansas_total$timepoint>89,'treated']<-1
# diff_kansas_total$fips<-0
# 
# i=1
# for(s in state){
#   diff_kansas_total[diff_kansas_total$state==s,]$fips<-i
#   i=i+1
# }
#write.csv(diff_kansas_total, file = 'data/diff_kansas_total.csv')


#synthetic control ----------------------------------------------------------------------------------------------
diff_kansas_total<-read.csv('data/diff_kansas_total.csv')[,-1]
diff_kansas<-read.csv('data/diff_kansas.csv')[,-1]

diff_kansas_total<-cbind(diff_kansas_total, abs(diff_kansas_total[,'y']))
colnames(diff_kansas_total)[19]<-'yabs'

diff_kansas<-cbind(diff_kansas, abs(diff_kansas[,'y']))
colnames(diff_kansas)[19]<-'yabs'
#这里用diff_kansas就是将t=78视作intervention time, 就是training vs validation.用diff_kansas_total就是将t=89视作intervention time, 就是pretreatment vs posttreatment 
#对原始数据进行synthetic, predictor就是y本身
#predictors中加yabs就是modified synthetic control,不加就是普通的
dataprep.out<-dataprep(foo = diff_kansas_total,
                       predictors = c('ysl','totalwagescapita_dif','emplvlcapita_dif','popestimate_dif', 'qtrly_estabs_count_dif'),
                       predictors.op = 'mean',
                       dependent = 'y',
                       unit.variable = 'fips',
                       time.variable = 'timepoint',
                       treatment.identifier=16,
                       special.predictors = NULL,
                       controls.identifier = unique(diff_kansas$fips)[-16],
                       time.predictors.prior = c(3:89),#need to be modified according to validation or predict
                       time.optimize.ssr = c(3:89),
                       unit.names.variable = 'state',
                       time.plot = 3:105
)

synth.out<-synth(dataprep.out, output=TRUE)
syn_weights<-synth.out$solution.w
syn_weights<-syn_weights[,1]

synth_kansas_post<-0

control_states<-state[-16]

for(i in c(1:49)){
  temp_y<-get(paste(control_states[i],'_pred_data', sep=''))$y#如果是做validation就要换成_validation_data， 不然是pred_data
  synth_kansas_post<-synth_kansas_post+temp_y*syn_weights[i]
}

SC_causal_effect<-Kansas_pred_data$y-synth_kansas_post#如果是做validation就要换成_validation_data, 不然是pred_data

SC_MSE_valid<-mean(SC_causal_effect^2)
#training MSE
synth_kansas_train<-0

for(i in c(1:49)){
  temp_y<-get(paste(control_states[i],'_training_data', sep=''))$y#如果是做validation就要换成_validation_data
  synth_kansas_train<-synth_kansas_train+temp_y*syn_weights[i]
}
SC_MSE_train<-mean((Kansas_training_data$y-synth_kansas_train)^2)


#synth_kansas_post要转化回gdp数据
post_SC_lngdpcapita<-0

last_value<-last_pre_lngdpcapita
for(i in c(1:16)){
  post_SC_lngdpcapita[i]<-last_value+synth_kansas_post[i]/100
  last_value<-post_SC_lngdpcapita[i]
}



post_SC_lngdpcapita<-ts(post_SC_lngdpcapita,start = 90)
one_step_effect<-exp(kansas[1665,'lngdpcapita'])-exp(post_SC_lngdpcapita[1])
four_step_effect<-0
for(i in c(1:4)){
  temp_step_effect<-exp(kansas[(1664+i),'lngdpcapita'])-exp(post_SC_lngdpcapita[i])
  four_step_effect<-four_step_effect+temp_step_effect
}
four_step_ave_effect<-four_step_effect/4

shorter_obs_kansas_lngdpcapita<-obs_kansas_lngdpcapita[11:46]





#modified synthetic control ----------------------------------------------------------------------------------------------
#predictors中加yabs就是modified synthetic control,不加就是普通的
dataprep.out<-dataprep(foo = diff_kansas_total,
                       predictors = c('ysl','totalwagescapita_dif','emplvlcapita_dif','popestimate_dif', 'qtrly_estabs_count_dif', 'yabs'),
                       predictors.op = 'mean',
                       dependent = 'y',
                       unit.variable = 'fips',
                       time.variable = 'timepoint',
                       treatment.identifier=16,
                       special.predictors = NULL,
                       controls.identifier = unique(diff_kansas$fips)[-16],
                       time.predictors.prior = c(3:89),#need to be modified according to validation or predict
                       time.optimize.ssr = c(3:89),
                       unit.names.variable = 'state',
                       time.plot = 3:105
)

msynth.out<-synth(dataprep.out, output=TRUE)
msyn_weights<-msynth.out$solution.w
msyn_weights<-msyn_weights[,1]

msynth_kansas_post<-0

control_states<-state[-16]

for(i in c(1:49)){
  temp_y<-get(paste(control_states[i],'_pred_data', sep=''))$y#如果是做validation就要换成_validation_data， 不然是pred_data
  msynth_kansas_post<-msynth_kansas_post+temp_y*msyn_weights[i]
}

mSC_causal_effect<-Kansas_pred_data$y-msynth_kansas_post#如果是做validation就要换成_validation_data, 不然是pred_data

mSC_MSE_valid<-mean(mSC_causal_effect^2)
#training MSE
msynth_kansas_train<-0

for(i in c(1:49)){
  temp_y<-get(paste(control_states[i],'_training_data', sep=''))$y#如果是做validation就要换成_validation_data
  msynth_kansas_train<-msynth_kansas_train+temp_y*syn_weights[i]
}
mSC_MSE_train<-mean((Kansas_training_data$y-msynth_kansas_train)^2)


#synth_kansas_post要转化回gdp数据
post_mSC_lngdpcapita<-0

last_value<-last_pre_lngdpcapita
for(i in c(1:16)){
  post_mSC_lngdpcapita[i]<-last_value+msynth_kansas_post[i]/100
  last_value<-post_mSC_lngdpcapita[i]
}



post_mSC_lngdpcapita<-ts(post_mSC_lngdpcapita,start = 90)
mSC_one_step_effect<-exp(kansas[1665,'lngdpcapita'])-exp(post_mSC_lngdpcapita[1])
mSC_four_step_effect<-0
for(i in c(1:4)){
  temp_step_effect<-exp(kansas[(1664+i),'lngdpcapita'])-exp(post_mSC_lngdpcapita[i])
  mSC_four_step_effect<-mSC_four_step_effect+temp_step_effect
}
mSC_four_step_ave_effect<-mSC_four_step_effect/4

shorter_obs_kansas_lngdpcapita<-obs_kansas_lngdpcapita[11:46]

#ASCM-----------------------------------------------------------------
#augsynth only time series
asyn <- augsynth(y ~ treated|y+totalwagescapita_dif+emplvlcapita_dif+popestimate_dif+qtrly_estabs_count_dif, state, timepoint,diff_kansas_total,scm=T)#这里可以加ysl，就是ST-ASCM了
summary(asyn)

asyn_weight<-as.data.frame(asyn$weights)


#合成valid  aug potential outcome
augsyn_pot_outcom_post=rep(0,16)
for(s in state){
  if(s=='Kansas'){
    next
  }
  temp_weight<-asyn_weight[s,]
  temp_w_y<-get(paste(s,'_pred_data',sep=""))$y*temp_weight
  augsyn_pot_outcom_post<-augsyn_pot_outcom_post+temp_w_y
}

#转化成gdp数据
augsyn_pot_outcom_post

post_ASCM_lngdpcapita<-0

last_value<-last_pre_lngdpcapita
for(i in c(1:16)){
  post_ASCM_lngdpcapita[i]<-last_value+augsyn_pot_outcom_post[i]/100
  last_value<-post_ASCM_lngdpcapita[i]
}

post_ASCM_lngdpcapita<-ts(post_ASCM_lngdpcapita,start = 90)

#下面两个在下一块画图的时候再转为series并定义一个额外的series量
post_SC_lngdpcapita<-as.numeric(post_SC_lngdpcapita)
post_conventional_ASCM_lngdpcapita<-as.numeric(post_ASCM_lngdpcapita)

#ST-ASCM--------------------------------------------------------------------------------
#就在上面augsynth中加一个ysl就行了
#augsynth only time series
asyn <- augsynth(y ~ treated|y+ysl+totalwagescapita_dif+emplvlcapita_dif+popestimate_dif+qtrly_estabs_count_dif, state, timepoint,diff_kansas_total,scm=T)#这里可以加ysl，就是ST-ASCM了
summary(asyn)

asyn_weight<-as.data.frame(asyn$weights)


#合成valid  aug potential outcome
augsyn_pot_outcom_post=rep(0,16)
for(s in state){
  if(s=='Kansas'){
    next
  }
  temp_weight<-asyn_weight[s,]
  temp_w_y<-get(paste(s,'_pred_data',sep=""))$y*temp_weight
  augsyn_pot_outcom_post<-augsyn_pot_outcom_post+temp_w_y
}

#转化成gdp数据
augsyn_pot_outcom_post

post_ASCM_lngdpcapita<-0

last_value<-last_pre_lngdpcapita
for(i in c(1:16)){
  post_ASCM_lngdpcapita[i]<-last_value+augsyn_pot_outcom_post[i]/100
  last_value<-post_ASCM_lngdpcapita[i]
}

post_ASCM_lngdpcapita<-ts(post_ASCM_lngdpcapita,start = 90)

#下面两个在下一块画图的时候再转为series并定义一个额外的series量
post_SC_lngdpcapita<-as.numeric(post_SC_lngdpcapita)
post_ST_ASCM_lngdpcapita<-as.numeric(post_ASCM_lngdpcapita)

#generalized synthetic control-----------------------标准化-----------
diff_kansas_total_mod<-diff_kansas_total
for(s in state){
  for(t in c(2:103)){
  diff_kansas_total_mod[diff_kansas_total_mod$state==s,][t,'totalwagescapita_dif']<-(diff_kansas_total[diff_kansas_total$state==s,][t,'totalwagescapita'])/(diff_kansas_total[diff_kansas_total$state==s,][t-1,'totalwagescapita'])-1
  
  diff_kansas_total_mod[diff_kansas_total_mod$state==s,][t,'emplvlcapita_dif']<-(diff_kansas_total[diff_kansas_total$state==s,][t,'emplvlcapita'])/(diff_kansas_total[diff_kansas_total$state==s,][t-1,'emplvlcapita'])-1
    
    diff_kansas_total_mod[diff_kansas_total_mod$state==s,][t,'popestimate_dif']<-(diff_kansas_total[diff_kansas_total$state==s,][t,'popestimate'])/(diff_kansas_total[diff_kansas_total$state==s,][t-1,'popestimate'])-1
      
      diff_kansas_total_mod[diff_kansas_total_mod$state==s,][t,'qtrly_estabs_count_dif']<-(diff_kansas_total[diff_kansas_total$state==s,][t,'qtrly_estabs_count'])/(diff_kansas_total[diff_kansas_total$state==s,][t-1,'qtrly_estabs_count'])-1
  }
}
diff_kansas_total_mod<-diff_kansas_total_mod[diff_kansas_total_mod$timepoint!=3,]
#给validation 创建一个dataset
diff_kansas_total_mod_validation<-diff_kansas_total_mod
diff_kansas_total_mod_validation[diff_kansas_total_mod_validation$timepoint>78 & diff_kansas_total_mod_validation$state=='Kansas','treated']<-1
#gsynth validation
gsynth_valid.out <- gsynth(y~treated+totalwagescapita_dif+emplvlcapita_dif+popestimate_dif+qtrly_estabs_count_dif, data = diff_kansas_total_mod_validation, index = c("state","timepoint"), nboots = 500, inference = "parametric", se = TRUE, parallel = TRUE)

gsynth_valid.out$att

gsynth_valid_effect_total<-gsynth_valid.out$att
gsynth_valid_effect_pre<-gsynth_valid_effect_total[1:75]
gsynth_valid_effect_post<-gsynth_valid_effect_total[76:86]

gsynth_training_MSE<-mean(gsynth_valid_effect_pre^2)
gsynth_valid_MSE<-mean(gsynth_valid_effect_post^2)


  gap_plot<-plot(gsynth_valid.out, type = "gap" , xlab = "time_point", ylab="ATT")
  #ggsave(paste0("D:/Lara/wildfires_social_vulnerability/results/", county,"_diff.png"))
  
  ### treated average and estimated counterfactual average outcomesraw = "none", main="",
  plot(gsynth_valid.out, type = "counterfactual",  xlab = "time_point", ylab="y")


#gsynth的第一项得写treatment indicator(gsynth prediction)
gsynth.out <- gsynth(y~treated+totalwagescapita_dif+emplvlcapita_dif+popestimate_dif+qtrly_estabs_count_dif, data = diff_kansas_total_mod, index = c("state","timepoint"), nboots = 500, inference = "parametric", se = TRUE, parallel = TRUE)

gsynth.out$att
gsynth.out$Y.ct

#转化成gdp数据
gsynth_pot_outcom_post<-gsynth.out$Y.ct[87:102]

post_gsynth_lngdpcapita<-0

last_value<-last_pre_lngdpcapita
for(i in c(1:16)){
  post_gsynth_lngdpcapita[i]<-last_value+gsynth_pot_outcom_post[i]/100
  last_value<-post_gsynth_lngdpcapita[i]
}

post_gsynth_lngdpcapita<-ts(post_gsynth_lngdpcapita,start = 90)



gsynth_one_step_effect<-exp(kansas[1665,'lngdpcapita'])-exp(post_gsynth_lngdpcapita[1])
gsynth_four_step_effect<-0
for(i in c(1:4)){
  temp_step_effect<-exp(kansas[(1664+i),'lngdpcapita'])-exp(post_gsynth_lngdpcapita[i])
  gsynth_four_step_effect<-gsynth_four_step_effect+temp_step_effect
}
gsynth_four_step_ave_effect<-gsynth_four_step_effect/4


#下面两个在下一块画图的时候再转为series并定义一个额外的series量
post_SC_lngdpcapita<-as.numeric(post_SC_lngdpcapita)
post_ASCM_lngdpcapita<-as.numeric(post_conventional_ASCM_lngdpcapita)
post_ST_ASCM_lngdpcapita<-as.numeric(post_ST_ASCM_lngdpcapita)



  gap_plot<-plot(gsynth.out, type = "gap" , xlab = "time_point", ylab="ATT")
  #ggsave(paste0("D:/Lara/wildfires_social_vulnerability/results/", county,"_diff.png"))
  
  ### treated average and estimated counterfactual average outcomesraw = "none", main="",
  plot(gsynth.out, type = "counterfactual",  xlab = "time_point", ylab="y")
