# Source chunk: data preparasion
# Original Rmd lines: 322-413

kansas<-read.csv('data/kansas.csv', header = T)
#原始kansas数据
kansas_raw<-read.csv('data/kansas_raw.csv')
location<-read.csv('data/us-state-capitals.csv')
#step1:处理Kansas数据
#step1.1: 将Kansas的state列中所有元素中的空格替换成下划
#write.csv(kansas_raw, file = 'data/kansas_modname.csv')
#提取state（其实在location提取就可以）
state<-unique(location$name)
#读取每个州的training数据
for (s in state){
    temp_data<-read.csv(paste('data/total_matrix/diff_using_60train_data/',s,'.csv', sep=''))
  assign(paste(s,'_training_data', sep=''), temp_data)
}
#标准化协变量，得出training_mod数据
for(s in state){
  temp_data<-read.csv(paste('data/total_matrix/diff_using_60train_data/',s,'.csv', sep=''))
    for(t in c(2:70)){
  temp_data[t,'totalwagescapita_dif']<-(temp_data[t,'totalwagescapita'])/(temp_data[t-1,'totalwagescapita'])-1
  
  temp_data[t,'emplvlcapita_dif']<-(temp_data[t,'emplvlcapita'])/(temp_data[t-1,'emplvlcapita'])-1
    
    temp_data[t,'popestimate_dif']<-(temp_data[t,'popestimate'])/(temp_data[t-1,'popestimate'])-1
      
      temp_data[t,'qtrly_estabs_count_dif']<-(temp_data[t,'qtrly_estabs_count'])/(temp_data[t-1,'qtrly_estabs_count'])-1
    }
  temp_data<-temp_data[-1,]
  assign(paste(s,'_training_data_mod', sep=''), temp_data)
}



#读取每个州的validation数据
for (s in state){
    temp_data<-read.csv(paste('data/total_matrix/all_diff_data/',s,'_matrix_pre_and_post_mod.csv', sep=''))
    temp_data_valid<-temp_data[72:83,]
  assign(paste(s,'_validation_data', sep=''), temp_data_valid)
  # validation_outputpath<-paste('data/total_matrix/diff_valid_data/',s,'_validation_data.csv', sep='')
  # write.csv(temp_data_valid,file = validation_outputpath)
}

#标准化协变量，得出validation_mod数据
for(s in state){
  temp_data<-get(paste0(s,'_validation_data'))
  temp_training_data_last<-get(paste0(s,'_training_data'))
  
  temp_data[1,'totalwagescapita_dif']<-(temp_data[1,'totalwagescapita'])/(temp_training_data_last[70,'totalwagescapita'])-1
  
  temp_data[1,'emplvlcapita_dif']<-(temp_data[1,'emplvlcapita'])/(temp_training_data_last[70,'emplvlcapita'])-1
    
  temp_data[1,'popestimate_dif']<-(temp_data[1,'popestimate'])/(temp_training_data_last[70,'popestimate'])-1
      
  temp_data[1,'qtrly_estabs_count_dif']<-(temp_data[1,'qtrly_estabs_count'])/(temp_training_data_last[70,'qtrly_estabs_count'])-1
  
    for(t in c(2:12)){
  temp_data[t,'totalwagescapita_dif']<-(temp_data[t,'totalwagescapita'])/(temp_data[t-1,'totalwagescapita'])-1
  
  temp_data[t,'emplvlcapita_dif']<-(temp_data[t,'emplvlcapita'])/(temp_data[t-1,'emplvlcapita'])-1
    
  temp_data[t,'popestimate_dif']<-(temp_data[t,'popestimate'])/(temp_data[t-1,'popestimate'])-1
      
  temp_data[t,'qtrly_estabs_count_dif']<-(temp_data[t,'qtrly_estabs_count'])/(temp_data[t-1,'qtrly_estabs_count'])-1
    }
  assign(paste(s,'_validation_data_mod', sep=''), temp_data)
}

#读取每个周的pre-treatment data
for (s in state){
   temp_data<-read.csv(paste('data/total_matrix/all_diff_data/',s,'_matrix_pre_and_post_mod.csv', sep=''))
   assign(paste(s,'_pre_treat_data', sep=''), temp_data)
   pre_treatment_outputpath<-paste('data/total_matrix/diff_using_60train_data/pre_treatment_data/',s,'_pre_treatment_data.csv', sep='')
   #write.csv(temp_data, file=pre_treatment_outputpath)
}

#读取location数据并生成weight matrix
  location<-read.csv('data/us-state-capitals.csv')
#spatial weight vector
state_dataframe<-NULL
state_dataframe<-cbind(index=c(1:50),location)

distance_matrix<-distance_matrix_func(state_dataframe)

#inverse distance weight matrix
W<-1/distance_matrix
for(i in c(1:50)){
  W[i,i]<-0
}
colnames(W)<-state


