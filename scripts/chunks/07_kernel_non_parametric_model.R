# Source chunk: kernel non-parametric model
# Original Rmd lines: 432-444

#kernel非参数模型
for (s in state){
  temp_model<-npreg( y~ ysl+ysl_lag1+ysl_lag2+ysl_lag3+ysl_lag4+ysl_lag5+
                       y_lag1+y_lag2+y_lag3+y_lag4+y_lag5+
                       totalwagescapita_dif+emplvlcapita_dif+popestimate_dif+qtrly_estabs_count_dif
                     ,data=get(paste(s,"_training_data", sep = '')), gradients=TRUE, coefplot=TRUE, regtype="ll", bwmethod="cv.aic", residuals=TRUE, ckertype="gaussian", errors=gaussian)
  
assign(paste(s,"_model", sep=''),temp_model)
}


