# Source chunk: extract residuals for kernel
# Original Rmd lines: 446-479

for(s in state){
  assign(paste(s,"_residual", sep=''),residuals(get(paste(s,"_model", sep=""))))
}
residual_matrix<-NULL

for(s in state){
  residual_matrix<-cbind(residual_matrix,get(paste(s,"_residual", sep="")))
}

colnames(residual_matrix)<-state
cor(residual_matrix)

count=0
for(i in c(1:50)){
  for(j in c(1:50)){
    cor_result_temp<-cor.test(residual_matrix[,i], residual_matrix[,j])
    if(cor_result_temp$p.value<=0.05&cor_result_temp$p.value!=0){
          print(cor_result_temp$p.value)
      count=count+1
    }
  }
}
#检测residual的时间自相关性
par(mfrow=c(5,5))
for(i in c(1:25)){
  acf(residual_matrix[,i])
}


for(i in c(26:50)){
  acf(residual_matrix[,i])
}
