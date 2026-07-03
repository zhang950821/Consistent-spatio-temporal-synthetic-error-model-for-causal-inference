# Source chunk: interference test
# Original Rmd lines: 1661-1667

 windows(width = 10, height =10)
par(mfrow=c(5,5))
for (i in 1:25) {
  acf(pre_and_post_residual_matrix[,i])
}
