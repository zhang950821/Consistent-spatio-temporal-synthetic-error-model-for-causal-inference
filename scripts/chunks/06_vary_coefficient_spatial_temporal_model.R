# Source chunk: vary-coefficient spatial temporal model
# Original Rmd lines: 417-430



#变系数半参数模型
Alabama_model<-gam(y~ s(totalwagescapita_dif, by=ysl)+s(totalwagescapita_dif, by=ysl_lag1)+s(totalwagescapita_dif, by=ysl_lag2)+s(totalwagescapita_dif, by=ysl_lag3), data=Alabama_training_data)



Alabama_model<-gam(y~ s(ysl)+ysl_lag1+ysl_lag2+ysl_lag3+ysl_lag4+ysl_lag5+
                       y_lag1+y_lag2+y_lag3+y_lag4+y_lag5+
                       totalwagescapita_dif+emplvlcapita_dif+popestimate_dif+qtrly_estabs_count_dif
                     ,data=Alabama_training_data, gradients=TRUE, coefplot=TRUE, regtype="ll", bwmethod="cv.aic", residuals=TRUE, ckertype="gaussian", errors=gaussian)

