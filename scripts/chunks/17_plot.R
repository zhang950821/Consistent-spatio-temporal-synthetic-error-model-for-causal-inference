# Source chunk: plot
# Original Rmd lines: 1493-1550

post_LST_lngdpcapita_series<-ts(post_LST_lngdpcapita, start=90)
post_LST_lngdpcapita_series<-ts(c(obs_kansas_lngdpcapita[30],as.numeric(post_LST_lngdpcapita_series)), start = 89)

post_se_lngdpcapita_series_plt<-ts(c(obs_kansas_lngdpcapita[30],as.numeric(post_se_lngdpcapita_series)), start = 89)


post_LSE_se_lngdpcapita_series_plt<-ts(c(obs_kansas_lngdpcapita[30],as.numeric(post_LSE_se_lngdpcapita_series)), start = 89)

ts.plot(obs_kansas_lngdpcapita, post_LST_lngdpcapita_series, post_se_lngdpcapita_series_plt,post_LSE_se_lngdpcapita_series_plt,
        col=c('black','red', 'blue', 'green'), ylab='Kansas ln(GDP) per capita')
legend("topleft", legend = c("Observed data", "LST Model", "STPSEM", 'NPSTSEM'), col=c('black','red', 'blue', 'green'), lty=1)




# ts.plot(obs_kansas_lngdpcapita,gpars=list(ylab='Kansas ln(GDP) per capita'))
# lines(post_se_lngdpcapita_series, col='blue')

#RSC
post_RSC_lngdpcapita_series<-ts(post_RSC_lngdpcapita, start=90)
post_RSC_lngdpcapita_series<-ts(c(obs_kansas_lngdpcapita[30],post_RSC_lngdpcapita), start = 89)


#mSC
post_mSC_lngdpcapita_series<-ts(post_mSC_lngdpcapita, start=90)
post_mSC_lngdpcapita_series<-ts(c(obs_kansas_lngdpcapita[30],post_mSC_lngdpcapita), start = 89)

#GSC
post_gsynth_lngdpcapita_series<-ts(post_gsynth_lngdpcapita, start=90)
post_gsynth_lngdpcapita_series<-ts(c(obs_kansas_lngdpcapita[30],post_gsynth_lngdpcapita), start = 89)

#PSTSEM vs SC
post_SC_lngdpcapita_series<-ts(post_SC_lngdpcapita, start=90)
post_SC_lngdpcapita_series<-ts(c(obs_kansas_lngdpcapita[30],post_SC_lngdpcapita), start = 89)

ts.plot(obs_kansas_lngdpcapita,post_SC_lngdpcapita_series, post_se_lngdpcapita_series_plt,
        col=c('black','red','blue'), ylab='Kansas ln(GDP) per capita')
legend("topleft", legend = c("Observed data", "Synthetic control", "STPSEM"), col=c('black','red','blue'), lty=1, cex = 0.5)


#NPSTSEM vs ASCM
post_ASCM_lngdpcapita_series<-ts(post_ASCM_lngdpcapita, start=90)
post_ASCM_lngdpcapita_series<-ts(c(obs_kansas_lngdpcapita[30],post_ASCM_lngdpcapita), start = 89)

post_ST_ASCM_lngdpcapita_series<-ts(post_ST_ASCM_lngdpcapita, start=90)
post_ST_ASCM_lngdpcapita_series<-ts(c(obs_kansas_lngdpcapita[30],post_ST_ASCM_lngdpcapita), start = 89)

ts.plot(obs_kansas_lngdpcapita,post_ASCM_lngdpcapita_series, post_LSE_se_lngdpcapita_series_plt,
        col=c('black','red','blue'), ylab='Kansas ln(GDP) per capita')
legend("topleft", legend = c("Observed data", "ST-Augmented Synthetic Control", "NPSTSEM"), col=c('black','red','blue'), lty=1, cex = 0.5)

#PSTSEM vs ASCM
ts.plot(obs_kansas_lngdpcapita,post_ASCM_lngdpcapita_series, post_se_lngdpcapita_series_plt,
        col=c('black','red','blue'), ylab='Kansas ln(GDP) per capita')
legend("topleft", legend = c("Observed data", "ST-Augmented Synthetic Control", "STPSEM"), col=c('black','red','blue'), lty=1, cex = 0.5)

