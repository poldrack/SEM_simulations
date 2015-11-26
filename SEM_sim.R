# use simsem to generate simulated data
args=commandArgs(trailingOnly=TRUE)
#samp_prop=as.numeric(args[1])
#simnum=as.numeric(args[2])
samp_prop=0.5
simnum=1

library(lavaan)
library(mice)
library(gtools)
library(Amelia)
c0=read.table('set1.csv',sep=',',header=TRUE,na.strings=c('NA','.'))

c1=read.table('set2.csv',sep=',',header=TRUE,na.strings=c('NA','.'))
data_full=cbind(c0,c1)


# subset variables to create clear structure
data2=subset(data_full,select=c('ts_interference','ant_conflict_rt_effect',
                                'ddt_total_k','persistance','scorev',
                                'func_total','scap_dprime','smnm_manip_dprime',
                                'vmnm_manip_dprime','dysfunc_total',
                                'bis_factor2_bi','scorei'))
data2=data2[complete.cases(data2),]


cfa_model='FA =~ ts_interference + ant_conflict_rt_effect + ddt_total_k
FB =~ persistance + scorev + func_total
FC =~ scap_dprime + smnm_manip_dprime + vmnm_manip_dprime
FD =~ dysfunc_total + bis_factor2_bi + scorei'
fit <- cfa(cfa_model, data =data2)

cfa_model2='FA =~ scap_dprime + smnm_manip_dprime + vmnm_manip_dprime + ts_interference + ant_conflict_rt_effect + ddt_total_k 
FB =~   dysfunc_total + bis_factor2_bi + scorei + persistance + scorev + func_total'
fit2 <- cfa(cfa_model2, data =data2)

runcfa = function(data,model,model2,impute=FALSE,missing='fiml',ridge=0.001,estimator='MLR',scale=FALSE) {
  out=tryCatch(
    {
      if (impute) {
        m=mice(data)
        data=complete(m)
      }
      if (scale) {
        data=as.data.frame(scale(data,center = TRUE,scale = TRUE))
      }
      fitted=cfa(cfa_model,data=data,ridge=ridge,missing=missing,estimator=estimator,zero.add=c(0.1,0.1))
      fitted2=cfa(cfa_model2,data=data,ridge=ridge,missing=missing,estimator=estimator,zero.add=c(0.1,0.1))
      meas=fitmeasures(fitted,fit.measures='all')
      meas2=fitmeasures(fitted2,fit.measures='all')
      a=anova(fitted,fitted2)
      c(meas['rmsea'],meas2['bic2']-meas['bic2'],a['Chisq diff'][2,1],a["Pr(>Chisq)"][2,1])
    },
    error=function(cond) {return(c(NA,NA,NA,NA))}
  )
}

# generate samples for missing data analysis

nsamp=400



  nkill=round(ncol(data2)*(1-samp_prop))
  
  if (samp_prop<1){ combs=combinations(ncol(data2),nkill)}
  nsamp_sub=round(nsamp/samp_prop)

    dsim=data2[sample.int(nrow(data2),nsamp_sub,replace=TRUE),] #generate(fit,n=nsamp_sub)
    if (samp_prop<1){
      idx=1
      for (j in 1:nsamp_sub) {
        dsim[j,combs[idx,]]=NA
        if (idx>=nrow(combs)) {idx=1} else {idx=idx+1}
      }
    }
    # change impute to FALSE to see effects of no imputation
    cfaout=runcfa(dsim,cfa_model,cfa_model2,impute=TRUE)

output=c(samp_prop,simnum,cfaout)
write.table(output,file=sprintf('outputs/sim_%0.3f_%d.txt',samp_prop,simnum),row.names=FALSE,col.names=FALSE,quote=FALSE)
