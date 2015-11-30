# use simsem to generate simulated data
args=commandArgs(trailingOnly=TRUE)
samp_prop=as.numeric(args[1])
simnum=as.numeric(args[2])
imp=as.numeric(args[3])
nsamp=as.numeric(args[4])

if (is.na(nsamp)) {nsamp=400}

if (is.na(samp_prop)) {
   cat('no command line args given - using canned values\n')
   samp_prop=0.25
   simnum=1
   imp=1
}

library(lavaan)
library(gtools)
library(Amelia)
library(nonnest2)

c0=read.table('set1.csv',sep=',',header=TRUE,na.strings=c('NA','.'))

c1=read.table('set2.csv',sep=',',header=TRUE,na.strings=c('NA','.'))
data_full=cbind(c0,c1)


# subset variables to create clear structure
# data2=subset(data_full,select=c('ts_interference','ant_conflict_rt_effect',
#                                 'ddt_total_k','persistance','scorev',
#                                 'func_total','scap_dprime','smnm_manip_dprime',
#                                 'vmnm_manip_dprime','dysfunc_total',
#                                 'bis_factor2_bi','scorei'))
# transform the skewed variables
#data2$ddt_total_k=log(data2$ddt_total_k)
#data2$dysfunc_total=log(data2$dysfunc_total)

data2=subset(data_full,select=c('ssp_totalraw','vr2dr_totalraw','mr_totalraw',
                              'vmnm_maint_dprime','vmnm_manip_dprime','vcap_dprime',
                               'bis_factor2_bi','chaphypo_total','bipollarii_sumscore','scorei',
                                'reward_dependence','chapsoc_total','chapphy_total'))


data2=data2[complete.cases(data2),]


cfa_model='FA =~ ssp_totalraw + vr2dr_totalraw + mr_totalraw + vmnm_maint_dprime + vmnm_manip_dprime + vcap_dprime
FB =~ bis_factor2_bi + chaphypo_total + bipollarii_sumscore + scorei
FC =~ reward_dependence + chapsoc_total + chapphy_total'

fit <- cfa(cfa_model, data =data2)

cfa_model2='FA =~ ssp_totalraw + vr2dr_totalraw + mr_totalraw + vmnm_maint_dprime + vmnm_manip_dprime 
FB =~ vcap_dprime + bis_factor2_bi + chaphypo_total + bipollarii_sumscore + scorei
FC =~ reward_dependence + chapsoc_total + chapphy_total'

fit2 <- cfa(cfa_model2, data =data2)
anova(fit,fit2)

runcfa = function(data,model,model2,impute=0,missing='fiml',ridge=1e-05,estimator='MLR',scale=TRUE) {
  out=tryCatch(
    {
      if (impute==1) {
        am=amelia(data,m=1)
        data=am$imputations[[1]]
      }
      if (scale) {
        data=as.data.frame(scale(data,center = TRUE,scale = TRUE))
      }
      f=cfa(model,data=data,ridge=ridge,missing=missing,estimator=estimator,zero.add=c(0.1,0.1))
      f2=cfa(model2,data=data,ridge=ridge,missing=missing,estimator=estimator,zero.add=c(0.1,0.1))
      meas=fitmeasures(f,fit.measures='all')
      meas2=fitmeasures(f2,fit.measures='all')
      v=vuongtest(f,f2)
      
      c(meas['rmsea'],meas['bic2'],meas2['bic2'],v$p_LRT$A)
    },
    error=function(cond) {
    			 cat('something went wrong\n')
			
			 return(c(NA,NA,NA,NA))
			 }
  )
}

# generate samples for missing data analysis




  nkill=round(ncol(data2)*(1-samp_prop))
  
  if (samp_prop<1){ 
    combs=combinations(ncol(data2),nkill)
    combs=combs[sample(nrow(combs)),]
    }
  nsamp_sub=round(nsamp/samp_prop)

    dsim=data2[sample.int(nrow(data2),nsamp_sub,replace=TRUE),] #generate(fit,n=nsamp_sub)
    if (samp_prop<1){
      idx=1
      for (j in 1:nsamp_sub) {
        dsim[j,combs[idx,]]=NA
        if (idx>=nrow(combs)) {
	   idx=1
    	   combs=combs[sample(nrow(combs)),]

	} else {idx=idx+1}
      }
    }
    # change impute to FALSE to see effects of no imputation
    cfaout=runcfa(dsim,cfa_model,cfa_model2,impute=imp)

output=c(samp_prop,simnum,cfaout)
if (imp==1) {outdir='outputs_amelia'} else {outdir='outputs_fiml'}

write.table(output,file=sprintf('%s/sim_%0.3f_%d_%d.txt',outdir,samp_prop,nsamp,simnum),row.names=FALSE,col.names=FALSE,quote=FALSE)
