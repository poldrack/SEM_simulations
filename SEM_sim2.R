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

data2=subset(data_full,select=c('vr2dr_totalraw','ssp_totalraw',
                                'ds_totalraw','mr_totalraw','voc_totalraw',
                                'lns_totalraw',
                                'asrs_score','hopkins_globalseverity',
                                'bis_factor1_ci','bis_factor2_bi',
                                'dysfunc_total','func_total',
                                'mpq_score','scorei','scorev',
                                'bipollarii_sumscore','golden_sumscore',
                                'chaphypo_total','chapinf_total',
                                'chapsoc_total','chapphy_total',
                                'persistance','novelty','harmavoidance',
                                'reward_dependence','scap_dprime',
                                'vcap_dprime','bart_meanadjustedpumps',
                                'cpt_fa',
                                'sr_dprime',
                                'choose_hi_prob_mn','avoid_low_prob_mn',
                                'smnm_maint_dprime','smnm_manip_dprime',
                                'vmnm_maint_dprime','vmnm_manip_dprime'))


data2=as.data.frame(scale(data2[complete.cases(data2),],scale=TRUE,center=TRUE))


cfa_model='FA =~ bis_factor1_ci + bis_factor2_bi + dysfunc_total +  func_total + mpq_score + scorei + persistance + novelty
FB =~ ssp_totalraw + ds_totalraw + mr_totalraw + voc_totalraw + lns_totalraw + vcap_dprime + vmnm_maint_dprime + vmnm_manip_dprime
FC =~ hopkins_globalseverity + func_total + scorev + bipollarii_sumscore + golden_sumscore + chaphypo_total + persistance + novelty + harmavoidance
FD =~ vr2dr_totalraw + ssp_totalraw + mr_totalraw + scap_dprime + sr_dprime + smnm_maint_dprime + smnm_manip_dprime
FE =~ hopkins_globalseverity + bis_factor2_bi + scorei + bipollarii_sumscore + golden_sumscore + chaphypo_total + persistance
FF =~ chapinf_total + chapsoc_total + chapphy_total + reward_dependence
FG =~ choose_hi_prob_mn + avoid_low_prob_mn
FH =~ asrs_score + hopkins_globalseverity + bis_factor1_ci + bis_factor2_bi + persistance + bart_meanadjustedpumps
FI =~ scap_dprime + bart_meanadjustedpumps + cpt_fa + smnm_maint_dprime + vmnm_maint_dprime
'
fit <- cfa(cfa_model, data =data2)

cfa_model2='FA =~ bis_factor1_ci + bis_factor2_bi + dysfunc_total +  func_total + mpq_score + scorei + persistance + novelty
FB =~ ssp_totalraw + ds_totalraw + mr_totalraw + voc_totalraw + lns_totalraw + vcap_dprime + vmnm_maint_dprime + vmnm_manip_dprime
FC =~ hopkins_globalseverity + func_total + scorev + bipollarii_sumscore + golden_sumscore + chaphypo_total + persistance + novelty + harmavoidance
FD =~ vr2dr_totalraw + ssp_totalraw + mr_totalraw + scap_dprime + sr_dprime + smnm_maint_dprime + smnm_manip_dprime
FE =~ hopkins_globalseverity + bis_factor2_bi + scorei + bipollarii_sumscore + golden_sumscore + chaphypo_total + persistance
FF =~ chapinf_total + chapsoc_total + chapphy_total + reward_dependence
FG =~ choose_hi_prob_mn + avoid_low_prob_mn
FH =~ asrs_score + hopkins_globalseverity + bis_factor1_ci + bis_factor2_bi + persistance + bart_meanadjustedpumps
FI =~ bart_meanadjustedpumps + cpt_fa + smnm_maint_dprime + vmnm_maint_dprime 
'

fit2 <- cfa(cfa_model2, data =data2)

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
      a=anova(f,f2)
      
      c(meas['rmsea'],meas['bic2'],meas2['bic2'],a$"Pr(>Chisq)"[2])
    },
    error=function(cond) {
    			 cat('something went wrong\n')
			
			 return(c(NA,NA,NA,NA))
			 }
  )
}

# generate samples for missing data analysis




  nkill=round(ncol(data2)*(1-samp_prop))
  
#   if (samp_prop<1){ 
#     combs=combinations(ncol(data2),nkill)
#     combs=combs[sample(nrow(combs)),]
#     }  # doesn't work with this many variables
  
  nsamp_sub=round(nsamp/samp_prop)

    dsim=data2[sample.int(nrow(data2),nsamp_sub,replace=TRUE),] #generate(fit,n=nsamp_sub)
    if (samp_prop<1){
      idx=1
      for (j in 1:nsamp_sub) {
        dsim[j,sample.int(ncol(data2),nkill)]=NA
      }
    }
    # change impute to FALSE to see effects of no imputation
    cfaout=runcfa(dsim,cfa_model,cfa_model2,impute=imp)

output=c(samp_prop,simnum,cfaout)
if (imp==1) {outdir='outputs_amelia'} else {outdir='outputs_fiml'}

write.table(output,file=sprintf('%s/sim_%0.3f_%d_%d.txt',outdir,samp_prop,nsamp,simnum),row.names=FALSE,col.names=FALSE,quote=FALSE)
