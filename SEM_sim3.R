# v3: generate data using boostrap sampling
# using forms idea from Dave Mackinnon
# for 36 variables, use 9 forms with 4 measures each

args=commandArgs(trailingOnly=TRUE)
nforms_per=as.numeric(args[1])
simnum=as.numeric(args[2])
imp=as.numeric(args[3])
minsize=as.numeric(args[4])


if (is.na(minsize)) {
   cat('no command line args given - using canned values\n')
   samp_prop=0.07
   simnum=1
   imp=0
   nforms_per=2
}

library(lavaan)
#library(matrixcalc)
#library(gtools)

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
#fit <- cfa(cfa_model, data =data2)
# semPaths(fit,nCharNodes=0,layout='tree2',rotation=4,sizeMan2 = 1,sizeLat2=3)


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
#fit2 <- cfa(cfa_model2, data =data2)

cfa_model3='FA =~ bis_factor1_ci + bis_factor2_bi + dysfunc_total +  func_total + mpq_score + scorei + persistance + novelty
FB =~ ssp_totalraw + ds_totalraw + mr_totalraw + voc_totalraw + lns_totalraw + vcap_dprime + vmnm_maint_dprime 
+ vmnm_manip_dprime
FC =~ hopkins_globalseverity + func_total + scorev + bipollarii_sumscore + golden_sumscore + chaphypo_total 
+ persistance + novelty + harmavoidance
FD =~ vr2dr_totalraw + ssp_totalraw + mr_totalraw + scap_dprime + sr_dprime + smnm_maint_dprime + smnm_manip_dprime
FE =~ hopkins_globalseverity + bis_factor2_bi + scorei + bipollarii_sumscore + golden_sumscore + chaphypo_total 
+ persistance
FF =~ chapinf_total + chapsoc_total + chapphy_total + reward_dependence
FG =~ choose_hi_prob_mn + avoid_low_prob_mn
FH =~ asrs_score + hopkins_globalseverity + bis_factor1_ci + bis_factor2_bi + persistance + bart_meanadjustedpumps + cpt_fa 
+ smnm_maint_dprime + vmnm_maint_dprime 
'

#fit3 <- cfa(cfa_model3, data =data2)

# create forms
nforms=12
forms=c()
for (i in 1:nforms) {
  for (j in 1:round(36/nforms)) {
    forms=c(forms,i)
  }
}
forms=sample(forms)

runcfa = function(data,model,model2,model3,impute=0,ameliaridge=0.05,missing='fiml',ridge=0.1,scale=TRUE) {
  out=tryCatch(
    {
      if (impute==1) {
        am=amelia(data,m=1,empri=ameliaridge*nrow(data))
        data=am$imputations[[1]]
      }
      if (scale) {
        data=as.data.frame(scale(data,center = TRUE,scale = TRUE))
      }
      f=cfa(model,data=data,ridge=ridge,missing=missing,zero.add=c(0.1,0.1))
      f2=cfa(model2,data=data,ridge=ridge,missing=missing,zero.add=c(0.1,0.1))
      f3=cfa(model3,data=data,ridge=ridge,missing=missing,zero.add=c(0.1,0.1))
      meas=fitmeasures(f,fit.measures='all')
      a=anova(f,f2)
      a2=anova(f2,f3)
      
      c(meas['rmsea'],a$"Pr(>Chisq)"[2],a2$"Pr(>Chisq)"[2])
    },
    error=function(cond) {
    			 cat('something went wrong\n')
			
			 return(c(NA,NA,NA,NA))
			 }
  )
}

# generate samples for missing data analysis

sampdata_forms=function(d,ncombos,nforms,samp_prob) {
  nforms_per=round(nforms*samp_prob)
  nsamp_sub=round(ncombos*(nforms*(nforms-1)/2))
  
  dsim_samp=d[sample.int(nrow(d),nsamp_sub,replace=TRUE),] #generate(fit,n=nsamp_sub)
  dsim=dsim_samp
  dsim[,]=NA
  formnums=c()
  
  idx=1
  for (z in 1:ncombos) {
    for (i in 1:nforms) {
      for (j in i:nforms) {
          if (i!=j) {
            formnums=rbind(formnums,c(i,j)) 
            dsim[idx,which(forms==i)]=dsim_samp[idx,which(forms==i)]
            dsim[idx,which(forms==j)]=dsim_samp[idx,which(forms==j)]
            idx=idx+1
          }
      }
    }
  }
  return(dsim)
}


sampdata_random=function(d,nsamp_sub,samp_prop) {
  # sample variables totally at random
  nkill=round(ncol(d)*(1-samp_prop))
  
  #   if (samp_prop<1){ 
  #     combs=combinations(ncol(data2),nkill)
  #     combs=combs[sample(nrow(combs)),]
  #     }  # doesn't work with this many variables
  
  dsim=d[sample.int(nrow(d),nsamp_sub,replace=TRUE),] #generate(fit,n=nsamp_sub)
  if (samp_prop<1){
    idx=1
    for (j in 1:nsamp_sub) {
      dsim[j,sample.int(ncol(d),nkill)]=NA
    }
  }
  
  return(dsim)
}

jointhist=function(d){
  jhist=c()
  for (i in 1:ncol(d)) {
    for (j in i:ncol(d)) {
      if (i!=j) {
        jhist=rbind(jhist,c(i,j,sum(!is.na(d[,i]*d[,j]))))
      }
    }
  }
  return(jhist)
}

sampdata_optimal=function(d,nsamp_sub,samp_prop) {

  dsim=d[sample.int(nrow(d),nsamp_sub,replace=TRUE),] #generate(fit,n=nsamp_sub)
  dsim_samp=dsim
  dsim_samp[,]=NA
  
  if (samp_prop<1){
    for (j in 1:nsamp_sub) {
      jhist=jointhist(dsim_samp)
      idx=sort(jhist[,3],index.return=TRUE)$ix
      jhist=jhist[idx,]
      # if there are zeros, randomize their order
      # to prevent different sampling across tests
      if (sum(jhist[,3]==0)>0) {
        idx2=which(jhist[,3]==0)
        idx2_rand=sample(idx2)
        jhist[idx2_rand,]=jhist[idx2,]
      }
      v=unique(as.vector(t(jhist[,1:2])))
      
      for (i in 1:round(samp_prop*ncol(d))) {
        dsim_samp[j,v[i]]=dsim[j,v[i]]
      }
      
    }
  }
  
  return(dsim_samp)
}



dsim_forms=sampdata_forms(data2,minsize,nforms,nforms_per)
dsim_opt=sampdata_optimal(data2,nrow(dsim_forms),2/9)

eig_forms=eigen(cov(dsim_forms,use='pairwise.complete'))$values
eig_opt=eigen(cov(dsim_opt,use='pairwise.complete'))$values

badeig_forms=(sum(is.na(eig_forms))+sum(eig_forms<=0))/length(eig_forms)
badeig_opt=(sum(is.na(eig_opt))+sum(eig_opt<=0))/length(eig_opt)

cfaout_forms=runcfa(dsim_forms,cfa_model,cfa_model2,cfa_model3,impute=imp)
cfaout_opt=runcfa(dsim_opt,cfa_model,cfa_model2,cfa_model3,impute=imp)

output=c(nforms_per,minsize,simnum,cfaout_forms,cfaout_opt,badeig_forms,badeig_opt)
if (imp==1) {outdir='outputs_amelia'} else {outdir='outputs_fiml'}

write.table(output,file=sprintf('%s/sim_%d_%d_%d.txt',outdir,nforms_per,minsize,simnum),row.names=FALSE,col.names=FALSE,quote=FALSE)
