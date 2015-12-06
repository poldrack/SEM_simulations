# load results from simulations run on wrangler

from __future__ import division
import os,glob
import numpy
import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd

# amelia results
basedir='/corral-repl/utexas/poldracklab/data/SEM_simulations/outputs_fiml'


samps=[400,500,600,700,800]
sprops=[0.06,0.07,0.1,0.16,0.25,0.5,1.0]

try:
    data
except:
    for l in samps:
     for s in sprops:
        files=glob.glob(os.path.join(basedir,'sim_%0.3f_%d_*txt'%(s,l)))
        files.sort()
        print '%0.3f %0.3f: found %d files'%(l,s,len(files))
        for f in files:
            d=numpy.genfromtxt(f)
            d=d[:5]

            try:
                data=numpy.vstack((data,numpy.hstack((d,l))))
            except:
                data=numpy.hstack((d,l))





# outputs: RMSEA, BIC diff, x2diff, pval



sumall=numpy.sum(data,1)

df=pd.DataFrame(index=range(len(samps)*len(sprops)),columns=['N','numvars','pgood','pnonzero','RMSEA','sigp','sigp2'])

idx=0
for i in range(len(samps)):
  for j in range(len(sprops)):
    l=samps[i]
    p=sprops[j]
    smatch=data[:,5]==l
    pmatch=data[:,0]==p
    n=~numpy.isnan(sumall)
    goods=smatch*pmatch*n
    nz=smatch*n*pmatch*data[:,2]>0.0
    df.loc[idx].N=l
    df.loc[idx].numvars=round(36*p)
    df.loc[idx].pgood=numpy.sum(goods)/numpy.sum(smatch*pmatch)
    df.loc[idx].pnonzero=numpy.sum(data[goods,2]>0.0)/numpy.sum(goods)
    df.loc[idx].RMSEA=numpy.median(data[nz,2])
    df.loc[idx].sigp=numpy.mean(data[goods,3]<0.05)
    df.loc[idx].sigp2=numpy.mean(data[goods,4]<0.05)
    idx+=1


df=df[df.numvars !=2]

# make figures
plt.figure(figsize=(8,6))
plt.subplot(2,2,1)
sns.barplot(x='N',y='RMSEA',hue='numvars',data=df)
plt.xlabel('Base sample size')
plt.ylabel('RMSEA')
plt.title('A) Model goodness of fit')


plt.subplot(2,2,2)

sns.barplot(x='N',y='sigp',hue='numvars',data=df)

plt.xlabel('Base sample size')
plt.ylabel('Proportion of significant model comparisons')
plt.title('B) Power to detect correct model (close)')
plt.ylim((0,1))


plt.subplot(2,2,3)
sns.barplot(x='N',y='pgood',hue='numvars',data=df)

plt.xlabel('Base sample size')
plt.ylabel('Proportion of successful model fits')
plt.title('C) Success of model fitting')
plt.ylim((0,1))


plt.subplot(2,2,4)
ax=sns.barplot(x='N',y='pnonzero',hue='numvars',data=df)

plt.xlabel('Base sample size')
plt.ylabel('P(nonzero RMSEA|convergence')
plt.title('D) Nonzero RMSEA values')
plt.ylim((0,1))


plt.tight_layout()


plt.savefig('results_power.pdf')
