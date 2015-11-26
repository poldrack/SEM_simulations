# load results from simulations run on wrangler

from __future__ import division
import os,glob
import numpy
import seaborn as sns
import matplotlib.pyplot as plt

basedir='/Users/poldrack/code/SEM_simulations/outputs_imp'

levels=[0.5,0.25,0.2]

try:
    del data
except:
    pass

for l in levels:
    files=glob.glob(os.path.join(basedir,'sim_%0.3f_*txt'%l))
    files.sort()
    print '%0.3f: found %d files'%(l,len(files))
    for f in files:
        d=numpy.genfromtxt(f)
        try:
            data=numpy.vstack((data,d))
        except:
            data=d


# outputs: RMSEA, BIC diff, x2diff, pval
RMSEA_imp=numpy.zeros(len(levels))
BICdiff_imp=numpy.zeros(len(levels))
chi2diff_imp=numpy.zeros(len(levels))
sigp_imp=numpy.zeros(len(levels))
pgood_imp=numpy.zeros(len(levels))
pnonzero_imp=numpy.zeros(len(levels))

sumall=numpy.sum(data,1)
for i in range(len(levels)):
    l=levels[i]
    s=data[:,0]==l
    n=~numpy.isnan(sumall)
    goods=s*n
    pgood_imp[i]=numpy.sum(goods)/numpy.sum(s)
    pnonzero_imp[i]=numpy.sum(data[goods,2]>0.0)/numpy.sum(s)
    RMSEA_imp[i]=numpy.median(data[goods,2])
    BICdiff_imp[i]=numpy.median(data[goods,3])
    chi2diff_imp[i]=numpy.median(data[goods,4])
    sigp_imp[i]=numpy.mean(data[goods,5]<0.05)

basedir='/Users/poldrack/code/SEM_simulations/outputs_noimp'

levels=[1.0,0.5,0.25,0.2]

try:
    del data
except:
    pass

for l in levels:
    files=glob.glob(os.path.join(basedir,'sim_%0.3f_*txt'%l))
    files.sort()
    print '%0.3f: found %d files'%(l,len(files))
    for f in files:
        d=numpy.genfromtxt(f)
        try:
            data=numpy.vstack((data,d))
        except:
            data=d


# outputs: RMSEA, BIC diff, x2diff, pval
RMSEA=numpy.zeros(len(levels))
BICdiff=numpy.zeros(len(levels))
chi2diff=numpy.zeros(len(levels))
sigp=numpy.zeros(len(levels))
pgood=numpy.zeros(len(levels))
pnonzero=numpy.zeros(len(levels))
sumall=numpy.sum(data,1)
for i in range(len(levels)):
    l=levels[i]
    s=data[:,0]==l
    n=~numpy.isnan(sumall)
    goods=s*n
    pgood[i]=numpy.sum(goods)/numpy.sum(s)
    pnonzero[i]=numpy.sum(data[goods,2]>0.0)/numpy.sum(s)
    RMSEA[i]=numpy.median(data[goods,2])
    BICdiff[i]=numpy.median(data[goods,3])
    chi2diff[i]=numpy.median(data[goods,4])
    sigp[i]=numpy.mean(data[goods,5]<0.05)

# make figures
plt.figure(figsize=(8,6))
plt.subplot(2,2,1)
plt.plot(levels,RMSEA)
plt.plot(levels[1:],RMSEA_imp,color='red')
plt.legend(['FIML (no imputation)','imputation (MICE)'],loc=4)
plt.xlabel('Proportion of measures present')
plt.ylabel('RMSEA')
plt.title('A) Model goodness of fit')

plt.subplot(2,2,2)
plt.plot(levels,sigp)
plt.plot(levels[1:],sigp_imp,color='red')
plt.legend(['FIML (no imputation)','imputation (MICE)'],loc=4)
plt.xlabel('Proportion of measures present')
plt.ylabel('Proportion of significant model comparisons')
plt.title('B) Power to detect correct model')
plt.axis([0.2,1.0,0.5,1.05])

plt.subplot(2,2,3)
plt.plot(levels,pgood)
plt.plot(levels[1:],pgood_imp,color='red')
plt.legend(['FIML (no imputation)','imputation (MICE)'],loc=4)
plt.xlabel('Proportion of measures present')
plt.ylabel('Proportion of successful model fits')
plt.title('C) Success of model fitting')
plt.axis([0.2,1.0,0,1])

plt.subplot(2,2,4)
plt.plot(levels,pnonzero)
plt.plot(levels[1:],pnonzero_imp,color='red')
plt.legend(['FIML (no imputation)','imputation (MICE)'],loc=4)
plt.xlabel('Proportion of measures present')
plt.ylabel('Proportion of nonzero RMSEA values')
plt.title('D) Nonzero RMSEA values')
plt.axis([0.2,1.0,0,1])

plt.tight_layout()
plt.savefig('results.pdf')