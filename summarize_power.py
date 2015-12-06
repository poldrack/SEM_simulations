# load results from simulations run on wrangler

from __future__ import division
import os,glob
import numpy
import seaborn as sns
import matplotlib.pyplot as plt


# amelia results
basedir='/Users/poldrack/code/SEM_simulations/outputs_fiml'

samps=[400,500,600,700,800]
try:
    del data
except:
    pass

for l in samps:
    files=glob.glob(os.path.join(basedir,'sim_0.100_%d_*txt'%l))
    files.sort()
    print '%0.3f: found %d files'%(l,len(files))
    for f in files:
        d=numpy.genfromtxt(f)
        d=d[:5]

        try:
            data=numpy.vstack((data,numpy.hstack((d,l))))
        except:
            data=numpy.hstack((d,l))





# outputs: RMSEA, BIC diff, x2diff, pval
RMSEA=numpy.zeros(len(samps))
sigp=numpy.zeros(len(samps))
sigp2=numpy.zeros(len(samps))
pgood=numpy.zeros(len(samps))
pnonzero=numpy.zeros(len(samps))
sumall=numpy.sum(data,1)
for i in range(len(samps)):
    l=samps[i]
    s=data[:,5]==l
    n=~numpy.isnan(sumall)
    goods=s*n
    nz=s*n*data[:,2]>0.0
    pgood[i]=numpy.sum(goods)/numpy.sum(s)
    pnonzero[i]=numpy.sum(data[goods,2]>0.0)/numpy.sum(goods)
    RMSEA[i]=numpy.median(data[nz,2])
    sigp[i]=numpy.mean(data[goods,3]<0.05)
    sigp2[i]=numpy.mean(data[goods,4]<0.05)




# make figures
plt.figure(figsize=(8,6))
plt.subplot(2,2,1)
plt.plot(samps,RMSEA)
plt.xlabel('Proportion of measures present')
plt.ylabel('RMSEA')
plt.title('A) Model goodness of fit')

plt.subplot(2,2,2)
plt.plot(samps,sigp)
plt.plot(samps,sigp2)
plt.legend(['model2 (hard)','model3 (easy)'])
plt.xlabel('Proportion of measures present')
plt.ylabel('Proportion of significant model comparisons')
plt.title('B) Power to detect correct model')
plt.axis([400,800,0.0,1.05])

plt.subplot(2,2,3)
plt.plot(samps,pgood)
plt.xlabel('Proportion of measures present')
plt.ylabel('Proportion of successful model fits')
plt.title('C) Success of model fitting')
plt.axis([400,800,0,1])

plt.subplot(2,2,4)
plt.plot(samps,pnonzero)
plt.xlabel('Proportion of measures present')
plt.ylabel('P(nonzero RMSEA|convergence')
plt.title('D) Nonzero RMSEA values')
plt.axis([400,800,0.0,1])

plt.tight_layout()
plt.savefig('results_power.pdf')
