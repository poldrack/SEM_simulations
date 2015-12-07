# simulation of sampling for UH2 project

import numpy



nsubs=4000
nmeas=70
nsamp=12 # current method only works with even number of sampled measures

meas=numpy.arange(nmeas)
jhist=numpy.zeros((nmeas,nmeas))
jhist_opt=numpy.zeros((nmeas,nmeas))

def jhist_to_list_sorted(jh):
    output=numpy.zeros(( jh.shape[0]*(jh.shape[1]-1)/2 ,3))
    ctr=0
    for i in range(jh.shape[0]):
        for j in range(i,jh.shape[1]):
            if not i==j:
                output[ctr,:]=[i,j,jh[i,j]]
                ctr+=1
    idx=numpy.argsort(output[:,2])
    output=output[idx,:]
    return output.tolist()

def sample_lowfreq_items(jhlist,n):
    output=[]
    i=0
    # kludge - just get twice as many as needed
    while (len(output)<n):
        if not jhlist[i][0] in output:
            output.append(int(jhlist[i][0]))
        if not jhlist[i][1] in output:
            output.append(int(jhlist[i][1]))
        i+=1
    return output[:n]

for i in range(nsubs):
    numpy.random.shuffle(meas)
    msamp=meas[:nsamp].copy()
    for x in msamp:
        for y in msamp:
            jhist[x,y]+=1

    # get ordered joint hist and sample from bottom section
    jhlist=jhist_to_list_sorted(jhist_opt)
    msamp_opt=sample_lowfreq_items(jhlist,nsamp)
    assert len(msamp_opt)==nsamp
    for x in msamp_opt:
        for y in msamp_opt:
            jhist_opt[x,y]+=1

triu=numpy.triu_indices(nmeas,1)
print 'random:',numpy.min(jhist[triu]),numpy.max(jhist[triu])
print 'optimzed:',numpy.min(jhist_opt[triu]),numpy.max(jhist_opt[triu])

import matplotlib.pyplot as plt
h_orig=numpy.histogram(jhist[triu].ravel(),50)
h_opt=numpy.histogram(jhist_opt[triu].ravel(),50)

plt.plot((h_orig[1][:-1]+h_orig[1][1:])/2.,h_orig[0])
plt.plot((h_opt[1][:-1]+h_opt[1][1:])/2.,h_opt[0])
plt.xlabel('Frequency of variable pair')
plt.ylabel('Number of cells')
plt.legend(['Random sampling','Optimized sampling'])
plt.savefig('optsample.pdf')
