### simulate form-based sampling

import numpy

nvars=70
nforms=10
nvarsperform=nvars/nforms
nformspersubject=2

target_N=200
formreps=200

def make_random_forms(nvars,nforms,shuffle_forms=True):
    nvarsperform=nvars/nforms
    v=[]
    for i in range(nforms):
        for j in range(nvarsperform):
            v.append(i)
    v=numpy.array(v)
    if shuffle_forms:
        numpy.random.shuffle(v)
    return v
    
nsets=target_N/Nperform



def sample_forms(nvars,nforms,nformspersubject,formreps=1,shuffle_forms=True):
    covmatrix=numpy.zeros((nvars,nvars))
    covmatrix_triu=numpy.triu_indices(covmatrix.shape[0],1)

    subs=0
    #while subs < target_N*choose(nforms,nformspersubject):
    while numpy.min(covmatrix[covmatrix_triu])<target_N:

        forms=make_random_forms(nvars,nforms,shuffle_forms=shuffle_forms)
        for formrep in range(formreps):
            for sub_forms in itertools.combinations(range(nforms),nformspersubject):
                for i in range(len(sub_forms)):
                    for j in range(i,len(sub_forms)):
                        if not i==j:
                            subs+=1
                            itasks=[x for x in range(nvars) if forms[x]==sub_forms[i]]
                            jtasks=[x for x in range(nvars) if forms[x]==sub_forms[j]]
                            alltasks=itasks+jtasks
                            alltasks.sort()
                            for it in range(len(alltasks)):
                                for jt in range(it,len(alltasks)):
                                    if it!=jt:
                                        covmatrix[alltasks[it],alltasks[jt]] +=1
    return covmatrix

covmat_shuf=sample_forms(nvars,nforms,nformspersubject,formreps=1,shuffle_forms=True)
covmat_noshuf=sample_forms(nvars,nforms,nformspersubject,formreps=formreps,shuffle_forms=True)
plt.figure(figsize=(10,10))
plt.subplot(2,2,1)
plt.imshow(covmat_shuf,cmap='gray')
plt.colorbar()
plt.title('shuffled form sets')

plt.subplot(2,2,2)
plt.imshow(covmat_noshuf,cmap='gray')
plt.colorbar()
plt.title('single form set')

plt.subplot(2,2,3)
plt.hist(covmat_shuf[covmatrix_triu],100)
plt.title('pairwise sampling frequency')

plt.subplot(2,2,4)
plt.hist(covmat_noshuf[covmatrix_triu],100)
plt.title('pairwise sampling frequency')

plt.savefig('formsims.pdf')