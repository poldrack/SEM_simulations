### simulate form-based sampling

import numpy
import itertools
import matplotlib.pyplot as plt

nvars=70
nforms=10
nvarsperform=nvars/nforms
nformspersubject=2

target_N=200
formreps=200

# simulate times for each measure, following a truncated exponential
def mk_vartimes(min_time=2,max_time=20,mean_time=5,time_tolerance=1):
    vartimes=numpy.zeros(nvars)
    while numpy.abs(numpy.mean(vartimes)-mean_time)>time_tolerance:
        vartimes=numpy.random.exponential(numpy.ones(nvars)*(mean_time))
        vartimes[vartimes<min_time]=min_time
        vartimes[vartimes>max_time]=max_time
    return vartimes

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

def makeformlist(nvars,nforms):
    nvarsperform=nvars/nforms
    v=[]
    for j in range(nvarsperform):
        for i in range(nforms):
            v.append(i)
    v=numpy.array(v)
    return v
    
def make_random_forms_timed(nvars,nforms,vartimes):
    assert len(vartimes)==nvars
    nvarsperform=nvars/nforms
    v=makeformlist(nvars,nforms)
    formtime=numpy.zeros(nforms)
    formtime[0]=100000
    formtime_tolerance=10
      
    # sort vartimes and then randomize within form
    while (numpy.max(formtime) - numpy.min(formtime))>formtime_tolerance:
        vt_idx=numpy.argsort(vartimes)
        
        vt_idx_shuffled=vt_idx.copy()
        
        for f in range(nforms):
            start_idx=f*nvarsperform
            end_idx=(f+1)*nvarsperform
            tmp=vt_idx[start_idx:end_idx].copy()
            numpy.random.shuffle(tmp)
            vt_idx_shuffled[start_idx:end_idx]=tmp
        vs=v[vt_idx_shuffled]
        formtime=numpy.zeros(nforms)
        for f in range(nforms):
            formtime[f]=numpy.sum(vartimes[vs==f])
        #print (numpy.max(formtime) - numpy.min(formtime))
    return vs,formtime

def sample_forms(nvars,nforms,nformspersubject,formreps=1,shuffle_forms=True):
    covmatrix=numpy.zeros((nvars,nvars))
    covmatrix_triu=numpy.triu_indices(covmatrix.shape[0],1)
    vartimes=mk_vartimes()
    
    subs=0
    setnum=1
    #while subs < target_N*choose(nforms,nformspersubject):
    while numpy.min(covmatrix[covmatrix_triu])<target_N:
        print 'set %d'%setnum
        forms,formtimes=make_random_forms_timed(nvars,nforms,vartimes)
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
        setnum+=1
    return covmatrix

covmat_shuf=sample_forms(nvars,nforms,nformspersubject,formreps=1,shuffle_forms=True)
covmatrix_triu=numpy.triu_indices(covmat_shuf.shape[0],1)
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

plt.savefig('formsims_timed.pdf')