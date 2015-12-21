import os,sys
import numpy
import glob


indir='outputs_fiml'

infiles=glob.glob(os.path.join(indir,'*'))
infiles.sort()

outfile=indir+'.txt'
assert not os.path.exists(outfile)
f=open(outfile,'w')

for i in infiles:
    l=[x.strip() for x in open(i).readlines()]
    f.write('%s\n'%'\t'.join(l))
f.close()
