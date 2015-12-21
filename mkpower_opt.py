f=open('runjobs_power_opt_noimp.sh','w')

m=0
n=100
for i in range(500):
  for j in range(3,7):
        f.write('Rscript SEM_sim4.R %d %d %d %d\n'%(j,i,m,n))
f.close()


