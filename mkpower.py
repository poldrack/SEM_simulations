f=open('runjobs_power_noimp.sh','w')

m=0
for i in range(500):
  for j in [0.06,0.07,0.1,0.16,0.25,0.5,1.0]:
    for n in [400,500,600,700,800]:
        f.write('Rscript SEM_sim2.R %f %d %d %d\n'%(j,i,m,n))
f.close()


