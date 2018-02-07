#Purpose:calculate CO2/O2 ratio in plant

#pick one (this is from Zhu's paper Fig.3)
  #present
  CO2<-0.0004 
  #past  #220ppm
  CO2<-0.00022
  #future   #700ppm
  CO2<-0.0007

# step 1:get into plant (physically diffusion)
O2<-0.21  # keep this value
ratio1<-O2/CO2

# step 2:water dissolving
alpha<-25  #This is the CO2/O2 Solubility ratio at 25 degree
ratio2<-ratio1*1/alpha  

# step 3:
tau<-75  #This is the CO2/O2 Specificity ratio 
ratio3<-tau/ratio2
ratio3

#results:
#past     ratio3:1.964286
#future    ratio3:6.25