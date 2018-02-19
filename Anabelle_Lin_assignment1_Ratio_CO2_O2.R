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

#----------------------------------------------------------------
#relationship of efficiency with temperature
#2.18.2018
#based on paper: Improved temperature response functions for models of Rubisco-limited photosynthesis

#this is the function of tau with temperature
#this is equation 6 in the paper
  R=8.31445  #molar gas constant, M2kgs-2K-1mol-1
  Temp.f = function(c,Ha){
    parameter = function(Tk){#Tk is leaf temeprature
      exp(c-Ha/(R*Tk))
    }
    parameter
  }
  
  #this is the function of leaf temeprature
  #all c and Ha value are from table 1 in the paper
  Vc.max = Temp.f(26.35,65.33)
  Vo.max = Temp.f(22.98,60.11)
  Ko = Temp.f(20.3,36.38)#MMOL
  Kc =  Temp.f(38.05,79.43)#umol
  tau = Vc.max(temp)*Ko(temp)/(Kc(temp)*Vo.max(temp))
  
#efficiency
#this is from Zhu's paper
efficiency = function (CO2,O2,temp){
  tau = Vc.max(temp)*Ko(temp)*1000/(Kc(temp)*Vo.max(temp))
  phi = O2/(CO2*tau) #this is the ratio of RuBP oxygenation to carboxylation
  dpr = 1-3*(1-0.5*phi)/(3+3.5*phi)  #maximal energy conversion efficiency
  dpr
}
leaf.temp=c(1:25)
effici = efficiency(0.0004,0.21,leaf.temp)
plot(leaf.temp,effici,type="l")
