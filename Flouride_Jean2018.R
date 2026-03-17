# Fouride PBPK model by Jean et al. (2018)
# Dedicated model version simulations 
# Author: Paul Schlosser, U.S. EPA, March 2026

## Load libraries and essential MCSimMod and PBPK_run() functions.
library(readxl) # Used to import parameter values
library(MCSimMod) # Load MCSimMod

# Set working directory to the directory containing this file.
script.dir = dirname(sys.frame(1)$ofile)
setwd(script.dir)

Flouride <- createModel(mName="Flouride", verboseOutput=FALSE) #TRUE)
Flouride$loadModel()
Flouride$updateParms()

# Load functions to run the PBPK model template.
sim_days = 91
times <- seq(0,sim_days,by=0.1)*24 # simulation days

BW = 16; age = 4; ht = 103 # BW(kg), age(y), height(cm)
Flouride$parms["BW0"] = BW
Forc <- list(cbind(times=c(0,sim_days*24), BW_in=c(BW,BW)),
             cbind(times=c(0,sim_days*24), age_in=c(age,age)),
             cbind(times=c(0,sim_days*24), height_in=c(ht,ht))
)

# Exposures
water_conc = 0.7 # mg/L F concentration in water
water_ing = 0.442 # L/d water ingestion (volume of water consumed)
soil_ing = 0.00119 # mg/kg/d F intake by soil ingestion
diet_ing = 0.021 # mg/kg/d F intake in diet
paste_ing = 0.04 # mg/kg/d F intake in toothpaste
exp_days = 30

water_in = water_ing*water_conc*0.83/24 # (L/d) x (mg/L) * bio-availability / (24 h/d) = mg/h
soil_in = soil_ing*0.4*BW/24 # (mg/kg/d) x bio-availability x BW(kg) / (24 h/d) = mg/h
diet_in = diet_ing*0.4*BW/24 # (mg/kg/d) x bio-availability x BW(kg) / (24 h/d) = mg/h
toothpaste = paste_ing*1*BW/24 # (mg/kg/d) x bio-availability x BW(kg) / (24 h/d) = mg/h
air_in = 1e-5*1*BW/24  # (mg/kg/d) x bio-availability x BW(kg) / (24 h/d) = mg/h

Flouride$Y0["R_0Li"] = water_in + soil_in + diet_in + toothpaste + air_in
dose_end = data.frame(var=c("R_0Li"), time=exp_days*24, value=0.0, method="rep")

out <- as.data.frame(Flouride$runModel(times=times, forcings=Forc, events=list(data=dose_end), 
                                       rtol=1e-10, atol=1e-10, method="lsoda"))

#Jean = cbind(read_excel("Data/Flouride/Model_Fluoride_Child_90 daysSim_QA-copy_02-25-2026.xlsx", 
#             range="B38:B86439"),
#             read_excel("Data/Flouride/Model_Fluoride_Child_90 daysSim_QA-copy_02-25-2026.xlsx", 
#                     range="D38:D86439"),
#             read_excel("Data/Flouride/Model_Fluoride_Child_90 daysSim_QA-copy_02-25-2026.xlsx", 
#                      range="AM38:AM86439"),
#             read_excel("Data/Flouride/Model_Fluoride_Child_90 daysSim_QA-copy_02-25-2026.xlsx", 
#                        range="AE38:AE86439"))
#colnames(Jean)=c("time","Ca","Cu","CL_BoNet")
Jean=read.csv("Data/Flouride/Jean_2018_results.csv")

par(mfrow=c(1,2), mar=c(3,3,0,0), oma=c(1,0,1,1), mgp=c(1.5,0.5,0))
  # Arterial blood
  plot(out$time/24, out$C_Art, type="l", xlab="Time (days)", xlim=c(0,91), xaxp=c(1,91,6), 
       ylab="Arterial blood (mg/L)", ylim=c(0,0.015), yaxp=c(0,0.015,5)) # 
  lines(Jean$time/24,Jean$Ca, lty="dashed", lwd=3)
  
  # Urinary flouride
  plot(out$time/24, out$R_KiU*24/(0.03*Flouride$parms[["BW0"]]), type="l", 
       xlab="Time (days)", xlim=c(1,91), xaxp=c(1,91,6), 
       ylab="Urinary flouride (mg/L)", ylim=c(0, 1.4), yaxp=c(0,1.4,7))
  lines(Jean$time/24,Jean$Cu,, lty="dashed", lwd=3)
  
  # Bone "mineral" clearance rate
  plot(out$time/24, out$R_BoMiNet, type="l", xlab="Time (days)", xlim=c(0,91), xaxp=c(1,91,6), 
       ylab="Net CL to bone mineral (mg/h)")
  lines(Jean$time/24,Jean$CL_BoNet, lty="dashed", lwd=3)
  
  # Simulation without stopping exposure
  out2 <- as.data.frame(Flouride$runModel(times=times, forcings=Forc, rtol=1e-10, 
                                          atol=1e-10, method="lsoda"))
  plot(times/24, out2$C_Art, type="l", xlab="Time (days)", xlim=c(0,91), 
       xaxp=c(1,91,6), ylab="Arterial blood with continuous dosing (mg/L)")
  
  