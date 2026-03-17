# Fouride PBPK model by Jean et al. (2018)
# Dedicated model version simulations 
# Author: Paul Schlosser, U.S. EPA, March 2026

## Load libraries and essential MCSimMod and PBPK_run() functions.
library(readxl) # Used to import parameter values
library(MCSimMod) # Load MCSimMod

# Set working directory to the directory containing this file.
script.dir = dirname(sys.frame(1)$ofile)
setwd(script.dir)

Flouride <- createModel(mName="Flouride", verboseOutput=TRUE)
Flouride$loadModel()
Flouride$updateParms()

# Load functions to run the PBPK model template.
days <- seq(0,91,by=0.25) # simulation days

BW = 16; age = 4; ht = 103 # BW(kg), age(y), height(cm)
Flouride$parms["BW0"] = BW
Forc <- list(cbind(times=c(0,91), BW_in=c(BW,BW)),
             cbind(times=c(0,91), age_in=c(age,age)),
             cbind(times=c(0,91), height_in=c(ht,ht))
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

Flouride$Y0["R_0Li"] = water_ing + soil_ing + diet_ing + paste_ing + air_in
dose_end = data.frame(var=c("R_0Li"), time=exp_days*24, value=0, method="rep")

out <- Flouride$runModel(times=days*24, forcings = Forc, events=list(data=dose_end),
                         rtol=1e-8, atol=1e-8, method="lsoda")

Jean = cbind(read_excel("Data/Flouride/Model Fluoride_Child_90 daysSim_QA-copy_02-25-2026.xlsx", 
             range="A39:A86439"),
             read_excel("Data/Flouride/Model Fluoride_Child_90 daysSim_QA-copy_02-25-2026.xlsx", 
                     range="D39:D86439"),
             read_excel("Data/Flouride/Model Fluoride_Child_90 daysSim_QA-copy_02-25-2026.xlsx", 
                      range="AM39:AM86439"))
colnames(Jean)=c("time","Ca","Cu")

par(mfrow=c(1,2), mar=c(3,3,0,0), oma=c(1,0,1,1), mgp=c(1.5,0.5,0))
  # Arterial blood
  plot(times, out$C_art, type="l", xlab="Time (days)", xlim=c(0,91), xaxp=c(1,91,6), 
       ylab="Arterial blood (mg/L)", ylim=c(0,0.015), yaxp=c(0,0.015,5))
  lines(Jean$time,Jean$Ca, lty="dashed", lwd=3)
  
  # Urinary flouride
  plot(times, out$R_met_1st_om*24/(0.03*template$parms[["BW"]]), type="l", 
       xlab="Time (days)", xlim=c(1,91), xaxp=c(1,91,6), 
       ylab="Urinary flouride (mg/L)", ylim=c(0, 1.4), yaxp=c(0,1.4,7))
  lines(Jean$time,Jean$Cu,, lty="dashed", lwd=3)
  
  out2 <- PBPK_run(model.param.filename = "F_template_parameters_Model.xlsx",
                  model.param.sheetname = "Human4yo", 
                  exposure.param.filename = "F_template_parameters_Exposure.xlsx", 
                  exposure.param.sheetname = "Four_year_old", data.times=times*24,
                  adj.parms = c(T_iv_infuse=91*24))
  plot(times, out2$C_art, type="l", xlab="Time (days)", xlim=c(0,91), xaxp=c(1,91,6), 
       ylab="Arterial blood (mg/L)")