# Fouride PBPK model by Jean et al. (2018)
# Model template simulations 
# Author: Paul Schlosser, U.S. EPA, March 2026

# Set working directory to the directory containing this file.
script.dir = dirname(sys.frame(1)$ofile)
setwd(script.dir)

# Load functions to run the PBPK model template.
source("run_template_model.R")

times <- seq(0,91,by=0.25) # days
out <- PBPK_run(model.param.filename = "F_template_parameters_Model.xlsx",
                    model.param.sheetname = "Human4yo", 
                    exposure.param.filename = "F_template_parameters_Exposure.xlsx", 
                    exposure.param.sheetname = "Four_year_old", data.times=times*24)

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