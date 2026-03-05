# Fouride PBPK model by Jean et al. (2018)
# Model template simulations 
# Author: Paul Schlosser, U.S. EPA, March 2026

# Set working directory to the directory containing this file.
script.dir = dirname(sys.frame(1)$ofile)
setwd(script.dir)

# Load functions to run the PBPK model template.
source("run_template_model.R")

times <- 0:251 # days
out <- PBPK_run(model.param.filename = "F_template_parameters_Model.xlsx",
                    model.param.sheetname = "Human", 
                    exposure.param.filename = "F_template_parameters_Exposure.xlsx", 
                    exposure.param.sheetname = "Four_year_old", data.times=times*24)

 par(mfrow=c(1,2), mar=c(3,3,0,0), oma=c(1,0,1,1), mgp=c(1.5,0.5,0))
  # Arterial blood
  plot(times, out$C_art, type="l", xlab="Time (days)", xlim=c(0,251), xaxp=c(0,250,5), 
       ylab="Arterial blood (mg/L)")

  
  # Urinary flouride
  plot(d_200$time_blood_data, d_200$blood_data, pch=19, col=pub.col, 
       log="y", xlab="Time (hr)", xlim=c(0,24), xaxp=c(0,24,6), 
       ylab="Concentration (mg/L)", ylim=c(0.001, 1000))
  lines(times, C_art[,2], col = templ.col5.1, lty = templ.lty, lwd = 3)
  lines(times, C_fat[,2], col = templ.col5.2, lty = templ.lty, lwd = 3)
  points(d_200$time_fat_data, d_200$fat_data, pch = 17, col = pub.col)
  lines(d_200$time_blood_sim, d_200$blood_sim, col=pub.col5.1, lty=pub.lty, lwd=2)
  lines(d_200$time_fat_sim, d_200$fat_sim, col=pub.col5.2, lty="dotdash", lwd=2)
  title("200 ppm", line = -1.25)
  