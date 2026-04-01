#------------------------------------------------------------------------------
# CF_Corley_scripts.R
#
# This file contains functions to recreate results for the Corley chloroform 
# experiments modeled by Sasso et. al. (2013).
#
# Author: Bidya Prasad, December 2021
# Revisions for use with MCSimMod: Pau Schlosser, Nov-Dec 2025
#------------------------------------------------------------------------------

# Set working directory to the directory containing this file.
script.dir=dirname(sys.frame(1)$ofile)
setwd(script.dir)

library(RColorBrewer)
library(readxl)

# Create model and load functions to (compile if needed and) run the PBPK model template.
source("run_template_model.R")

# Corley.chamber.plot contains the commands for the plots in the VOC manuscript submission. 
Corley.chamber.plot<-function(maketiff=FALSE, test_univ = FALSE){
  # Creates manuscript Figure 10 (mice) and Figure S-7 (rats)
  # Set input maketiff=TRUE to create tiff file.
  
  vcol=c("#440154FF", "#2A788EFF", "#7AD151FF") 
  vcol5=c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF") 
  vcol7=c("#440154FF", "#472F7DFF", "#39568CFF", "#2A788EFF", "#1F988BFF", 
            "#35B779FF", "#7AD151FF") # viridis(7, begin=0.0, end=0.8)
  
  pub.col <- vcol[2] # also pub.col5.2 <- vcol5[2]
  templ.col <- vcol[3] # also templ.col5.2 <- vcol5[5]
  pub.lty="dashed"; templ.lty="solid"; paper.col=vcol[2]; paper.lty="dashed"
  pub.col5.1=vcol5[1]; pub.col5.2=vcol5[2]; templ.col5.1=vcol5[4];
  templ.col5.2=vcol5[5]; pub.col7.1=vcol7[1]; pub.col7.2=vcol7[2]; pub.col7.3=vcol7[3]; 
  templ.col7.1=vcol7[5]; templ.col7.2=vcol7[6]; templ.col7.3=vcol7[7]
  
  par(mfrow=c(1,2), mar=c(2.5, 2.5, 1, 1), mgp=c(1.5, 0.4, 0), oma=c(4, 0, 0, 1))
  
  ####### Rat simulations & plot
  # Load outputs from the acslx Sasso model
  acslx=read_excel("Data/Data_CF/rat_closed_chamber.xlsx")
  # Load inhalation data
  data_rat=read_excel("Data/Data_CF/corley_1990_chamber_exp.xlsx", sheet="rat")
  
  plot(1,1, type="n", log="y", xlab="Time (hr)", ylab="Chamber Concentration (ppm)", 
       xlim=c(0, 5.5), ylim=c(10, 3000), cex.lab=1.25, yaxt="n", xaxt="n")
  axis(side=1, at=seq(0,5.5,0.5), labels=seq(0,5.5,0.5), cex.axis=1)
  axis(side=2, at=c(10,30,100,300,1000,3000), labels=c(10,30,100,300,1000,3000), cex.axis=1)

  # Simulates 3 rats exposed to chloroform via inhalation in a 9.1L closed  chamber
  conc.ppm <- c(103, 516, 929, 1291, 2581)
  BW <- c(0.226, 0.227, 0.237, 0.239, 0.218)
  for (i in 1:length(conc.ppm)) {
    points(data_rat$time_hr, data_rat[[paste0("exp",i,"_conc_ppm")]], pch=19, col=pub.col)
    out <- PBPK_run(model.param.filename="CF_template_parameters_Model.xlsx",
                    model.param.sheetname="Revised_rat_params",
                    exposure.param.filename="CF_template_parameters_Exposure.xlsx",
                    exposure.param.sheetname="Corley_rat",
                    adj.parms=c(Conc_init=conc.ppm[i], BW=BW[i]))
    lines(out$time, out$C_chppm, lwd=2, lty=templ.lty, col=templ.col) # Revised params/Template
    if (test_univ) {
      res <- PBPK_run(model.param.filename="CF_template_parameters_Model.xlsx",
                      model.param.sheetname="Revised_rat_params",
                      exposure.param.filename="CF_template_parameters_Exposure.xlsx",
                      exposure.param.sheetname="Corley_rat",
                      adj.parms=c(Conc_init=conc.ppm[i], BW=BW[i]), test_univ=TRUE)
      lines(res$time, res$C_chppm, lwd=2, lty="dotted", col=vcol[1]) # Revised-universal lung params/Template
    } else {
      lines(acslx$time, acslx[[paste0("exp",i,"_rev")]], lwd=2, lty=pub.lty, col=pub.col) # Revised params/Sasso code
      res <- PBPK_run(model.param.filename="CF_template_parameters_Model.xlsx",
                      model.param.sheetname="Corley_rat_params", 
                      exposure.param.filename="CF_template_parameters_Exposure.xlsx", 
                      exposure.param.sheetname="Corley_rat",
                      adj.parms=c(Conc_init=conc.ppm[i], BW=BW[i]))
      lines(res$time, res$C_chppm, lwd=2, lty=templ.lty, col=templ.col7.1) # Corley params/Template
    }
    lines(acslx$time, acslx[[paste0("exp",i,"_Cor")]], lwd=2, lty=pub.lty, col=pub.col5.2) # Corley params/Sasso code
  }
  title("Rat", line=-1.25)
  
  ########## Mouse simulations & plot
  # Load outputs from the acslx Sasso model
  acslx=read_excel("Data/Data_CF/mouse_closed_chamber.xlsx")
  # Load inhalation data
  data_mouse=read_excel("Data/Data_CF/corley_1990_chamber_exp.xlsx", sheet="mouse")
  
  plot(1,1, type="n", log="y", xlab="Time (hr)",cex.lab=1.25, yaxt="n", xaxt="n",  
       ylab="Chamber Concentration (ppm)", xlim=c(0, 5.5), ylim=c(10, 5000))
  axis(side=1, at=seq(0,5.5,0.5), labels=seq(0,5.5,0.5), cex.axis=1)
  axis(side=2, at=c(10,30,100,300,1000,3000), labels=c(10,30,100,300,1000,3000), cex.axis=1)
  
  # Simulates 15 mice exposed to chloroform via inhalation in a 9.1L closed chamber
  conc.ppm <- c(1000, 2500, 5000); BW <- c(0.028, 0.029, 0.029)
  for (i in 1:length(conc.ppm)){
    points(data_mouse$time_hr, data_mouse[[paste0("exp",i,"_conc_ppm")]], pch=19, col=pub.col)
    out <- PBPK_run(model.param.filename="CF_template_parameters_Model.xlsx",
                    model.param.sheetname="Revised_mouse_params",
                    exposure.param.filename="CF_template_parameters_Exposure.xlsx",
                    exposure.param.sheetname="Corley_mouse",
                    adj.parms=c(Conc_init=conc.ppm[i], BW=BW[i]))
    lines(out$time, out$C_chppm, lwd=2, lty=templ.lty, col=templ.col) # Revised params/Template
    if (test_univ) {
      res <- PBPK_run(model.param.filename="CF_template_parameters_Model.xlsx",
                      model.param.sheetname="Revised_mouse_params",
                      exposure.param.filename="CF_template_parameters_Exposure.xlsx",
                      exposure.param.sheetname="Corley_mouse",
                      adj.parms=c(Conc_init=conc.ppm[i], BW=BW[i]), test_univ=TRUE)
      lines(res$time, res$C_chppm, lwd=2, lty="dotted", col=vcol[1]) # Revised-universal lung params/Template
    } else {
      lines(acslx$time, acslx[[paste0("exp",i,"_rev")]], lwd=2, lty="dotdash", col=pub.col) # Revised params/Sasso code
      res <- PBPK_run(model.param.filename="CF_template_parameters_Model.xlsx",
                      model.param.sheetname="Corley_mouse_params", 
                      exposure.param.filename="CF_template_parameters_Exposure.xlsx", 
                      exposure.param.sheetname="Corley_mouse",
                      adj.parms=c(Conc_init=conc.ppm[i], BW=BW[i]))
      lines(res$time, res$C_chppm, lwd=2, lty=templ.lty, col=templ.col7.1) # Corley params/Template
      }
    lines(acslx$time, acslx[[paste0("exp",i,"_Cor")]], lwd=2, lty=pub.lty, col=pub.col5.2) # Corley params/Sasso code
  }
  title("Mouse", line=-1.25)

  par(mfrow=c(1,1), oma=c(0, 0, 1, 0), mar=c(0, 0, 0, 0), new=TRUE)
  plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n")
  if (test_univ) {
    legend("bottomleft", c("Published Data", "Published Model (Corley assumptions)"),
           xpd=TRUE, inset=c(0.02,0), bty="n", col=c(pub.col,pub.col5.2),
           lty=c(NA,pub.lty), pch=c(19,NA), lwd=c(2,2), cex=1.12)
    legend("bottomleft", c("Template (Revised assumptions)",
                           "Template With Universal Lung-Blood"),
           xpd=TRUE, inset=c(0.5, 0), bty="n", col=c(templ.col, vcol[1]),
           lty=c(templ.lty, "dotted"), pch=c(NA,NA), lwd=c(2,2), cex=1.12)
  } else {
    legend("bottomleft", c("Published Data", "Published Model (Corley assumptions)",
                           "Template Model (Corley assumptions)"),
           xpd=TRUE, inset=c(0.05,0), bty="n", col=c(pub.col,pub.col5.2,templ.col7.1),
           lty=c(NA,pub.lty,templ.lty), pch=c(19,NA,NA), lwd=c(2,2,2), cex=1.12)
    legend("bottomleft", c("Published Model (Revised assumptions)",
                           "Template Moswl (Revised assumptions)"),
           xpd=TRUE, inset=c(0.55, 0), bty="n", col=c(pub.col, templ.col),
           lty=c("dotdash", templ.lty), pch=c(NA,NA), lwd=c(2,2), cex=1.12)
    }
  
  # Plot mouse simulation: Figure 10 of PFAS PBPK Template
  if (maketiff){
  tiff("CF_Corley_mouse.tiff", res=300, height=8, width=8, units="in")
  par(mar=c(5, 5, 2, 0), oma=c(4, 0.5, 0.5, 2.5))
  plot(1,1, type="n", log="y", xlab="Time (hr)", cex.lab=1.25, yaxt="n", xaxt="n", 
       ylab="Chamber Concentration (ppm)", xlim=c(0,5.5), ylim=c(10, 5000))
  axis(side=1, at=seq(0,5.5,0.5), labels=seq(0,5.5,0.5), cex.axis=1)
  axis(side=2, at=c(10,100,1000), labels=c(10,100,1000), cex.axis=1)

  for (i in 1:length(conc.ppm)){
    points(data_mouse$time_hr, data_mouse[[paste0("exp",i,"_conc_ppm")]], pch=19, col=pub.col)
    out <- PBPK_run(model.param.filename="CF_template_parameters_Model.xlsx",
                    model.param.sheetname="Revised_mouse_params",
                    exposure.param.filename="CF_template_parameters_Exposure.xlsx",
                    exposure.param.sheetname="Corley_mouse",
                    adj.parms=c(Conc_init=conc.ppm[i], BW=BW[i]))
    lines(out$time, out$C_chppm, lwd=2, lty=templ.lty, col=templ.col) # Revised params/Template
    lines(acslx$time, acslx[[paste0("exp",i,"_rev")]], lwd=2, lty="dotdash", col=pub.col) # Revised params/Sasso code
    res <- PBPK_run(model.param.filename="CF_template_parameters_Model.xlsx",
                    model.param.sheetname="Corley_mouse_params", 
                    exposure.param.filename="CF_template_parameters_Exposure.xlsx", 
                    exposure.param.sheetname="Corley_mouse",
                    adj.parms=c(Conc_init=conc.ppm[i], BW=BW[i]))
    lines(res$time, res$C_chppm, lwd=2, lty=templ.lty, col=templ.col7.1) # Corley params/Template
    lines(acslx$time, acslx[[paste0("exp",i,"_Cor")]], lwd=2, lty=pub.lty, col=pub.col5.2) # Corley params/Sasso code
  }
  par(mfrow=c(1,1), oma=c(0, 0, 1, 0), mar=c(0, 0, 0, 0), new=TRUE)
  plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("bottomleft", c("Published Data", "Corley assumptions (Published Model)", 
                         "Corley assumptions (Template Version)"),
         xpd=TRUE, inset=c(0.08, 0.02), bty="n", pch=c(19,NA,NA), lwd=c(2,2,3),
         lty=c(NA, pub.lty, templ.lty), col=c(pub.col, pub.col5.2, templ.col7.1))
  legend("bottomleft", c("Revised assumptions (Published Model)", 
                         "Revised assumptions (Template Version)"),
         xpd=TRUE, inset=c(0.52, 0.02), bty="n", pch=c(NA,NA), lwd=c(2,3),
         col=c(pub.col, templ.col), lty=c("dotdash", templ.lty))
  dev.off()
  } # End if (maketiff)
}

# plot.Corley.pub contains the commands for the plots in the VOC manuscript submission.
# This function should be run after the CF.Corley.chamber.total.plot.pub() function.
# The plot is saved as a vector file (".svg") in the user's working directory.

plot.Corley.pub <- function(){
  svg(filename="CF_Corley_erat_mouse.svg", width=12, height=5, pointsize=12)
  CF.Corley.chamber.total.plot.pub()
  dev.off()
}

Corley.diffs <- function(species="rat", test=FALSE){
  # Calculate error: if test=FALSE, percent difference between the template and 
  # Sasso acslx models for: (1) Corley parameters and (2) revised parameters
  # Discard first data point at time=0, C_ven=0.
  # If species != "rat", mouse is assumed
  # If test = TRUE, calculate differences using revised parameters with or 
  # without 'universal blood and lung' structure.
  conc.ppm <- c(103, 516, 929, 1291, 2581); BW <- c(0.226, 0.227, 0.237, 0.239, 0.218)
  if (species!="rat") {
    species="mouse"
    conc.ppm <- c(1000, 2500, 5000); BW <- c(0.028, 0.029, 0.029)
  }
  sheet2 = paste0("Revised_",species,"_params")
  # Import acslx results 
  acslx <- as.data.frame(read_excel(paste0("Data/Data_CF/",species,"_closed_chamber.xlsx")))
  if (test) {
    print("Maximum percentage difference between published and 'universal'",quote=FALSE)
    print(paste("blood and lung model structures for revised",species,"parameters."),quote=FALSE)
    sheet1 = sheet2
  } else {
    print(paste("Maximum percentage difference between Template and acslx",species,"models."),quote=FALSE)
    sheet1 = paste0("Corley_",species,"_params")
  }
  
  for (i in 1:length(conc.ppm)){
    Cor <- PBPK_run(model.param.filename="CF_template_parameters_Model.xlsx",
                    model.param.sheetname=sheet1, # Sasso paper - Revised|Corley parameters
                    exposure.param.filename="CF_template_parameters_Exposure.xlsx", 
                    exposure.param.sheetname=paste0("Corley_",species), data.times=acslx$time,
                    adj.parms=c(Conc_init=conc.ppm[i], BW=BW[i]), test_univ=test)
    revi <- PBPK_run(model.param.filename="CF_template_parameters_Model.xlsx",
                    model.param.sheetname=sheet2,  # Sasso paper - revised parameters
                    exposure.param.filename="CF_template_parameters_Exposure.xlsx",
                    exposure.param.sheetname=paste0("Corley_",species), data.times=acslx$time,
                    adj.parms=c(Conc_init=conc.ppm[i], BW=BW[i]))
    
    print(paste("Experiment",i),quote=FALSE)
    if (test) {
      print(paste("Corley vs 'universal:", max.diff(Cor$C_chppm, revi$C_chppm)),quote=FALSE)
    } else {
      print(paste("a. Corley parameters:", 
                  max.diff(Cor$C_chppm, acslx[paste0("exp",i,"_Cor")])),quote=FALSE)
      print(paste("b. Revised parameters:", 
                  max.diff(revi$C_chppm, acslx[paste0("exp",i,"_rev")])),quote=FALSE)
    }
  }
}