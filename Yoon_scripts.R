# Yoon et al. PBPK model for VOCs (2007)
# Model template simulations 
# Author: Amanda Bernstein, US EPA (ORISE), January 2023
# Revisions for use with MCSimMod: Paul Schlosser, December 2025
# Revisions for comparison with 'universal' blood & lung structure:
#   Paul Schlosser, April 2026

# Set working directory to the directory containing this file.
script.dir = dirname(sys.frame(1)$ofile); setwd(script.dir)
source("run_template_model.R") # Load functions to run the PBPK model template.

# Table 5: Periodic inhalation exposure for 2 month old rats

daily_avg <- function(out, t5, idx){
  # Compute daily averages over the last week of simulations in Yoon.table5 
  # functions below
  nt <- length(out$time)
  nl <- max(which(out$time.days <= (out$time.days[nt]-7))) # Start of last week
  t5$Cmax[idx] <- max(out$C_ven)
  t5$AUC[idx] <- (out$AUC_art[nt] - out$AUC_art[nl])/7
  t5$AM[idx] <- (out$A_met_sat[nt] - out$A_met_sat[nl])/7
  t5$AMLVL[idx] <- (out$A_met_sat_li[nt] - out$A_met_sat_li[nl])/(7*out$V_li[nt]*1000)
      # AMLVL is normalized to liver mass in grams
  # Extra-hepatic metabolism
  livermet <- out$A_met_sat_li[nt] - out$A_met_sat_li[nl]
  t5$LungEHM[idx] <- 100*(out$A_met_sat_lu[nt] - out$A_met_sat_lu[nl])/livermet
  t5$KidneyEHM[idx] <- 100*(out$A_met_sat_om[nt] - out$A_met_sat_om[nl])/livermet
  return(t5)
}

Yoon.table5 <- function(chem="VC", test_univ = FALSE){
  # Yoon et al. (2007) Table 5 Results. 'chem' can also be "TCE" or "CCl4"
  # 4-hour inhalation exposure, 5 days per week for 2 month old rats
  if (chem=="VC"){
    esheet = "inhal_4hr_5dweek"
    conc <- c(1,10000)
  } else if (chem=="TCE"){
    esheet = "inhal_8hr_5dweek"
    conc <- c(50,600)
    } else {
      esheet = "inhal_6hr_5dweek"
      conc <- c(5,400)
    }
  
  # Load values saved from previous version of this function (used the old MCSim method):
  s5 <- as.data.frame(readRDS(paste0("Data/Yoon.table5.",chem,".rds")))
  
  # Set up simulation runs
  Cmax <- AUC <- AM <- AMLVL <- LungEHM <- KidneyEHM <- rep(NaN,length(conc))
  EHM <- c(rep("no",length(conc)),rep("yes",length(conc)))
  at5 <- t5 <- data.frame(conc, EHM, Cmax, AUC, AM, AMLVL, LungEHM, KidneyEHM)
  idx = 1
  
  # Compute dose metrics without, then with extra-hepatic metabolism
  for (psheet in c(paste0("2mo_",chem,"_noEHM"), paste0("2mo_",chem,"_EHM"))) {
    for (ii in conc){ # for each concentration
      out <- PBPK_run(model.param.filename = "Yoon_template_parameters_Model.xlsx",
                      model.param.sheetname = psheet, 
                      exposure.param.filename = "Yoon_template_parameters_Exposure.xlsx", 
                      exposure.param.sheetname = esheet, 
                      adj.parms = c(Conc_init = ii))
      t5 <- daily_avg(out, t5, idx)
      if (test_univ){
        out <- PBPK_run(model.param.filename = "Yoon_template_parameters_Model.xlsx",
                        model.param.sheetname = psheet, 
                        exposure.param.filename = "Yoon_template_parameters_Exposure.xlsx", 
                        exposure.param.sheetname = esheet, 
                        adj.parms = c(Conc_init = ii), test_univ = TRUE)
        at5 <- daily_avg(out, at5, idx)
      }
      idx = idx + 1
    }
  }
  
  # Append table of % differences between current t5 and saved version (s5),
  # rounded to three significant figures:
  print.noquote(paste("Replication of Yoon et al. (2007) Table 5 values (rows 1-4) for",chem))
  print.noquote("  and % differences between previously saved calculations (rows 5-8).")
  s5[,3:8] <- abs(round(100*(s5[,3:8] - t5[,3:8])/(s5[,3:8]+1e-18), 3))
  pt5[,3:8] <- signif((pt5 <- t5)[,3:8],3)
  res = rbind(pt5,s5)
  if (test_univ){ # Differences between Template version with/without 'Universal' blood & lung
    print.noquote("'Universal' blood & lung results for Yoon et al. (2007) Table 5 values (rows 9-12)")
    print.noquote("  and % differences between 'Universal' and structure-matched results (rows 13-16).")
    s5[,3:8] <- abs(round(100*(at5[,3:8] - t5[,3:8])/(t5[,3:8]+1e-18), 3))
    at5[,3:8] <- signif(at5[,3:8],3)
    res = rbind(res,at5,s5)
  }
  return(res)
}