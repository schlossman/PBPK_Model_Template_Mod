initParms_Flouride <- function(newParms = NULL) {
  parms <- c(
    MOLWT = 19,
    RTemp = 24450.0,
    oral_dose_init = 0.0,
    iv_dose = 0.0,
    BW0 = 16,
    Conc0_ppm = 0.0,
    NCH = 0.0,
    VCHC = 0.0,
    KL = 0.0,
    F_inh = 1.0,
    AS_co = 0.74,
    AS_pcl = 0.75,
    P_Li = 1.30634,
    P_Ki = 5.54528,
    P_RP = 1.10639,
    P_SP = 0.83979,
    P_Bo = 1333,
    P_PB = 1.333,
    Q_cardiacc = 15,
    VPR = 1.472222,
    Q_Kic = 0.18,
    Q_Boc = 0.05,
    V_Blc = 0.07,
    V_Lic = 0.026,
    V_Boc = 0.07,
    GASD = 0.0,
    V_ch = 0.0,
    Conc_ambient = 0.0
  )

  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
    parms[names(newParms)] <- newParms
  }

  parms <- within(as.list(parms), {
  })
  out <- .C("getParms_Flouride",  as.double(parms),
            out=double(length(parms)),
            as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

Outputs_Flouride <- c(
    "A_bal",
    "A_body",
    "A_out",
    "C_Li",
    "C_Ki",
    "C_Bo",
    "C_RP",
    "C_SP",
    "C_ch",
    "C_Ven",
    "C_Art",
    "BW",
    "Q_bal",
    "Q_c",
    "Q_Li",
    "Q_Ki",
    "Q_Bo",
    "Q_RP",
    "Q_SP",
    "CL_bKiU",
    "R_inh",
    "V_Bl",
    "V_Ki",
    "V_Li",
    "V_Bo",
    "V_RP",
    "V_SP",
    "Q_rel",
    "VolT_frac"
)

initStates_Flouride <- function(parms, newStates = NULL) {
  Y <- c(
    A_Li = 0.0,
    A_Ki = 0.0,
    A_Bo = 0.0,
    A_Mi = 0.0,
    A_RP = 0.0,
    A_SP = 0.0,
    R_IV = 0.0,
    R_0Li = 0.0,
    A_in = 0.0,
    A_ch = 0.0,
    A_Urine = 0.0,
    AUC_Ven = 0.0,
    AUC_Li = 0.0,
    AUC_Ki = 0.0,
    AUC_Bo = 0.0,
    AUC_RP = 0.0,
    AUC_SP = 0.0,
    Q_cc = 0.0,
    Conc_mgL = 0.0
  )

  Y <- within(c(as.list(parms),as.list(Y)), {    Y["A_ch"] <- Conc0_ppm * GASD * V_ch 

    Y["A_Li"] <- 0.0 
    Y["A_Ki"] <- 0.0 
    Y["A_Bo"] <- 0.0 
    Y["A_Mi"] <- 0.0 
    Y["A_RP"] <- 0.0 
    Y["A_SP"] <- 0.0 
    Y["R_IV"] <- 0.0 
    Y["R_0Li"] <- 0.0 
    Y["A_Urine"] <- 0.0 
    Y["A_in"] <- oral_dose_init * BW0 + iv_dose * BW0 
    Y["Conc_mgL"] <- Conc_ambient * GASD 

    Y["Q_cc"] <- Q_cardiacc 

    Y["AUC_Ven"] <- 0.0 
    Y["AUC_Li"] <- 0.0 
    Y["AUC_Ki"] <- 0.0 
    Y["AUC_Bo"] <- 0.0 
    Y["AUC_RP"] <- 0.0 
    Y["AUC_SP"] <- 0.0 

  })$Y

  if (!is.null(newStates)) {
    if (!all(names(newStates) %in% c(names(Y)))) {
      stop("illegal state variable name in newStates")
    }
    Y[names(newStates)] <- newStates
  }

.C("initState_Flouride", as.double(Y));
Y
}
