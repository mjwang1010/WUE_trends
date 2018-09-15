# Calculate hourly surface conductance
# Calculate different kinds of WUE, including:
#1 ecosystem WUE GPP/ET
#2 intrinsic WUE GPP/Gs
#3 inherent WUE GPP/ET*VPD
#4 underlying WUE GPP/ET*VPD^0.5
#5 G1 (stomatal slope) derive from Medlyn's model
# Get peak-GPP-month hourly data
# save in one file
#####
rm(list = ls())

library(lubridate)

###############
# define functions
###############
# slope of the Ta-VPD curve (KPa K-1)
delta_fun <- function(Ta){
  # input - air temperature, (C)
  # output - slope of T-es curve (KPa*K-1)
  h2osat_fun <- function(x){
    h2o_base = 0.61121
    h2o_mult = 17.502 
    h2o_add = 240.97
    esT = h2o_base*exp((h2o_mult*x)/(h2o_add+x))
    return(esT)
  }
  psi_fun <- function(x,y){
    psi_mult = 240.97*17.502
    psi_add = 240.97
    delta = (psi_mult*x)/((y+psi_add)^2)
    return(delta)
  }
  #
  esT = h2osat_fun(Ta) # h2o saturation
  delta = psi_fun(esT, Ta)
  return(delta)
}

# Calculate aerodynamic conductance (m s-1)
Ga_calfromflx_fun <- function(u, ustar){
  ra = u/(ustar^2) + 6.2*ustar^(-2/3)
  Ga = 1/ra;
  #Ga[u==-9999|ustar==-9999] = NA;
  return(Ga)
}

# Calculate surface conductance from FLUXNET data (m s-1)
Gs_calfromflx_fun <- function(VPD, A, ro, Cp, Ga, delta, gamma, LEobs){
  # calculate surface conductance by using Penman-Monteith equation
  # Estimate the Gcx by inverting P-M equation
  # VPD - vapour pressure deficit kPa
  # A - available energy W*m-2
  # ro - air density
  # Cp - 
  # Ga - aerodynamic conductance 
  # delta - 
  # gamma - 
  # LEobs - LE observations W*m-2
  #
  ratio = delta/gamma
  Gs = LEobs*Ga/(ratio*A - (ratio+1)*LEobs + Ga*ro*Cp*VPD/gamma)
  return(Gs)
}

# Fill hourly CO2 concentration data
fillCa_fun <- function(Ca_hr, CO2_mo_fill, flx_year_hr, flx_month_hr, n_steps_day) {
  # fill hourly CO2 concentration with global monthly mean values
  library(lubridate)
  #
  moday <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  moday_leap <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  flx_year <- unique(flx_year_hr)
  nyear <- length(flx_year)
  dim(CO2_mo_fill) <- c(12, length(CO2_mo_fill)/12)
  #
  Ca_hr_fill <- Ca_hr
  for (i in 1:nyear) {
    #
    #########
    if (leap_year(flx_year[i])) {
      tempdata_mo <- CO2_mo_fill[,i]
      for(j in 1:12) {
        tempdata_hr <- rep(tempdata_mo[j], each=moday_leap[j]*n_steps_day)
        Ca_hr_fill[flx_year_hr==flx_year[i]&flx_month_hr==j] <- tempdata_hr
      }
    } 
    else {
      tempdata_mo <- CO2_mo_fill[,i]
      for(j in 1:12) {
        tempdata_hr <- rep(tempdata_mo[j], each=moday[j]*n_steps_day)
        Ca_hr_fill[flx_year_hr==flx_year[i]&flx_month_hr==j] <- tempdata_hr
      }
    }
  }
  #
  Ca_hr_fill[!is.na(Ca_hr)] <- Ca_hr[!is.na(Ca_hr)]
  return(Ca_hr_fill)
}


################
# end of definitions of functions
################

#######
# define variables
# value filling the missing data
mfill = -9999
# psychrometric constant
gamma = 0.066 # kPa*K-1
# air density
ro = 1.15 # kg*m-3
# specific heat capacity of air
Cp = 1012 # J*kg-1*K-1
# gas constant 
Rgas <- 8.3144598 # J mol-1 K-1

###
# file paths and names
# Your own file paths and names
infilepath1 = '.../FLUXNET2015_Nov/'
infilepath4 = '.../FLUXNET2015_PATCH1/'
infilename3 <- '.../co2_mm_gl.txt' # CO2 concentration
infilename1 <- '.../SynFLX2015/months_peakgpp_allsites.csv' 
infilename2 <- '.../FLUXNET2015_siteinfo.csv'
outfilepath1 <- '...'

###
# site information
# read site information in a file
siteinfo <- read.table(infilename2, header=T, sep=',', stringsAsFactors=F)
sitepft <- siteinfo$pft
siteid <- siteinfo$site
nsite = length(siteid)

###
# information about peak-gpp months
monthinfo <- read.table(infilename1, header = T, sep = ',', stringsAsFactors = F)

time.start <- Sys.time()
###
# save peak-gpp month data
df.out <- data.frame()
for(i in 1:nsite){
  
  ######
  # surface conductance under dry periods
  ######
  # read hourly flux data
  flxfiledir1 = list.files(infilepath1, pattern = paste0('FLX_',siteid[i],'.*'))
  if (isTRUE(all.equal(flxfiledir1, character(0)))) next 
  flxfiledir2hr = list.files(paste0(infilepath1, flxfiledir1), pattern = '.*_FULLSET_H{1}')
  flxT_hr = read.csv(paste0(infilepath1,flxfiledir1,'/',flxfiledir2hr))
  ## get timestamp of hourly data
  ## First get this because 'flx_year' will be used
  time_hr = flxT_hr$TIMESTAMP_START
  flx_year_hr = floor(time_hr/100000000)
  flx_month_hr = floor((time_hr-flx_year_hr*100000000)/1000000)
  flx_day_hr <- floor((time_hr-flx_year_hr*100000000-flx_month_hr*1000000)/10000)
  flx_year = unique(flx_year_hr)  
  nyear = length(flx_year) # number of years
  # get hourly steps
  time_step = (time_hr[2]-time_hr[1])/100
  n_steps_day = ifelse(time_step==1, 24, 48)
  # get site PFT
  
  ######
  # read daily flux data
  flxfiledir2d <- list.files(paste0(infilepath1, flxfiledir1), pattern = '.*_FULLSET_DD')
  flxT_d <- read.csv(paste0(infilepath1,flxfiledir1,'/',flxfiledir2d))
  # get timestamp of daily data
  time_d <- flxT_d$TIMESTAMP
  flx_year_d <- floor(time_d/10000)
  flx_month_d <- floor((time_d - flx_year_d*10000)/100)
  
  ######
  # read monthly flux data
  flxfiledir2mo <- list.files(paste0(infilepath1, flxfiledir1), pattern = '.*_FULLSET_MM')
  flxT_mo <- read.csv(paste0(infilepath1,flxfiledir1,'/',flxfiledir2mo))
  # timestamp of monthly data
  time_mo <- flxT_mo$TIMESTAMP
  flx_year_mo <- floor(time_mo/100)
  flx_month_mo <- (time_mo-flx_year_mo*100)
  
  #######
  # Deal with QC problem in heat flux data
  # Heat flux QC of hourly data
  hflxdir1 = list.files(infilepath4, pattern = paste0('FLX_',siteid[i],'_FLUXNET2015_PATCH1_H{1}'))
  hflxqc_hr = read.csv(paste0(infilepath4, hflxdir1))
  timeqc_hr <- hflxqc_hr$TIMESTAMP_START
  Hqc_hr = hflxqc_hr$H_F_MDS_QC
  LEqc_hr = hflxqc_hr$LE_F_MDS_QC
  # years of hourly QC
  qc_year_hr <- floor(timeqc_hr/100000000)
  
  # Heat flux QC data in daily scale
  hflxdir2 <- list.files(infilepath4,pattern=paste0('FLX_',siteid[i],'_FLUXNET2015_PATCH1_DD'))
  hflxqc_d <- read.csv(paste0(infilepath4, hflxdir2))
  timeqc_d <- hflxqc_d$TIMESTAMP
  Hqc_d <- hflxqc_d$H_F_MDS_QC
  LEqc_d <- hflxqc_d$LE_F_MDS_QC
  # years of daily QC
  qc_year_d <- floor(time_d/10000)
  
  ######
  # 'clean' data 
  # A ROBUST way to clean data
  # assume: problem with the last year data
  # hourly data: 24 or 48 data in the last day
  # daily data: 31 days in December
  # monthly data: December is the last month
  # annual data: QC data and original data have the same size
  ndif_hr <- n_steps_day-
    length(which(flx_year_hr==flx_year[nyear]&flx_month_hr==12&flx_day_hr==31))#
  if (ndif_hr>0) {
    # add rows
    newrows <- array(dim = c(ndif_hr,dim(flxT_hr)[2]))
    newrows <- as.data.frame(newrows)
    names(newrows) <- names(flxT_hr) # have the same names
    flxT_hr <- rbind(flxT_hr, newrows)
  }
  #
  ndif_d <- 31 - 
    length(which(flx_year_d==flx_year[nyear]&flx_month_d==12))
  if (ndif_d>0) {
    newrows <- as.data.frame(array(dim = c(ndif_d, dim(flxT_d)[2])))
    names(newrows) <- names(flxT_d)
    flxT_d <- rbind(flxT_d, newrows)
  }
  #
  ndif_mo <- 12 - 
    length(which(flx_year_mo==flx_year[nyear]))
  if (ndif_mo>0) {
    newrows <- as.data.frame(array(dim = c(ndif_mo, dim(flxT_mo)[2])))
    names(newrows) <- names(flxT_mo)
    flxT_mo <- rbind(flxT_mo, newrows)
  }
  #
  # same as length of FLUXNET hourly data
  Hqc_hr <- Hqc_hr[1:length(flxT_hr[,1])]
  LEqc_hr <- LEqc_hr[1:length(flxT_hr[,1])]
  # same as length of FLUXNET daily data
  Hqc_d <- Hqc_d[1:length(flxT_d[,1])]
  LEqc_d <- LEqc_d[1:length(flxT_d[,1])]
  
  ######
  # read yearly flux data
  
  #####
  # read CO2 concentration data
  CO2T_data <- read.table(infilename3)
  
  #####
  # carbon fluxes
  # NEE/NEP, umolCO2 m-2 s-1
  NEE_hr = flxT_hr$NEE_VUT_REF
  NEEqc_hr = flxT_hr$NEE_VUT_REF_QC
  NEE_hr[NEE_hr==mfill|NEEqc_hr>1] = NA
  NEP_hr = -NEE_hr
  # GPP
  GPP_hr = flxT_hr$GPP_NT_VUT_REF
  GPPqc_hr = flxT_hr$NEE_VUT_REF_QC
  GPP_hr[GPP_hr==mfill|GPPqc_hr>1] = NA
  # RE
  RE_hr = flxT_hr$RECO_NT_VUT_REF
  REqc_hr = flxT_hr$NEE_VUT_REF_QC
  RE_hr[RE_hr==mfill|REqc_hr>1] = NA
  
  # water fluxes
  # LE
  LE_hr = flxT_hr$LE_F_MDS
  LE_hr[LE_hr==mfill|LEqc_hr>1] = NA
  
  # sensible heat fluxes (H)
  H_hr = flxT_hr$H_F_MDS
  H_hr[H_hr==mfill|Hqc_hr>1] = NA
  
  # radiation fluxes
  SW_hr = flxT_hr$SW_IN_F_MDS
  SWqc_hr = flxT_hr$SW_IN_F_MDS_QC
  SW_hr[SW_hr==mfill|SWqc_hr>1] = NA
  
  # soil heat flux
  G_hr = flxT_hr$G_F_MDS
  Gqc_hr = flxT_hr$G_F_MDS_QC
  G_hr[G_hr==mfill|Gqc_hr>1] = NA
  
  # net radiation
  Rn_hr = flxT_hr$NETRAD
  Rn_hr[Rn_hr==mfill] = NA
  if (isTRUE(all.equal(Rn_hr,logical(0)))) {
    print(paste0(siteid[i],' no net radiation'))
    next
  }
  
  # air temperature, C
  Ta_hr = flxT_hr$TA_F_MDS
  Taqc_hr = flxT_hr$TA_F_MDS_QC
  Ta_hr[Ta_hr==mfill|Taqc_hr>1] = NA
  
  # VPD, hPa
  VPD_hr = flxT_hr$VPD_F_MDS
  VPDqc_hr = flxT_hr$VPD_F_MDS_QC
  VPD_hr[VPD_hr==mfill|VPDqc_hr>1] = NA
  VPD_hr = VPD_hr/10 # KPa
  
  # precipitation
  P_hr = flxT_hr$P_F
  Pqc_hr = flxT_hr$P_F_QC
  
  # soil moisture (NOTE: this could be NULL)
  SWC_hr = flxT_hr$SWC_F_MDS_1
  SWCqc_hr = flxT_hr$SWC_F_MDS_1_QC
  SWC_hr[SWC_hr==mfill|SWCqc_hr>1] = NA
  ###
  # Sites have no SWC records, which are screened
  if (isTRUE(all.equal(SWC_hr,logical(0)))) {
    # create a vector
    SWC_hr <- rep(NA, length(NEP_hr))
  }
  
  # wind speed
  u_hr = flxT_hr$WS_F
  u_hr[u_hr==mfill] = NA
  
  # friction velocity
  ustar_hr = flxT_hr$USTAR
  ustar_hr[ustar_hr==mfill] = NA
  
  # CO2 concentration
  CO2_hr <- flxT_hr$CO2_F_MDS
  CO2_hr[CO2_hr==mfill] <- NA
  
  # nighttime NEE
  
  
  ##########
  # daily precipitation, mm
  P_d <- flxT_d$P_F
  P_d[P_d==mfill] <- NA
  
  ##########
  # monthly CO2 concentration from FLUXNET
  CO2_mo <- flxT_mo$CO2_F_MDS
  CO2_mo[CO2_mo==mfill] <- NA
  
  #####
  # Get global monthly mean CO2 concentration
  CO2yr_mm_gl <- CO2T_data[,'V1']
  CO2_mm_gl <- CO2T_data[CO2yr_mm_gl>=flx_year[1]&CO2yr_mm_gl<=flx_year[nyear],'V4']
  
  #####
  # Calculate surface conductance
  delta <- delta_fun(Ta_hr)
  Ga_hr <- Ga_calfromflx_fun(u_hr,ustar_hr)
  Gs_hr <- Gs_calfromflx_fun(VPD_hr,Rn_hr,ro,Cp,Ga_hr,delta,gamma,LE_hr)
  ##
  # post-processing, data selection
  Gs_hr_filter <- Gs_hr # data selection
  Gs_hr_filter[SW_hr<100|is.na(SW_hr)] <- NA # daytime data
  Gs_hr_filter[GPP_hr<5|is.na(GPP_hr)] <- NA # positvie carbon uptake
  Gs_hr_filter[Ta_hr<5|is.na(Ta_hr)] <- NA # extreme low air temperature
  Gs_hr_filter[Gs_hr_filter<=0.000001] <- NA # unrealistic values
  Gs_hr_filter[(Rn_hr-LE_hr)<5] <- NA # Rn generally larger than LE
  
  #########
  # GPP - umolCO2 m-2 s-1
  # VPD - KPa
  # Ca - umol mol-1
  # Gs - m s-1, turn into mol m-2 s-1
  # PA - Pa
  # Rgas - J mol-1 K-1
  # Tk - K
  ###
  # unit transformation
  Tk_hr <- Ta_hr+273.15 # from C to K
  PA_hr <- flxT_hr$PA_F*1000 # from kPa to Pa
  PA_hr[PA_hr==mfill] <- NA
  Ca_hr <- flxT_hr$CO2_F_MDS
  Ca_hr[Ca_hr==mfill] <- NA
  Gs_mol <- Gs_hr_filter*PA_hr/(Rgas*Tk_hr) # from m s-1 to mol m-2 s-1
  ########
  # remove impact of Precipitation
  P_flag <- P_d
  P_flag[] <- 0 # initiate an array for flagging
  P1d_index <- which(P_d>0.1&!is.na(P_d))
  P_flag[P1d_index] <- 1
  P2d_index <- P1d_index+1 # remove the day after the rainy day
  P2d_index[P2d_index>length(P_d)] <- length(P_d) # in case beyond the boundary
  P_flag[P2d_index] <- 1
  # from Daily to Hourly
  P_flag_hr <- rep(P_flag, each=n_steps_day)
  Gs_mol[P_flag_hr==1] <- NA
  
  ###########
  # use global mean co2 concentration to fill CO2 gaps
  CO2_mo_fill <- CO2_mo # 
  CO2_mo_fill[is.na(CO2_mo)] <- CO2_mm_gl[is.na(CO2_mo)] # monthly Ca
  Ca_hr_fill <- fillCa_fun(Ca_hr, CO2_mo_fill, flx_year_hr, flx_month_hr, n_steps_day) # hourly Ca
  
  #########
  # calculate different WUEs
  #########
  # calculate hourly G1 by using Medlyn's model
  G1_hr <- Gs_mol*VPD_hr^0.5*Ca_hr/(1.6*GPP_hr)-VPD_hr^0.5 # kPa^0.5
  G1_hr_fill <- Gs_mol*VPD_hr^0.5*Ca_hr_fill/(1.6*GPP_hr)-VPD_hr^0.5
  
  ###
  # calculate hourly iWUE
  # iWUE = GPP/Gsw, umol mol-1
  iWUE_hr <- GPP_hr/Gs_mol
  # Note: iWUE be in a reasonable range
  
  # calculate Ci
  # Ca-Ci = iWUE = GPP/Gsw*1.6
  # Then, Ci = Ca-GPP/Gsw*1.6
  Ci_hr <- Ca_hr-1.6*iWUE_hr
  
  ###
  # calculate hourly IWUE
  ET_factor <- (2.501*10^6 - 2.365*10^3*Ta_hr) * 18*10^-3 # J mol-1
  ET_hr <- LE_hr/ET_factor # molH2O m-2 s-1
  ihWUE_hr <- GPP_hr*VPD_hr/ET_hr # umol mol-1 kPa
  ihWUE_hr[is.na(iWUE_hr)] <- NA
  
  ###
  # calculate hourly eWUE
  eWUE_hr <- GPP_hr/ET_hr # umol mol-1
  eWUE_hr[is.na(iWUE_hr)] <- NA
  
  ###
  # calculate hourly uWUE
  uWUE_hr <- GPP_hr*VPD_hr^0.5/ET_hr # umol mol-1 kPa^0.5
  uWUE_hr[is.na(iWUE_hr)] <- NA

  ############
  ## save results
  ## save estimated surface conductances
  ## create a data frame
  time_start <- flxT_hr$TIMESTAMP_START
  ###
  ## save iWUE and related variables for post processes
  WUE_df <- data.frame(time_start, eWUE_hr, iWUE_hr, ihWUE_hr, uWUE_hr, G1_hr, Ci_hr, 
                       NEP_hr,GPP_hr,LE_hr,VPD_hr,SWC_hr,P_hr,Ta_hr,SW_hr,u_hr,ustar_hr,Rn_hr,Ga_hr)
  ###
  # save 'summer' time data
  # use peak index to minimize LAI effect (or soil evaporation)
  peakmonth <- c(monthinfo[monthinfo$siteID==siteid[i],]$month1,
                 monthinfo[monthinfo$siteID==siteid[i],]$month2, 
                 monthinfo[monthinfo$siteID==siteid[i],]$month3)
  WUE_peak <- WUE_df[is.element(flx_month_hr,peakmonth), ]
  # screening: 
  # add site ID and PFT
  dftemp1 <- data.frame(siteID = rep(siteid[i],dim(WUE_peak)[1]))
  dftemp2 <- data.frame(PFT = rep(sitepft[i],dim(WUE_peak)[1]))
  dftemp3 <- cbind(dftemp1, dftemp2, WUE_peak)
  df.out <- rbind(df.out, dftemp3)
  ###
}
time.end <- Sys.time()
###
# save peak-gpp month data
write.table(df.out, paste0(outfilepath1, 'peakmonth_hr_wue_allsites1.csv'), row.names=F, sep=',')

