# Trend test
# test trend for each site
# interannual trend

rm(list=ls())
library(trend)
library(ggplot2)
library(lubridate)

# define a function
# linear trend test
#####
mkslope_fun<-function(x) {
  # x - annual mean data; 
  # first column is years
  # second column is data
  tr.data <- x[complete.cases(x),]
  if (length(tr.data[,1])<5) {
    print(paste0(allsiteID[i],': less than 5 data'))
    re.value <- data.frame(NOY = 0,
                           MK.p = 0, 
                           sen.s = 0)
    return(re.value)
  } else {
    mk.res <- mk.test(tr.data[,2])
    slope.res <-sens.slope((tr.data[,2]))
    ny <- length(tr.data[,1])
    re.value <- data.frame(NOY = ny, 
                           MK.p = mk.res$p.value,
                           sen.s = slope.res$estimates)
    return(re.value)
  }
}

# Function to estimate trend (slope) 
# trend of absolute values
value_trend_fun <- function(x, varname, vargroup) {
  
  # for southern hemisphere
  # if (any(is.element(monthv, c(1,2,11,12)))) {
  #   
  # }
  ###
  agg.mean.x.res <- aggregate(x, by=list(vargroup), FUN='mean', na.rm=T)
  agg.std.x.res <- aggregate(x, by=list(vargroup), FUN='sd', na.rm=T)
  colnames(agg.mean.x.res) <- c('year', varname)
  mk.x.res <- mkslope_fun(agg.mean.x.res)
  #
  return(mk.x.res)
}

# trend of ratios
ratio_trend_fun <- function(x, varname, vargroup) {
  # 
  agg.mean.x.res <- aggregate(x, by=list(vargroup), FUN='mean', na.rm=T)
  agg.std.x.res <- aggregate(x, by=list(vargroup), FUN='sd', na.rm=T)
  colnames(agg.mean.x.res) <- c('year', varname)
  # mean of multiple years
  mean.ann.x <- mean(agg.mean.x.res[,varname], na.rm=T)
  ratio.x <- (agg.mean.x.res[,varname]-mean.ann.x)/mean.ann.x*100 # unit: %
  # NOTE: as.data.frame()
  ratio.x.res <- as.data.frame(cbind(agg.mean.x.res[,'year'], ratio.x)) # same structure as 'agg.mean.x.res'
  mk.rx.res <- mkslope_fun(ratio.x.res)
  #
  return(mk.rx.res)
}

#####

# Your own file paths and names
# NOTE: Be careful that in "peakmonth_hr_wue_allsites1.csv" the data is without filtering
infilename1 <- '.../peakmonth_hr_iwue_allsites1.csv'
infilename2 <- '.../FLUXNET_siteinfo2_forest.csv'
outfilepath1 <- '.../SynFLX2015/'
outfigpath1 <- '.../SynFLX2015Fig/'

###
# estimate iWUE trend in summer
# Read data
dataT <- read.table(infilename1, header = T, sep = ',', stringsAsFactors = F)
siteinfoT <- read.table(infilename2, header = T, sep = ',', stringsAsFactors = F)
allsiteID <- unique(dataT$siteID)
nsite <- length(allsiteID) # the number of sites

iWUE_peak <- dataT$iWUE_hr
GPP_peak <- dataT$GPP_hr
VPD_peak <- dataT$VPD_hr
Ca_peak <- dataT$Ca_hr
Gs_peak <- dataT$Gs_mol
LE_peak <- dataT$LE_hr
Ta_peak <- dataT$Ta_hr
SR_peak <- dataT$SW_hr # shortwave radiation
Pre_peak <- dataT$P_hr
SWC_peak <- dataT$SWC_hr
time_peak <- dataT$time_start

###
# a data frame to get trend detecting results
df.iwue.out <- data.frame()
df.gs.out <- data.frame()
df.gpp.out <- data.frame()
df.le.out <- data.frame()
df.vpd.out <- data.frame()
df.ca.out <- data.frame()
df.ta.out <- data.frame()
df.sr.out <- data.frame()
df.swc.out <- data.frame()
df.pre.out <- data.frame()
#
for (i in 1:nsite) {
  ###
  # a case test
  siteiwue <- iWUE_peak[dataT$siteID==allsiteID[i]]
  sitegs <- Gs_peak[dataT$siteID==allsiteID[i]]
  sitegpp <- GPP_peak[dataT$siteID==allsiteID[i]]
  sitele <- LE_peak[dataT$siteID==allsiteID[i]]
  sitevpd <- VPD_peak[dataT$siteID==allsiteID[i]]
  siteca <- Ca_peak[dataT$siteID==allsiteID[i]]
  siteta <- Ta_peak[dataT$siteID==allsiteID[i]]
  sitesr <- SR_peak[dataT$siteID==allsiteID[i]]
  sitepre <- Pre_peak[dataT$siteID==allsiteID[i]]
  siteswc <- SWC_peak[dataT$siteID==allsiteID[i]]
  #
  sitetime <- time_peak[dataT$siteID==allsiteID[i]]
  sitepft <- unique(dataT$PFT[dataT$siteID==allsiteID[i]])
  # sitedata <- dataT[dataT$siteID=='CH-Fru',]
  # get year, month and day
  sitetime.gmt <- strptime(sitetime, '%Y%m%d%H%M')
  siteyear.hr <- year(sitetime.gmt)
  sitemonth.hr <- month(sitetime.gmt)
  siteyear <- unique(siteyear.hr)

  ###
  # mk.iwue.res <- value_trend_fun(siteiwue, 'iwue', siteyear.hr)
  # mk.gs.res <- value_trend_fun(sitegs, 'gs', siteyear.hr)
  # mk.gpp.res <- value_trend_fun(sitegpp, 'gpp', siteyear.hr)
  # mk.le.res <- value_trend_fun(sitele, 'le', siteyear.hr)
  # mk.vpd.res <- value_trend_fun(sitevpd, 'vpd', siteyear.hr)
  # mk.ca.res <- value_trend_fun(siteca, 'ca', siteyear.hr)
  # #
  # if (mk.le.res==0) {
  #   print('next.')
  #   next
  # }
  
  ### ratio, relative to annual mean
  mk.riwue.res <- ratio_trend_fun(siteiwue, 'iwue', siteyear.hr)
  # mk.rgs.res <- ratio_trend_fun(sitegs, 'gs', siteyear.hr)
  # mk.rgpp.res <- ratio_trend_fun(sitegpp, 'gpp', siteyear.hr)
  # mk.rle.res <- ratio_trend_fun(sitele, 'le', siteyear.hr)
  # mk.rvpd.res <- ratio_trend_fun(sitevpd, 'vpd', siteyear.hr)
  # mk.rca.res  <- ratio_trend_fun(siteca, 'ca', siteyear.hr)
  # mk.rta.res <- ratio_trend_fun(siteta, 'ta', siteyear.hr)
  # mk.rsr.res <- ratio_trend_fun(sitesr, 'sr', siteyear.hr)
  # mk.rpre.res <- ratio_trend_fun(sitepre, 'pre', siteyear.hr)
  # mk.rswc.res <- ratio_trend_fun(siteswc, 'swc', siteyear.hr)
  if (mk.riwue.res$NOY==0) {
    print('next!')
    next
  }
  
  #####
  ### save trend results
  # site ID
  # PFT
  # Number of years (NOY)
  # MK.p.value
  # sen.slope
  ###
  # IWUE
  tempdf <- data.frame(siteID=allsiteID[i],
                       PFT = sitepft,
                       NOY = mk.riwue.res$NOY,
                       MK.p = mk.riwue.res$MK.p,
                       sen.s = mk.riwue.res$sen.s)
  df.iwue.out <- rbind(df.iwue.out, tempdf)
  # Gs results
  # tempdf <- data.frame(siteID=allsiteID[i],
  #                       PFT=sitepft,
  #                       NOY=mk.rgs.res$NOY,
  #                       MK.p = mk.rgs.res$MK.p,
  #                       sen.s = mk.rgs.res$sen.s)
  # df.gs.out <- rbind(df.gs.out, tempdf)
  # GPP results
  # tempdf <- data.frame(siteID=allsiteID[i],
  #                       PFT=sitepft,
  #                       NOY=mk.rgpp.res$NOY,
  #                       MK.p = mk.rgpp.res$MK.p,
  #                       sen.s = mk.rgpp.res$sen.s)
  # df.gpp.out <- rbind(df.gpp.out, tempdf)
  # LE results
  # tempdf <- data.frame(siteID=allsiteID[i],
  #                       PFT=sitepft,
  #                       NOY=mk.rle.res$NOY,
  #                       MK.p = mk.rle.res$MK.p,
  #                       sen.s = mk.rle.res$sen.s)
  # df.le.out <- rbind(df.le.out, tempdf)
  # # VPD results
  # tempdf <- data.frame(siteID=allsiteID[i],
  #                       PFT=sitepft,
  #                       NOY=mk.rvpd.res$NOY,
  #                       MK.p = mk.rvpd.res$MK.p,
  #                       sen.s = mk.rvpd.res$sen.s)
  # df.vpd.out <- rbind(df.vpd.out, tempdf)
  # Shortwave radiation results
  # tempdf <- data.frame(siteID=allsiteID[i],
  #                       PFT=sitepft,
  #                       NOY=mk.rsr.res$NOY,
  #                       MK.p = mk.rsr.res$MK.p,
  #                       sen.s = mk.rsr.res$sen.s)
  # df.sr.out <- rbind(df.sr.out, tempdf)
  # Ta results
  # tempdf <- data.frame(siteID=allsiteID[i],
  #                       PFT=sitepft,
  #                       NOY=mk.rta.res$NOY,
  #                       MK.p = mk.rta.res$MK.p,
  #                       sen.s = mk.rta.res$sen.s)
  # df.ta.out <- rbind(df.ta.out, tempdf)
  # Ca results
  # tempdf <- data.frame(siteID=allsiteID[i],
  #                       PFT=sitepft,
  #                       NOY=mk.rca.res$NOY,
  #                       MK.p = mk.rca.res$MK.p,
  #                       sen.s = mk.rca.res$sen.s)
  # df.ca.out <- rbind(df.ca.out, tempdf)
  # Precipitation
  # tempdf <- data.frame(siteID=allsiteID[i],
  #                      PFT=sitepft,
  #                      NOY=mk.rpre.res$NOY,
  #                      MK.p = mk.rpre.res$MK.p,
  #                      sen.s = mk.rpre.res$sen.s)
  # df.pre.out <- rbind(df.pre.out, tempdf)
  # Soil water content
  # tempdf <- data.frame(siteID=allsiteID[i],
  #                      PFT=sitepft,
  #                      NOY=mk.rswc.res$NOY,
  #                      MK.p = mk.rswc.res$MK.p,
  #                      sen.s = mk.rswc.res$sen.s)
  # df.swc.out <- rbind(df.swc.out, tempdf)
  ###
  
}

# Save the results --------------------------------------------------------

write.table(df.iwue.out, paste0(outfilepath1,'riwue_ann_trend_test1.csv'), row.names = F, sep = ',')
# write.table(df.gs.out, paste0(outfilepath1,'rgs_ann_trend.csv'), row.names = F, sep = ',')
# write.table(df.gpp.out, paste0(outfilepath1,'rgpp_ann_trend.csv'), row.names = F, sep = ',')
# write.table(df.le.out, paste0(outfilepath1,'rle_ann_trend.csv'), row.names = F, sep = ',')
# write.table(df.vpd.out, paste0(outfilepath1,'rvpd_ann_trend.csv'), row.names = F, sep = ',')
# write.table(df.ta.out, paste0(outfilepath1, 'rta_ann_trend.csv'), row.names = F, sep = ',')
# write.table(df.sr.out, paste0(outfilepath1, 'rsr_ann_trend.csv'), row.names = F, sep = ',')
# write.table(df.ca.out, paste0(outfilepath1,'rca_ann_trend.csv'), row.names = F, sep = ',')
# write.table(df.pre.out, paste0(outfilepath1,'rpre_ann_trend.csv'), row.names = F, sep = ',')
# write.table(df.swc.out, paste0(outfilepath1,'rswc_ann_trend.csv'), row.names = F, sep = ',')

