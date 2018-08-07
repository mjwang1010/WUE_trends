# prepare data for trend test
# annual mean IWUE and other related variables
# or relative variables to the mean annual value
# or anomalies to the mean annual value
###
rm(list=ls())
library(ggplot2)
library(lubridate)


# Paths and names ---------------------------------------------------------

# input file names 
infilename1 <- '/Users/AbelGalois/Documents/MJ/Proj_WUE/results/SynFLX2015/peakmonth_hr_iwue_allsites1.csv'
infilename2 <- '/Users/AbelGalois/Documents/MJ/Proj_WUE/results/SynFLX2015/peakmonth_hr_wue_allsites1.csv'

outfilepath1 <- '/Users/AbelGalois/Documents/MJ/Proj_WUE/results/SynFLX2015/'
outfigpath1 <- '/Users/AbelGalois/Documents/MJ/Proj_WUE/results/SynFLX2015Fig/'



# Read data ---------------------------------------------------------------

# estimate iWUE trend in summer
# Read data
dataT <- read.table(infilename1, header = T, sep = ',', stringsAsFactors = F)
dataT2 <- read.table(infilename2, header = T, sep = ',', stringsAsFactors = F) # get wind speed

allsiteID <- unique(dataT$siteID)
nsite <- length(allsiteID) # the number of sites

iWUE_peak <- dataT$iWUE_hr
Gs_peak <- dataT$Gs_mol
GPP_peak <- dataT$GPP_hr
VPD_peak <- dataT$VPD_hr
LE_peak <- dataT$LE_hr
Ca_peak <- dataT$Ca_hr
VPD_peak <- dataT$VPD_hr
SR_peak <- dataT$SW_hr
SWC_peak <- dataT$SWC_hr
Ta_peak <- dataT$Ta_hr
Pre_peak <- dataT$P_hr
time_peak <- dataT$time_start
# wind speed data
u_peak <- dataT2$u_hr


###

ann.iwue.df <- data.frame()
ann.gs.df <- data.frame()
ann.gpp.df <- data.frame()
ann.le.df <- data.frame()
ann.vpd.df <- data.frame()
ann.ta.df <- data.frame()
ann.sr.df <- data.frame()
ann.swc.df <- data.frame()
ann.pre.df <- data.frame()
ann.ca.df <- data.frame()
ann.u.df <- data.frame()

ann.temp.df <- data.frame()

for (i in 1:nsite) {
  ###
  # a case test
  siteiwue <- iWUE_peak[dataT$siteID==allsiteID[i]]
  sitegs <- Gs_peak[dataT$siteID==allsiteID[i]]
  sitegpp <- GPP_peak[dataT$siteID==allsiteID[i]]
  sitele <- LE_peak[dataT$siteID==allsiteID[i]]
  sitevpd <- VPD_peak[dataT$siteID==allsiteID[i]]
  siteta <- Ta_peak[dataT$siteID==allsiteID[i]]
  siteca <- Ca_peak[dataT$siteID==allsiteID[i]]
  sitesr <- SR_peak[dataT$siteID==allsiteID[i]]
  sitepre <- Pre_peak[dataT$siteID==allsiteID[i]]
  siteswc <- SWC_peak[dataT$siteID==allsiteID[i]]
  # 
  siteu <- u_peak[dataT$siteID==allsiteID[i]]
  siteu[is.na(siteiwue)] <- NA
  #
  sitetime <- time_peak[dataT$siteID==allsiteID[i]]
  sitepft <- unique(dataT$PFT[dataT$siteID==allsiteID[i]])
  ###
  # get year, month and day
  sitetime.gmt <- strptime(sitetime, '%Y%m%d%H%M')
  siteyear.hr <- year(sitetime.gmt)
  sitemonth.hr <- month(sitetime.gmt)
  siteyear <- unique(siteyear.hr[complete.cases(siteyear.hr)]) # remove NAs in the years
  
  # estimate each annual mean IWUE
  agg.mean.iwue.res <- aggregate(siteiwue, by=list(siteyear.hr), FUN='mean', na.rm=T)
  # agg.std.iwue.res <- aggregate(siteiwue, by=list(siteyear.hr), FUN='sd', na.rm=T)
  # 
  # agg.mean.gs.res <- aggregate(sitegs, by=list(siteyear.hr), FUN='mean', na.rm=T)
  # agg.std.gs.res <- aggregate(sitegs, by=list(siteyear.hr), FUN='sd', na.rm=T)
  # 
  # agg.mean.gpp.res <- aggregate(sitegpp, by=list(siteyear.hr), FUN='mean', na.rm=T)
  # agg.std.gpp.res <- aggregate(sitegpp, by=list(siteyear.hr), FUN='sd', na.rm=T)
  # 
  # agg.mean.le.res <- aggregate(sitele, by=list(siteyear.hr), FUN='mean', na.rm=T)
  # agg.std.le.res <- aggregate(sitele, by=list(siteyear.hr), FUN='sd', na.rm=T)
  # 
  # agg.mean.vpd.res <- aggregate(sitevpd, by=list(siteyear.hr), FUN='mean', na.rm=T)
  # agg.std.vpd.res <- aggregate(sitevpd, by=list(siteyear.hr), FUN='sd', na.rm=T)
  # 
  # agg.mean.ta.res <- aggregate(siteta, by=list(siteyear.hr), FUN='mean', na.rm=T)
  # agg.std.ta.res <- aggregate(siteta, by=list(siteyear.hr), FUN='sd', na.rm=T)
  # 
  # agg.mean.sr.res <- aggregate(sitesr, by=list(siteyear.hr), FUN='mean', na.rm=T)
  # agg.std.sr.res <- aggregate(sitesr, by=list(siteyear.hr), FUN='sd', na.rm=T)
  # 
  # agg.mean.ca.res <- aggregate(siteca, by=list(siteyear.hr), FUN='mean', na.rm=T)
  # agg.std.ca.res <- aggregate(siteca, by=list(siteyear.hr), FUN='sd', na.rm=T)
  # 
  # agg.mean.pre.res <- aggregate(sitepre, by=list(siteyear.hr), FUN='mean', na.rm=T)
  # agg.std.pre.res <- aggregate(sitepre, by=list(siteyear.hr), FUN='sd', na.rm=T)
  
  # agg.mean.swc.res <- aggregate(siteswc, by=list(siteyear.hr), FUN='mean', na.rm=T)
  # agg.std.swc.res <- aggregate(siteswc, by=list(siteyear.hr), FUN='sd', na.rm=T)
  
  agg.mean.u.res <- aggregate(siteu, by=list(siteyear.hr), FUN='mean', na.rm=T)
  
  ###
  # give names
  colnames(agg.mean.iwue.res) <- c('year', 'iwue')
  # colnames(agg.std.iwue.res) <- c('year', 'iwue')
  # colnames(agg.mean.gs.res) <- c('year', 'gs')
  # colnames(agg.std.gs.res) <- c('year', 'gs')
  # colnames(agg.mean.gpp.res) <- c('year', 'gpp')
  # colnames(agg.std.gpp.res) <- c('year', 'gpp')
  # colnames(agg.mean.le.res) <- c('year', 'le')
  # colnames(agg.std.le.res) <- c('year', 'le')
  # colnames(agg.mean.vpd.res) <- c('year', 'vpd')
  # colnames(agg.std.vpd.res) <- c('year', 'vpd')
  # colnames(agg.mean.ta.res) <- c('year', 'ta')
  # colnames(agg.std.ta.res) <- c('year', 'ta')
  # colnames(agg.mean.sr.res) <- c('year', 'sr')
  # colnames(agg.std.sr.res) <- c('year', 'sr')
  # colnames(agg.mean.ca.res) <- c('year', 'ca')
  # colnames(agg.std.ca.res) <- c('year', 'ca')
  # colnames(agg.mean.pre.res) <- c('year', 'pre')
  # colnames(agg.std.pre.res) <- c('year', 'pre')
  # colnames(agg.mean.swc.res) <- c('year', 'swc')
  colnames(agg.mean.u.res) <- c('year', 'u')

  ###
  # remove sites without enough data
  if (sum(!is.na(agg.mean.iwue.res$iwue)) < 5) {
    print(paste0(allsiteID[i], ': less than 5 data'))
    next
  }
  # if (sum(!is.na(agg.mean.gs.res$gs)) < 7) {
  #   print(paste0(allsiteID[i], ': less than 7 data'))
  #   next
  # }
  # if (sum(!is.na(agg.mean.gpp.res$gpp)) < 7) {
  #   print(paste0(allsiteID[i], ': less than 7 data'))
  #   next
  # }
  # if (sum(!is.na(agg.mean.le.res$le)) < 7) {
  #   print(paste0(allsiteID[i], ': less than 7 data'))
  #   next
  # }
  # if (sum(!is.na(agg.mean.vpd.res$vpd)) < 7) {
  #   print(paste0(allsiteID[i], ': less than 7 data'))
  #   next
  # }
  # if (sum(!is.na(agg.mean.ta.res$ta)) < 7) {
  #   print(paste0(allsiteID[i], ': less than 7 data'))
  #   next
  # }
  # if (sum(!is.na(agg.mean.sr.res$sr)) < 7) {
  #   print(paste0(allsiteID[i], ': less than 7 data'))
  #   next
  # }
  # if (sum(!is.na(agg.mean.ca.res$ca)) < 7) {
  #   print(paste0(allsiteID[i], ': less than 7 data'))
  #   next
  # }
  # if (sum(!is.na(agg.mean.pre.res$pre)) < 7) {
  #   print(paste0(allsiteID[i], ': less than 7 data'))
  #   next
  # }
  # if (sum(!is.na(agg.mean.swc.res$swc)) < 7) {
  #   next
  # }
  # wind speed
  if (sum(!is.na(agg.mean.u.res$u)) < 5) {
    next
  }
  
  ###
  ## estimate the ratio
  ratio_fun <- function(agg.mean.x.res, varname) {
    mean.ann.x <- mean(agg.mean.x.res[,varname], na.rm=T)
    ratio.x <- (agg.mean.x.res[,varname]-mean.ann.x)/mean.ann.x*100
    ratio.x <- as.data.frame(ratio.x)
    colnames(ratio.x) <- paste0('r',varname)
    ratio.x.res <- cbind(agg.mean.x.res, ratio.x)
  }
  annual.riwue.res <- ratio_fun(agg.mean.iwue.res, 'iwue')
  # annual.rgpp.res <- ratio_fun(agg.mean.gpp.res, 'gpp')
  # annual.rle.res <- ratio_fun(agg.mean.le.res, 'le')
  # annual.rgs.res <- ratio_fun(agg.mean.gs.res, 'gs')
  # annual.rvpd.res <- ratio_fun(agg.mean.vpd.res, 'vpd')
  # annual.rta.res <- ratio_fun(agg.mean.ta.res, 'ta')
  # annual.rsr.res <- ratio_fun(agg.mean.sr.res, 'sr')
  # annual.rca.res <- ratio_fun(agg.mean.ca.res, 'ca')
  # annual.rpre.res <- ratio_fun(agg.mean.pre.res, 'pre')
  annual.ru.res <- ratio_fun(agg.mean.u.res, 'u')
  
  ###
  ## estimate the anomalies
  # anom_fun <- function(agg.mean.x.res, varname) {
  #   mean.ann.x <- mean(agg.mean.x.res[,varname], na.rm=T)
  #   anom.x <- agg.mean.x.res[,varname]-mean.ann.x
  #   anom.x <- as.data.frame(anom.x)
  #   colnames(anom.x) <- paste0('a',varname)
  #   anom.x.res <- cbind(agg.mean.x.res, anom.x)
  # }
  # annual.aiwue.res <- anom_fun(agg.mean.iwue.res, 'iwue')
  # annual.agpp.res <- anom_fun(agg.mean.gpp.res, 'gpp')
  # annual.ale.res <- anom_fun(agg.mean.le.res, 'le')
  # annual.ags.res <- anom_fun(agg.mean.gs.res, 'gs')
  # annual.avpd.res <- anom_fun(agg.mean.vpd.res, 'vpd')
  # annual.ata.res <- anom_fun(agg.mean.ta.res, 'ta')
  # annual.asr.res <- anom_fun(agg.mean.sr.res, 'sr')
  # annual.aca.res <- anom_fun(agg.mean.ca.res, 'ca')
  # annual.aswc.res <- anom_fun(agg.mean.swc.res, 'swc')
  
  #####
  # # relative IWUE
  temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
                        PFT = rep(sitepft,length(siteyear)),
                        year = siteyear,
                        ratio = annual.riwue.res$riwue)
  ann.iwue.df <- rbind(ann.iwue.df, temp.df)
  # # relative Gs
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       ratio = annual.rgs.res$rgs)
  # ann.gs.df <- rbind(ann.gs.df, temp.df)
  # # relative GPP
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       ratio = annual.rgpp.res$rgpp)
  # ann.gpp.df <- rbind(ann.gpp.df, temp.df)
  # # relative LE
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       ratio = annual.rle.res$rle)
  # ann.le.df <- rbind(ann.le.df, temp.df)
  # # relative VPD
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       ratio = annual.rvpd.res$rvpd)
  # ann.vpd.df <- rbind(ann.vpd.df, temp.df)
  # # relative Ta
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       ratio = annual.rta.res$rta)
  # ann.ta.df <- rbind(ann.ta.df, temp.df)
  # # relative SRadiation
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       ratio = annual.rsr.res$rsr)
  # ann.sr.df <- rbind(ann.sr.df, temp.df)
  # # relavtive Ca
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       ratio = annual.rca.res$rca)
  # ann.ca.df <- rbind(ann.ca.df, temp.df)
  # # relative precipitation
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)),
  #                       year = siteyear,
  #                       ratio = annual.rpre.res$rpre)
  # ann.pre.df <- rbind(ann.pre.df, temp.df)
  temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
                        PFT = rep(sitepft,length(siteyear)),
                        year = siteyear,
                        ratio = annual.ru.res$ru)
  ann.u.df <- rbind(ann.u.df, temp.df)
  
  ######
  # # anomalous IWUE
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)),
  #                       year = siteyear,
  #                       anom = annual.aiwue.res$aiwue)
  # ann.iwue.df <- rbind(ann.iwue.df, temp.df)
  # # anomalous Gs
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       anom = annual.ags.res$ags)
  # ann.gs.df <- rbind(ann.gs.df, temp.df)
  # # anomalous GPP
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       anom = annual.agpp.res$agpp)
  # ann.gpp.df <- rbind(ann.gpp.df, temp.df)
  # # anomalous LE
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       anom = annual.ale.res$ale)
  # ann.le.df <- rbind(ann.le.df, temp.df)
  # # anomalous VPD
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       anom = annual.avpd.res$avpd)
  # ann.vpd.df <- rbind(ann.vpd.df, temp.df)
  # # anomalous Ta
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       anom = annual.ata.res$ata)
  # ann.ta.df <- rbind(ann.ta.df, temp.df)
  # # anomalous Solar Radiation
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)), 
  #                       year = siteyear,
  #                       anom = annual.asr.res$asr)
  # ann.sr.df <- rbind(ann.sr.df, temp.df)
  # # anomalous Ca
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)),
  #                       year = siteyear,
  #                       anom = annual.aca.res$aca)
  # ann.ca.df <- rbind(ann.ca.df, temp.df)
  #
  # temp.df <- data.frame(siteID = rep(allsiteID[i],length(siteyear)),
  #                       PFT = rep(sitepft,length(siteyear)),
  #                       year = siteyear,
  #                       anom = annual.aswc.res$aswc)
  # ann.swc.df <- rbind(ann.swc.df, temp.df)
  ###
  # annual mean
  #
  # temp.df <- data.frame(siteID = rep(allsiteID[i], length(siteyear)), 
  #                       PFT = rep(sitepft, length(siteyear)), 
  #                       year = siteyear, 
  #                       value = agg.mean.ta.res$ta)
  # ann.temp.df <- rbind(ann.temp.df, temp.df)
}

###
# save the results
write.table(ann.iwue.df, paste0(outfilepath1,'riwue_ann_ratio_test2.csv'),
            row.names = F, sep = ',')
# write.table(ann.gs.df, paste0(outfilepath1,'ags_ann_anom.csv'),
#             row.names = F, sep = ',')
# write.table(ann.gpp.df, paste0(outfilepath1,'agpp_ann_anom.csv'),
#             row.names = F, sep = ',')
# write.table(ann.le.df, paste0(outfilepath1,'ale_ann_anom.csv'),
#             row.names = F, sep = ',')
# write.table(ann.vpd.df, paste0(outfilepath1,'avpd_ann_anom.csv'),
#             row.names = F, sep = ',')
# write.table(ann.ta.df, paste0(outfilepath1,'ata_ann_anom.csv'),
#             row.names = F, sep = ',')
# write.table(ann.sr.df, paste0(outfilepath1,'asr_ann_anom.csv'),
#             row.names = F, sep = ',')
# write.table(ann.ca.df, paste0(outfilepath1,'aca_ann_anom.csv'),
#             row.names = F, sep = ',')
# write.table(ann.swc.df, paste0(outfilepath1,'aswc_ann_anom.csv'),
#             row.names = F, sep = ',')

write.table(ann.u.df, paste0(outfilepath1, 'ru_ann_ratio_test1.csv'), 
            row.names = F, sep = ',')

#####
### plotting
# pft.plot <- unique(ann.iwue.df$PFT)
# npft <- length(pft.plot)
# for (i in 1:npft) {
#   df.plot <- ann.iwue.df[ann.iwue.df$PFT==pft.plot[i],]
#   p1 <- ggplot(data = df.plot, aes(x = year, y = mean)) +
#     geom_point() + 
#     geom_smooth(method = 'lm') + 
#     ylab(expression(IWUE~(mu*mol~mol^-1)))+
#     theme_bw() + 
#     theme(panel.grid.minor = element_blank()) + 
#     facet_wrap(~siteID, ncol=3)
#   #
#   ggsave(paste0(outfigpath1, 'IAV_IWUE_',pft.plot[i],'.pdf'),
#          plot = p1)
# }
# ###
# # function for plotting
# plot_IAV_site_fun <- function(x, y, path) {
#   # x - record annual mean variables
#   # y - char, variable names ('IWUE', 'GPP', and 'Gs')
#   # path - paths for saving figures
#   ###
#   # data frame (x) includes variables:
#   # - siteID
#   # - PFT
#   # - year
#   # - mean
#   # - std
#   if (y=='IWUE') {
#     var.expr <- expression(IWUE~(mu*mol~mol^-1))
#   } else if (y=='GPP') {
#     var.expr <- expression(GPP~(mu*molCO[2]~m^-2~s^-1))
#   } else {
#     var.expr <- expression(Gs~(mol~m^-2~s^-1))
#   }
#   pft.plot <- unique(x$PFT)
#   npft <- length(pft.plot)
#   for (i in 1:npft) {
#     df.plot <- x[x$PFT==pft.plot[i],]
#     p1 <- ggplot(data = df.plot, aes(x = year, y = mean)) +
#       geom_point() + 
#       geom_smooth(method = 'lm') + 
#       ylab(var.expr)+
#       theme_bw() + 
#       theme(panel.grid.minor = element_blank()) + 
#       facet_wrap(~siteID, ncol=3)
#     #
#     ggsave(paste0(path,'IAV_',y,'_',pft.plot[i],'.pdf'),
#            plot = p1)
#   }
# }
# ###
# # plot using functions
# plot_IAV_site_fun(ann.gpp.df, 'GPP', outfigpath1)
# plot_IAV_site_fun(ann.gs.df, 'Gs', outfigpath1)
