# estimate mean slope 
# using bootstrap method
# give weights by time series length
rm(list = ls())
library(boot)
library(ggplot2)


# Define functions --------------------------------------------------------

# mean
boot_meanslope_fun <- function(x,index) {
  meanslope <- mean(x[index], na.rm = T)
  return(meanslope)
}
# median
boot_medslope_fun <- function(x,index) {
  medslope <- median(x[index], na.rm = T)
  return(medslope)
}
#
boot_var_fun <- function(x, pft) {
  # x data frame
  pftx <- x[x$PFT==pft,]
  # x.bslope <- boot(pftx$sen.s, boot_meanslope_fun, R=5000)
  wx <- pftx$NOY/sum(pftx$NOY, na.rm=T) # set weights
  x.bslope <- boot(pftx$sen.s, boot_meanslope_fun, R=10000, weights = wx)
  # x.bslope <- boot(pftx$sen.s, boot_meanslope_fun, R=10000)
  x.bslope.ci <- boot.ci(x.bslope, type='bca')
  x.bslope.ci2 <- boot.ci(x.bslope, conf = 0.9, type='bca')
  # print results
  # print(x.bslope)
  # print(x.bslope.ci)
  x.bslope <- data.frame(slope = x.bslope$t0, 
                         cilow = x.bslope.ci$bca[4], 
                         cihigh = x.bslope.ci$bca[5], 
                         cilow2 = x.bslope.ci2$bca[4], 
                         cihigh2 = x.bslope.ci2$bca[5])
  #
  return(x.bslope)
}


# File paths and names ----------------------------------------------------

# riwue / rgpp / rgs / rle / rvpd / rta / rsr / rpre
infilepath1 <- '/Users/AbelGalois/Documents/MJ/Proj_WUE/results/SynFLX2015/'
tempname1 <- c('riwue_ann_trend.csv', 'rgpp_ann_trend.csv',
               'rgs_ann_trend.csv', 'rle_ann_trend.csv',
               'rca_ann_trend.csv', 'rvpd_ann_trend.csv',
               'rpre_ann_trend.csv', 'rsr_ann_trend.csv', 
               'rta_ann_trend.csv')

infilename.list <- list.files(infilepath1, pattern = 'r*_ann_trend')
infilename.list <- infilename.list[is.element(infilename.list, tempname1)]
# infilename1 <- paste0(infilepath1, tempname1)
outfilepath1 <- infilepath1
outfigpath1 <- '/Users/AbelGalois/Documents/MJ/Proj_WUE/results/SynFLX2015Fig/'


# Set parameters ----------------------------------------------------------

# different PFTs
pft <- c('DBF','ENF')
npft <- length(pft)
# the number of files
nfile <- length(infilename.list)
# the sites with no more than 5 site years according to IWUE trend data
setsite <- c('CA-NS2','CA-NS5','CZ-BK1','DE-Lkb','IT-SRo','US-Me6')


# Get trends for each site ------------------------------------------------

df.out <- data.frame()
for (i in 1:nfile) {
  # read data
  dataT <- read.table(paste0(infilepath1, infilename.list[i]), 
                      header = T, sep = ',', stringsAsFactors = F)
  # get the variable name
  pos <- regexpr('_', infilename.list[i])
  varname <- substr(infilename.list[i], 2, pos-1)
  # for each PFT
  for (j in 1:npft) {
    # x. depends on input variables
    # remove # Years < 7
    # dataT.boot <- dataT[dataT$NOY>=7&dataT$PFT==pft[j],]
    dataT.boot <- dataT[!is.element(dataT$siteID,setsite)&dataT$PFT==pft[j],]
    

# remove maximum and minimum values ---------------------------------------
    # for robust trend detection
    temptrend <- dataT.boot$sen.s
    dataT.boot <- dataT.boot[-c(which.max(temptrend),which.min(temptrend)),]

# Detect the mean trend ---------------------------------------------------

    
    x.bslope <- boot_var_fun(dataT.boot, pft[j])
    temp.df <- data.frame(var = varname,
                          PFT = pft[j],
                          slope = x.bslope[1],
                          low = x.bslope[2],
                          high = x.bslope[3],
                          low2 = x.bslope[4], 
                          high2 = x.bslope[5])
    ###
    df.out <- rbind (df.out, temp.df)
  }
  # only consider ENF and DBF forests
  ######

}

###
# save the results
write.table(df.out, paste0(outfilepath1, 'trend_boot_slope_allsites_rextreme.csv'),
            row.names = F, sep = ',')


#####
# Draw a histagram showing frequencies
# ggplot(data = dataT[dataT$PFT==pft,], aes(x=sen.s)) + 
#   geom_histogram(binwidth = 0.05, color='black', fill='white') + 
#   geom_vline(aes(xintercept=x.bslope[1]), linetype='dashed', color='blue') + 
#   geom_vline(aes(xintercept=x.bslope[2]), linetype='dashed', color='red') + 
#   geom_vline(aes(xintercept=x.bslope[3]), linetype='dashed', color='red') + 
#   ###
#   xlab(expression(Annual~Change('%'~yr^-1)))
  # Note: change names according to variables
###
# save the figures
# Note: change names according to variables
# ggsave(paste0(outfigpath1,'meanslope_hist_',pft,'_Rad.pdf'))
