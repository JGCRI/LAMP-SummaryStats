library(dplyr)
library(reshape)

options(stringsAsfactors=FALSE)

# all temperature files (temperature data is spread over different files)
files <- list.files(path="~/LAMP-summary-stat/output/", pattern=glob2rx("tas_*.csv"), full.names=T)

## function reads in csv file based on whether it has a column names specified or not
# by checking if the first row has "time" as a column name for the second column.
# f: filename 
read.csv.mystyle <- function(f) {
  a <- scan(f, what="complex", nmax=3, sep=",")
  h <- ifelse(a[[2]]=="time", TRUE, FALSE) # some input files heave headers, some dont, my bad.
  dat <- read.table(f, header=h, sep=",", stringsAsFactors=FALSE)  
  colnames(dat) <- c("Z","time","value","lon", "lat", "units", 
                     "variable","scenario","type","model", "ensemble")
  return(dat)
}

# read all the files with temperature data and make a list
d.list <- lapply(files, read.csv.mystyle)
# combine items of above list into one data frame
d <- rbind_all(d.list)

#extract year and month from time 
d$time <- as.double(d$time)
d$year <- floor(d$time)
d$month <- floor((d$time - d$year) * 12) + 1

#clean to keep only columns needed
d <- d[c("year", "month", "value", "units", "variable", "scenario", "model", "ensemble")]

# rbind_all gives a dplyr data frame which has some strange idiosyncrasies. convert to normal df
d <- as.data.frame(d) 

# write out temperature data. 
write.csv(d, "temperature.csv") 

###########################
#############################
##############################

#DECADAL MEAN

# Split data into decades. cmpi5 temperature starts in 1850.
d1 <- d
d1 <- subset(d1, year < 2101)
d1$decade <- cut(d1$year, breaks=seq(1850, 2100, 10), dig.lab=5, include.lowest=T, label=seq(1860,2100,10))

# calculate decadal mean by month for each month
results.grouped <- group_by( d1, model, scenario, ensemble, month, decade )
results <- dplyr::summarise( results.grouped, 
                             decadal.mean.bymonth=mean(value))

# summarise gives a dplyr data frame which has some strange idiosyncrasies. convert to normal df
results <- as.data.frame(results)

#match in decadal mean by month into d
d1$decadal.mean.bymonth <- results [ match(paste0( d1$month, d1$scenario, d1$model, d1$ensemble, d1$decade ), 
                                  paste0( results$month, results$scenario, results$model, results$ensemble, results$decade )), "decadal.mean.bymonth"]

# calculate seasonally adjusted variance 
d1$variance <- (d1$value - d1$decadal.mean.bymonth)^2

results.grouped <- group_by( d1, model, scenario, ensemble,  decade )
results.var <- dplyr::summarise( results.grouped, 
                                 seasonally.adjusted.variance=sum(variance))
results.var <- as.data.frame(results.var)

results.var$seasonally.adjusted.variance <- results.var$seasonally.adjusted.variance / 120

## calculate total, actual decadal mean
d2 <- d
d2$decade <- cut(d2$year, breaks=seq(1850, 2100, 10), dig.lab=5, include.lowest=T, label=seq(1860,2100,10))

results.grouped <- group_by( d2, model, scenario, ensemble, decade )
results <- dplyr::summarise( results.grouped, 
                             decadal.mean=mean(value))
results<- as.data.frame(results)

# match decadal mean back into table with decadal variance (how to do in one step in dplyr?)
results.var$decadal.mean <- results [ match(paste0(  results.var$scenario, results.var$model, results.var$ensemble, results.var$decade ), 
                                            paste0(  results$scenario, results$model, results$ensemble, results$decade )), "decadal.mean"]

results.var$seasonally.adjusted.standard.deviation <- sqrt(results.var$seasonally.adjusted.variance)
results.var$seasonally.adjusted.variance <- NULL

write.csv(results.var, file="decadal-temperature.csv", row.names=F)
###########################################
###########################################
###########################################

## RUNOFF ###

r <- read.csv("c:/LAMP-SummaryStats/runoff.csv")

# Split data into decades. runoff starts in 1950
d1 <- r
d1$decade <- cut(d1$year, breaks=seq(1950, 2100, 10), dig.lab=5, include.lowest=T, label=seq(1960,2100,10))

# calculate decadal mean by month for each month
results.grouped <- group_by( d1, model, scenario, ensemble, month, decade )
results <- dplyr::summarise( results.grouped, 
                             decadal.mean.bymonth=mean(value))

# summarise gives a dplyr data frame which has some strange idiosyncrasies. convert to normal df
results <- as.data.frame(results)

#match in decadal mean by month into d
d1$decadal.mean.bymonth <- results [ match(paste0( d1$month, d1$scenario, d1$model, d1$ensemble, d1$decade ), 
                                           paste0( results$month, results$scenario, results$model, results$ensemble, results$decade )), "decadal.mean.bymonth"]

# calculate seasonally adjusted variance 
d1$variance <- (d1$value - d1$decadal.mean.bymonth)^2

results.grouped <- group_by( d1, model, scenario, ensemble, decade )
results.var <- dplyr::summarise( results.grouped, 
                                 seasonally.adjusted.variance=sum(variance))
results.var <- as.data.frame(results.var)

results.var$seasonally.adjusted.variance <- results.var$seasonally.adjusted.variance / 120

## calculate total, actual decadal mean
d2 <- r
d2$decade <- cut(d2$year, breaks=seq(1950, 2100, 10), dig.lab=5, include.lowest=T, label=seq(1960,2100,10))

results.grouped <- group_by( d2, model, scenario, ensemble, decade )
results <- dplyr::summarise( results.grouped, 
                             decadal.mean=mean(value))
results<- as.data.frame(results)

# match decadal mean back into table with decadal variance (how to do in one step in dplyr?)
results.var$decadal.mean <- results [ match(paste0(  results.var$scenario, results.var$model, results.var$ensemble, results.var$decade ), 
                                            paste0(  results$scenario, results$model, results$ensemble, results$decade )), "decadal.mean"]
results.var$seasonally.adjusted.standard.deviation <- sqrt(results.var$seasonally.adjusted.variance)
results.var$seasonally.adjusted.variance <- NULL


write.csv(results.var, file="decadal-runoff.csv", row.names=F)

##############################################
###############################################








