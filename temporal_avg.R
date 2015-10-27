library(dplyr)
library(reshape)

options(stringsAsfactors=FALSE)

files <- list.files(path="~/LAMP-summary-stat/output/", pattern=glob2rx("tas_*.csv"), full.names=T)

read.csv.mystyle <- function(f) {
  a <- scan(f, what="complex", nmax=3, sep=",")
  h <- ifelse(a[[2]]=="time", TRUE, FALSE) 
  dat <- read.table(f, header=h, sep=",", stringsAsFactors=FALSE)  
}

d.list <- lapply(files, read.csv.mystyle)

d <- rbind_all(d.list)

#season.map <- read.csv("~/AndyJones/season_month_mapping.csv")

d$time <- as.double(d$time)
d$year <- floor(d$time)
d$month <- floor((d$time - d$year) * 12) + 1

#clean to keep only columns needed
d <- d[c("year", "month", "value", "units", "variable", "scenario", "model", "ensemble")]

# 
# results.grouped.base <- group_by( d.base, model, variable, scenario,ensemble, season )
# results.agg.base <- dplyr::summarise( results.grouped.base, 
#                                  value=mean( value, na.rm=T ), 
#                                  period = "base" )
# 
# d.fut <- subset(d, year >=2040 & year <= 2059)
# 
# results.grouped.fut <- group_by( d.fut, model, variable, scenario,ensemble, season )
# results.agg.fut <- dplyr::summarise( results.grouped.fut, 
#                                      value=mean( value, na.rm=T), 
#                                      period = "future")
# 
# results <- rbind(results.agg.fut, results.agg.base)
# 
# results.cast <- cast(results, model+variable+scenario+ensemble+season~period, value="value")
# 
# results.cast$value.diff <- results.cast$future - results.cast$base
# 
# write.csv(results.cast, "~/AndyJones/PminusE.csv", row.names=F)
