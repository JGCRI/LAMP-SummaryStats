library(dplyr)
library(reshape)

options(stringsAsfactors=FALSE)

files <- list.files(path="~/LAMP-summary-stat/output/", pattern=glob2rx("tas_*.csv"), full.names=T)

read.csv.mystyle <- function(f) {
  a <- scan(f, what="complex", nmax=3, sep=",")
  h <- ifelse(a[[2]]=="time", TRUE, FALSE) # some input files heave headers, some dont.
  dat <- read.table(f, header=h, sep=",", stringsAsFactors=FALSE)  
  colnames(dat) <- c("Z","time","value","lon", "lat", "units", 
                     "variable","scenario","type","model", "ensemble")
  return(dat)
}

d.list <- lapply(files, read.csv.mystyle)
d <- rbind_all(d.list)

#extract year and month from time 
d$time <- as.double(d$time)
d$year <- floor(d$time)
d$month <- floor((d$time - d$year) * 12) + 1

#clean to keep only columns needed
d <- d[c("year", "month", "value", "units", "variable", "scenario", "model", "ensemble")]

d <- as.data.frame(d)

# write.csv(d, "temperature.csv")

###########################
#############################
##############################

#DECADAL MEAN

d <- read.csv("temperature.csv")

# Split data into decades
d$decade <- cut(d$year, breaks=seq(1850, 2100, 10), dig.lab=5, include.lowest=T, label=seq(1860,2100,10))

# calculate decadal mean for each month
results.grouped <- group_by( d, model, scenario, ensemble, month, decade )
results <- dplyr::summarise( results.grouped, 
                             decadal.mean=mean(value))
results <- as.data.frame(results)

# write.csv(results, file="temperature-decadalmean.csv", row.names=F)

#match in decadal mean into d
d$decadal.mean <- results [ match(paste0( d$month, d$scenario, d$model, d$ensemble, d$decade ), 
                                  paste0( results$month, results$scenario, results$model, results$ensemble, results$decade )), "decadal.mean"]

# calculate decadal variance. 
d$variance <- (d$value - d$decadal.mean)^2

results.grouped <- group_by( d, model, scenario, ensemble, month, decade )
results.var <- dplyr::summarise( results.grouped, 
                             decadal.variance=sum(variance))
results.var <- as.data.frame(results.var)

results.var$decadal.variance <- as.numeric(results.var$decadal.variance) / 120

# match decadal mean back into table with decadal variance (how to do in one step in dplyr?)
results.var$decadal.mean <- results [ match(paste0( results.var$month, results.var$scenario, results.var$model, results.var$ensemble, results.var$decade ), 
                                            paste0( results$month, results$scenario, results$model, results$ensemble, results$decade )), "decadal.mean"]

# write.csv
###########################################
###########################################
###########################################
## RUNOFF ###


r <- read.csv("~/LAMP-SummaryStats/runoff.csv")

r$decade <- cut(r$year, breaks=seq(1950, 2100, 10), dig.lab=5, include.lowest=T, label=seq(1960,2100,10))

results.grouped <- group_by( r, model, scenario, ensemble, month, decade )
results <- dplyr::summarise( results.grouped, 
                             decadal.mean=mean(value))

write.csv(results, file="runoff-decadalmean.csv", row.names=F)

##############################################
###############################################
###Figures Decadal Means####

d <- read.csv("runoff-decadalmean.csv")

scenario <- c("rcp26", "rcp45", "rcp60", "rcp85")
month <- 1:12


for (s in scenario){
  for (t in month){
    
    d.fig <- subset(d, month == t & (scenario == s|scenario=="historical"))
    
    p <- ggplot() + geom_line(data=d.fig, aes(decade, decadal.mean, color=model), size=1.25) + 
      scale_x_continuous(limits=c(1960,2100), breaks = seq(1960,2100, by=20), expand = c(0,0)) +
      scale_y_continuous(limits=c(0,0.25), breaks = seq(0,0.5, by=0.05), expand = c(0, 0)) #+
    #coord_cartesian(xlim=c(2010, 2250))+ coord_cartesian(ylim=c(0,10))
    p <- p + ylab("Decadal Monthly Mean Runoff") + xlab(paste0 ("month:", t)) + 

      
      scale_colour_manual(name = "model", values = color.1) +
      
      theme( legend.position= "right",
             legend.text = element_text(size = 18), 
             axis.text.x = element_text(size = 30, color="black", angle = 90, hjust=0.5, vjust=0.5), 
             axis.text.y = element_text(size = 34, color="black"),
             #legend.title = element_blank(),
             plot.title = element_text(size = 56, face="bold"),
             axis.title.x = element_blank(), 
             axis.title.y = element_text(size = 36, face="bold", vjust=0.5), 
             # legend.key = element_blank(),
             legend.title = element_blank(),
             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             panel.border  = element_rect(color="black", fill=NA))
    
    ggsave(filename = paste0("runoff_", s, "_", t, ".png"),  width = 400, height = 297, units = "mm" ) 
  }
}
###########
color.1 = c("#000000","#1CE6FF","#FF34FF","#FF4A46","#008941","#006FA6","#A30059","#FFDBE5","#7A4900","#0000A6",
            "#63FFAC","#B79762","#004D43","#8FB0FF","#997D87","#5A0007","#809693","#FEFFE6","#1B4400","#4FC601","#3B5DFF",
            "#4A3B53","#FF2F80","#61615A","#BA0900","#6B7900","#00C2A0","#FFAA92","#FF90C9","#B903AA","#D16100","#DDEFFF",
            "#000035","#7B4F4B","#A1C299","#300018","#0AA6D8","#013349","#00846F","#372101","#FFB500","#C2FFED","#A079BF",
            "#CC0744","#C0B9B2","#C2FF99","#001E09","#00489C","#6F0062","#0CBD66","#EEC3FF","#456D75","#B77B68","#7A87A1",
            "#788D66","#885578","#FAD09F","#FF8A9A","#D157A0","#BEC459","#456648","#0086ED","#886F4C","#34362D","#B4A8BD",
            "#00A6AA","#452C2C","#636375","#A3C8C9","#FF913F","#938A81","#575329","#00FECF","#B05B6F","#8CD0FF","#3B9700",
            "#04F757","#C8A1A1","#1E6E00","#7900D7","#A77500","#6367A9","#A05837","#6B002C","#772600","#D790FF","#9B9700",
            "#549E79","#FFF69F","#201625","#72418F","#BC23FF","#99ADC0","#3A2465","#922329","#5B4534","#FDE8DC","#404E55",
            "#0089A3","#CB7E98","#A4E804","#324E72","#6A3A4C","#83AB58","#001C1E","#D1F7CE","#004B28","#C8D0F6","#A3A489",
            "#806C66","#222800","#BF5650","#E83000","#66796D","#DA007C","#FF1A59","#8ADBB4","#1E0200","#5B4E51","#C895C5",
            "#320033","#FF6832","#66E1D3","#CFCDAC","#D0AC94","#7ED379","#012C58","#7A7BFF","#D68E01","#353339","#78AFA1",
            "#FEB2C6","#75797C","#837393","#943A4D","#B5F4FF","#D2DCD5","#9556BD","#6A714A","#001325","#02525F","#0AA3F7",
            "#E98176","#DBD5DD","#5EBCD1","#3D4F44","#7E6405","#02684E","#962B75","#8D8546","#9695C5","#E773CE","#D86A78",
            "#3E89BE","#CA834E","#518A87","#5B113C","#55813B","#E704C4","#00005F","#A97399","#4B8160","#59738A","#FF5DA7",
            "#F7C9BF","#643127","#513A01","#6B94AA","#51A058","#A45B02","#1D1702","#E20027","#E7AB63","#4C6001","#9C6966",
            "#64547B","#97979E","#006A66","#391406","#F4D749","#0045D2","#006C31","#DDB6D0","#7C6571","#9FB2A4","#00D891",
            "#15A08A","#BC65E9","#FFFFFE","#C6DC99","#203B3C","#671190","#6B3A64","#F5E1FF","#FFA0F2","#CCAA35","#374527",
            "#8BB400","#797868","#C6005A","#3B000A","#C86240","#29607C","#402334","#7D5A44","#CCB87C","#B88183","#AA5199",
            "#B5D6C3","#A38469","#9F94F0","#A74571","#B894A6","#71BB8C","#00B433","#789EC9","#6D80BA","#953F00","#5EFF03",
            "#E4FFFC","#1BE177","#BCB1E5","#76912F","#003109","#0060CD","#D20096","#895563","#29201D","#5B3213","#A76F42",
            "#89412E","#1A3A2A","#494B5A","#A88C85","#F4ABAA","#A3F3AB","#00C6C8","#EA8B66","#958A9F","#BDC9D2","#9FA064",
            "#BE4700","#658188","#83A485","#453C23","#47675D","#3A3F00","#061203","#DFFB71","#868E7E","#98D058","#6C8F7D",
            "#D7BFC2","#3C3E6E","#D83D66","#2F5D9B","#6C5E46","#D25B88","#5B656C","#00B57F","#545C46","#866097","#365D25",
            "#252F99","#00CCFF","#674E60","#FC009C","#92896B")

#################

### temperature charts


d <- read.csv("temperature-decadalmean.csv")

scenario <- c("rcp26", "rcp45", "rcp60", "rcp85")
month <- 1:12


for (s in scenario){
  for (t in month){
    
    d.fig <- subset(d, month == t & ensemble=="r1i1p1" & (scenario == s|scenario=="historical"))
    
    p <- ggplot() + geom_line(data=d.fig, aes(decade, decadal.mean, color=model), size=1.25) + 
      scale_x_continuous(limits=c(1860,2100), breaks = seq(1860,2100, by=40), expand = c(0,0)) +
      scale_y_continuous(limits=c(290,310), breaks = seq(290,310, by=2), expand = c(0, 0)) #+
    #coord_cartesian(xlim=c(2010, 2250))+ coord_cartesian(ylim=c(0,10))
    p <- p + ylab("Decadal Monthly Mean Runoff") + xlab(paste0 ("month:", t)) + 
      
      scale_colour_manual(name = "model", values = color.1) +
      
      theme( legend.position= "right",
             legend.text = element_text(size = 18), 
             axis.text.x = element_text(size = 30, color="black", angle = 90, hjust=0.5, vjust=0.5), 
             axis.text.y = element_text(size = 34, color="black"),
             #legend.title = element_blank(),
             plot.title = element_text(size = 56, face="bold"),
             axis.title.x = element_blank(), 
             axis.title.y = element_text(size = 36, face="bold", vjust=0.5), 
             # legend.key = element_blank(),
             legend.title = element_blank(),
             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             panel.border  = element_rect(color="black", fill=NA))
    
    ggsave(filename = paste0("temperature_", s, "_", t, ".png"),  width = 400, height = 297, units = "mm" ) 
  }
}


#################################
## Variance ###

d$decade <- cut(d$year, breaks=seq(1850, 2100, 10), dig.lab=5, include.lowest=T, label=seq(1860,2100,10))


results.grouped <- group_by( r, model, scenario, ensemble, month, decade )
results <- dplyr::summarise( results.grouped, 
                             decadal.mean=mean(value))


