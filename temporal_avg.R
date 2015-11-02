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

#extract year and month from time 
d$time <- as.double(d$time)
d$year <- floor(d$time)
d$month <- floor((d$time - d$year) * 12) + 1

#clean to keep only columns needed
d <- d[c("year", "month", "value", "units", "variable", "scenario", "model", "ensemble")]

d <- as.data.frame(d)

# write.csv(d, file="~/LAMP-summary-stat/temperature.csv", row.names=F)

##########################################
# rolling decadal average -- doesnt work.
decadal.mean <- function(d, width=10, align="right", partial=TRUE){
  res <- rollapply(d$value, width, align=align, partial=TRUE, FUN="mean")
}


tmp <- rollapply.mystyle()

results.grouped <- group_by( d, model, scenario, ensemble, month )
results <- dplyr::summarise( results.grouped, 
                             value=rollapply.mystyle(d))

d <- d[with(d, order(year)), ]

results <- ddply(d, .(model, scenario, ensemble, month), .fun=function(d) 
  data.frame(decadal.mean=rollapply(d$value, width=10, align="right", partial=TRUE, fill=NA,
                                    by.column=FALSE, FUN=mean), year=d$year) , .progress="text")
############################################


###########################
#############################
##############################

#DECADAL MEAN

d$decade <- cut(d$year, breaks=seq(1850, 2100, 10), dig.lab=5, include.lowest=T, label=seq(1860,2100,10))

results.grouped <- group_by( d, model, scenario, ensemble, month, decade )
results <- dplyr::summarise( results.grouped, 
                             decadal.mean=mean(value))

write.csv(results, file="temperature-decadalmean.csv", row.names=F)

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
      scale_x_continuous(limits=c(1950,2100), breaks = seq(1960,2100, by=20), expand = c(0,0)) +
      scale_y_continuous(limits=c(0,0.25), breaks = seq(0,0.5, by=0.05), expand = c(0, 0)) #+
    #coord_cartesian(xlim=c(2010, 2250))+ coord_cartesian(ylim=c(0,10))
    p <- p + ylab("Decadal Monthly Mean Runoff") + 
      #  p <- p + scale_colour_manual(name="model", values=c( ) ) +      
      
      scale_color_brewer( type = "qual" , palette="Paired")+
      
      theme( legend.position= "right",
             legend.text = element_text(size = 18), 
             axis.text.x = element_text(size = 30, color="black", angle = 90, hjust=0.5, vjust=0.5), 
             axis.text.y = element_text(size = 34, color="black"),
             legend.title = element_blank(),
             plot.title = element_text(size = 56, face="bold"),
             axis.title.x = element_blank(), 
             axis.title.y = element_text(size = 36, face="bold", vjust=0.5), 
             # legend.key = element_blank(),
             #legend.title = element_blank(),
             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             panel.border  = element_rect(color="black", fill=NA))
    
    ggsave(filename = paste0(s, "_", t, ".png"),  width = 400, height = 297, units = "mm" ) 
  }
}





