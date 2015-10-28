# This calculates average runoff over latin america ##

coord <- read.csv("/people/link593/wrk/gcam-auto/gcam-hydro/inputs/coordinates.csv", header=F)

files <- list.files("/pic/projects/GCAM/CMIP5-data/hydro-output/", pattern="Avg_Runoff_*", full.names=T)
files <- grep(pattern=".mat", x=files, value=T)

#loop counter

first=TRUE

for (f in files){
d <- readMat(f)
df <- as.data.frame(d$q)

#### parse file name for time range....##

filename <- strsplit(f, "/")[[1]][length(strsplit(f, "/")[[1]])]
startyear <- substr ( strsplit(filename, "_")[[1]][length(strsplit(filename, "_")[[1]]) - 1 ], 1, 4 )
endyear <- substr ( strsplit(filename, "_")[[1]][length(strsplit(filename, "_")[[1]]) ], 1, 4 )

dates <- seq(as.Date(paste0(startyear, "/", startmonth, "/", "01")),
			 as.Date(paste0(endyear, "/", endmonth, "/", "01")), by="month")

names(df) <- dates

df$latitude <- coord$V3
df$longitude <- coord$V2

#subset to latin america 
df <- subset(df, latitude >= -60 & latitude <= 30 & longitude )
df <- subset(df, longitude >= -120 & longitude <= -30 & longitude )
df$latitude <- NULL
df$longitude <- NULL

# take average of latin america -- NOT weighting by grid cell area ##
d.lam <- colMeans(df)

d.lam <- as.data.frame(d.lam)
d.lam$date <- rownames(d.lam)
d.lam$year <- as.numeric(substr(d.lam$date, 1,4))
d.lam$month <- as.numeric(substr(d.lam$date, 6,7))

## very badly hacked way of parsing through filename, need alternative ## 

d.lam$model <- strsplit(filename, "_")[[1]][4]
d.lam$scenario <- strsplit(filename, "_")[[1]][length(strsplit(filename, "_")[[1]]) - 3 ]
d.lam$ensemble <- strsplit(filename, "_")[[1]][length(strsplit(filename, "_")[[1]]) - 2 ]
d.lam$variable <- "runoff"
d.lam$region <- "Latin America"

colnames(d.lam)[1] <- "value"

write.table( results, file="runoff.csv", row.names=F, sep=",", col.names=first, append=!first)

first = FALSE

}
