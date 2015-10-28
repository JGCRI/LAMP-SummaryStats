
## This script takes CMIP5 data and calculates a spatial average over S.America
## The final product is a csv file of all models by month(?).
library(RCMIP5)
library(ncdf4)
library(dplyr)


path <-  "/people/mund663/evergreen/tarred/CMIP5-temp"
areapath <- "/people/mund663/evergreen/tarred/CMIP5-temp/Landareas"
landfracpath <- "/people/mund663/evergreen/tarred/CMIP5-temp/Landfrac"
OUTPUT_DIR <- "/people/mund663/evergreen/tarred/CMIP5-temp/summary-stat/"

experiments <- c( "historical", "rcp45", "rcp85")
varlist <- c("tas")
areavar <- "areacella"
landfracvar <- "sftlf"
modellist <- c("ACCESS1-0","ACCESS1-3","BNU-ESM","CCSM4","CESM1-BGC","CESM1-CAM5","CESM1-FASTCHEM","CESM1-WACCM","CMCC-CESM",
               "CMCC-CMS","CMCC-CM","CNRM-CM5-2","CNRM-CM5","CSIRO-Mk3-6-0","CSIRO-Mk3L-1-2","FGOALS-g2","FIO-ESM",
               "GFDL-CM3","GFDL-ESM2G","GFDL-ESM2M","HadGEM2-AO","HadGEM2-CC","HadGEM2-ES","IPSL-CM5A-LR",
               "IPSL-CM5A-MR","IPSL-CM5B-LR","MIROC-ESM-CHEM","MIROC-ESM","MIROC4h","MIROC5","MPI-ESM-LR","MPI-ESM-MR",
               "MPI-ESM-P","MRI-CGCM3","MRI-ESM1","bcc-csm1-1-m","bcc-csm1-1","inmcm4","ACCESS1-0","EC-EARTH",
               "GISS-E2-H-CC","GISS-E2-H","GISS-E2-R-CC","GISS-E2-R","NorESM1-ME","NorESM1-M")
#"r1i1p1", 
ensemble_list <- c("r1i1p1", "r2i1p1", "r3i1p1", "r4i1p1", "r5i1p1", "r6i1p1", "r7i1p1", "r8i1p1", "r9i1p1", "r10i1p1")

convert_todf <- function(d, vname=d$variable, suffix="") {
  
  df <- as.data.frame(d)    
  df$units <- d$valUnit
  df$variable <- paste0(vname, suffix)
  df$scenario <- d$experiment
  df$type <- "Simulated"
  df$model <- d$model
  df
  
}

filelist <- getFileInfo(path=path )

for (e in ensemble_list){
  # loop counter
  first=T
  for (v in varlist){
    for (m in modellist){
      for (ex in experiments){
      print("================================================")
      print (v)
      print(m)
      print(ex)
      print("================================================")
      
      # load area file 
      # why does this load as a list and not as a CMIP data object? 
      larea <- loadCMIP5(variable=areavar, model=m, experiment="historical", 
                         path=areapath, ensemble="r0i0p0", domain="fx",verbose=T, recursive=F)
      
      lfrac <- loadCMIP5(variable=landfracvar, model=m, experiment="historical", 
                         path=landfracpath, ensemble="r0i0p0", domain="fx",verbose=T, recursive=T)
      
      # stop if area or landfrac file doesnt exist because cant calculate weighted avg without area
      # this script shouuld be able to calculate total area of cells, but glitchy right now
      
      if(is.null(larea)){
        next    
      }
      if(is.null(lfrac)){
        next    
      }
     
      ## only land areas
      print("calculating land only area of cell")
      larea.frac <- larea
      larea.frac$val$value <- larea$val$value * lfrac$val$value
      
      # load data file
      d <- loadCMIP5( variable=v, model=m, experiment=ex, ensemble= e, path=path, verbose=T, yearRange=c(1984, 2060))
#     d <- loadCMIP5( variable="tas", model="ACCESS1-0", experiment="rcp85", ensemble="r1i1p1", path=path, verbose=T) 
      
      if (is.null(d)) next

      # There is a problem with duplicate files in our directories that are not detected by RCMIP5
      # Until this is fixed, handle it ourselves
      if(nrow(subset(filelist,variable==v & model==m & ensemble==e & experiment==ex)) > 1) {
        print("More than one file present! Averaging")
        d$val <- group_by(d$val, lon, lat, Z, time) %>% summarise(value=mean(value))
      }
      
      ## filter to desired spatial coordinates -- Latin America for now
      print("filter temp to Latin America")
      # longitudes are in 0-360 degree with 0 being in the pacific. 
      dim_lam <- filterDimensions(d, lats=d$lat[d$lat >= -60 & d$lat <=30],
                                lons=d$lon[d$lon >= 60 & d$lon <=150])
      area_lam <- filterDimensions(larea.frac, lats=larea.frac$lat[larea.frac$lat >= -60 & larea.frac$lat <=30], 
                                 lons=larea.frac$lon[larea.frac$lon >= 60 & larea.frac$lon <=150])


      ## fix the decimal places of lat, lon variables in both area and data files
      ## they need to be identical, otherwise global avg function fails.
      print("round the lat lon numbrs")
      dim_lam$lat <- round(dim_lam$lat, digits = 2)
      dim_lam$lon <- round(dim_lam$lon, digits = 2)
      area_lam$lat <- round(area_lam$lat, digits = 2)
      area_lam$lon <- round(area_lam$lon, digits = 2)
     
      ## take spatial average
      print("take spatial average over s.america")
      lam_avg <- makeGlobalStat(dim_lam, area=area_lam, verbose=T, FUN=weighted.mean, na.rm=TRUE)
      #lam_avg <- makeGlobalStat(dim_lam, area=larea, verbose=T, FUN=weighted.mean, na.rm=TRUE)
      tas_lam <- convert_todf(lam_avg, vname=v)
	    tas_lam$ensemble <- e
      
      # write file out, append to same file as you go along
      outfnm <- paste0( OUTPUT_DIR, v, "_", ex, "_", e, ".csv" )
      write.table( tas_lam, file=outfnm, row.names=F, sep=",", col.names=first, append=!first)
      first=FALSE
	rm (d, larea, lfrac, larea.frac, dim_lam, area_lam, lam_avg, tas_lam)
gc()
    }
  }
}
}