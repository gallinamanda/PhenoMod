library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(zeallot)

setwd("~/Dropbox/MSB-Phenology/PhenoMod") 
sites <- read.csv("clean_data/npn_sites.csv")
sites <- sites[,c(2,4,3)]
names(sites) <- c("site", "lon", "lat")
coords <- data.frame(lon=sites[,2], lat=sites[,3])
coordinates(coords) <- c("lon", "lat")

tmin.2009.nrcc <- list.files(path="raw_data/NRCC_use/2009/tmin")
tmax.2009.nrcc <- list.files(path="raw_data/NRCC_use/2009/tmax")
tmin.2010.nrcc <- list.files(path="raw_data/NRCC_use/2010/tmin")
tmax.2010.nrcc <- list.files(path="raw_data/NRCC_use/2010/tmax")
tmin.2011.nrcc <- list.files(path="raw_data/NRCC_use/2011/tmin")
tmax.2011.nrcc <- list.files(path="raw_data/NRCC_use/2011/tmax")
tmin.2012.nrcc <- list.files(path="raw_data/NRCC_use/2012/tmin")
tmax.2012.nrcc <- list.files(path="raw_data/NRCC_use/2012/tmax")
tmin.2013.nrcc <- list.files(path="raw_data/NRCC_use/2013/tmin")
tmax.2013.nrcc <- list.files(path="raw_data/NRCC_use/2013/tmax")
tmin.2014.nrcc <- list.files(path="raw_data/NRCC_use/2014/tmin")
tmax.2014.nrcc <- list.files(path="raw_data/NRCC_use/2014/tmax")
tmin.2015.nrcc <- list.files(path="raw_data/NRCC_use/2015/tmin")
tmax.2015.nrcc <- list.files(path="raw_data/NRCC_use/2015/tmax")
tmin.2016.nrcc <- list.files(path="raw_data/NRCC_use/2016/tmin")
tmax.2016.nrcc <- list.files(path="raw_data/NRCC_use/2016/tmax")
tmin.2017.nrcc <- list.files(path="raw_data/NRCC_use/2017/tmin")
tmax.2017.nrcc <- list.files(path="raw_data/NRCC_use/2017/tmax")
tmin.2018.nrcc <- list.files(path="raw_data/NRCC_use/2018/tmin")
tmax.2018.nrcc <- list.files(path="raw_data/NRCC_use/2018/tmax")
tmin.2019.nrcc <- list.files(path="raw_data/NRCC_use/2019/tmin")
tmax.2019.nrcc <- list.files(path="raw_data/NRCC_use/2019/tmax")
tmin.2020.nrcc <- list.files(path="raw_data/NRCC_use/2020/tmin")
tmax.2020.nrcc <- list.files(path="raw_data/NRCC_use/2020/tmax")

.nrcc.unpack <- function(file_list, path, coords, year, var, doymax){
  setwd(path) 
  yearlong <- matrix(NA, nrow=3075, ncol=doymax)
  for (i in 1:length(file_list)){
    b <- brick(file_list[i], varname = var)
    # extract min temp from these lat/lon in order...
    val <- raster:::extract(x=b, y=coords, df=TRUE)
    names(val) <- c("ID", "value")
    x <- val$value
    yearlong[,i] <- x
  }
  
  yearlong <- fahrenheit.to.celsius(yearlong, round = 2)
  yearlong <- as.data.frame(yearlong)
  doy <- 1:doymax
  doy <- as.character(doy)
  #year.doy <- paste0(year,".",doy)
  
  colnames(yearlong) <- doy
  return(list(yearlong=yearlong))
}

######### run this function on each year*tmin or tmax combination

c(tmin.2009) %<-% 
  .nrcc.unpack(tmin.2009.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2009/tmin", 
               coords, 2009, "tmin", 365)

c(tmax.2009) %<-% 
  .nrcc.unpack(tmax.2009.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2009/tmax", 
               coords, 2009, "tmax", 365)

c(tmin.2010) %<-% 
  .nrcc.unpack(tmin.2010.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2010/tmin", 
               coords, 2010, "tmin", 365)

c(tmax.2010) %<-% 
  .nrcc.unpack(tmax.2010.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2010/tmax", 
               coords, 2010, "tmax", 365)

c(tmin.2011) %<-% 
  .nrcc.unpack(tmin.2011.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2011/tmin", 
               coords, 2011, "tmin", 365)

c(tmax.2011) %<-% 
  .nrcc.unpack(tmax.2011.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2011/tmax", 
               coords, 2011, "tmax", 365)

c(tmin.2012) %<-% 
  .nrcc.unpack(tmin.2012.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2012/tmin", 
               coords, 2012, "tmin", 366)

c(tmax.2012) %<-% 
  .nrcc.unpack(tmax.2012.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2012/tmax", 
               coords, 2012, "tmax", 366)

c(tmin.2013) %<-% 
  .nrcc.unpack(tmin.2013.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2013/tmin", 
               coords, 2013, "tmin", 365)

c(tmax.2013) %<-% 
  .nrcc.unpack(tmax.2013.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2013/tmax", 
               coords, 2013, "tmax", 365)

c(tmin.2014) %<-% 
  .nrcc.unpack(tmin.2014.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2014/tmin", 
               coords, 2014, "tmin", 365)

c(tmax.2014) %<-% 
  .nrcc.unpack(tmax.2014.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2014/tmax", 
               coords, 2014, "tmax", 365)

c(tmin.2015) %<-% 
  .nrcc.unpack(tmin.2015.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2015/tmin", 
               coords, 2015, "tmin", 365)

c(tmax.2015) %<-% 
  .nrcc.unpack(tmax.2015.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2015/tmax", 
               coords, 2015, "tmax", 365)

c(tmin.2016) %<-% 
  .nrcc.unpack(tmin.2016.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2016/tmin", 
               coords, 2016, "tmin", 366)

c(tmax.2016) %<-% 
  .nrcc.unpack(tmax.2016.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2016/tmax", 
               coords, 2016, "tmax", 366)

c(tmin.2017) %<-% 
  .nrcc.unpack(tmin.2017.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2017/tmin", 
               coords, 2017, "tmin", 365)

c(tmax.2017) %<-% 
  .nrcc.unpack(tmax.2017.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2017/tmax", 
               coords, 2017, "tmax", 365)

c(tmin.2018) %<-% 
  .nrcc.unpack(tmin.2018.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2018/tmin", 
               coords, 2018, "tmin", 365)

c(tmax.2018) %<-% 
  .nrcc.unpack(tmax.2018.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2018/tmax", 
               coords, 2018, "tmax", 365)

c(tmin.2019) %<-% 
  .nrcc.unpack(tmin.2019.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2019/tmin", 
               coords, 2019, "tmin", 365)

c(tmax.2019) %<-% 
  .nrcc.unpack(tmax.2019.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2019/tmax", 
               coords, 2019, "tmax", 365)

c(tmin.2020) %<-% 
  .nrcc.unpack(tmin.2020.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2020/tmin", 
               coords, 2020, "tmin", 366)

c(tmax.2020) %<-% 
  .nrcc.unpack(tmax.2020.nrcc, "~/Dropbox/MSB-Phenology/PhenoMod/raw_data/NRCC_use/2020/tmax", 
               coords, 2020, "tmax", 366)

#############################
### make some GDD objects ###
#############################

gdd_2009 <- gdd(tmax = as.matrix(tmax.2009), as.matrix(tmin.2009), 
                       tbase = 5, type = "B")
gdd_2009 <- as.data.frame(gdd_2009)

.gdd.calc <- function(tmin, tmax, year, doymax){
    gdd.yr <- matrix(NA, nrow=3075, ncol=doymax)
    for (i in 1:nrow(gdd.yr)){
      min <- as.matrix(tmin)
      max <- as.matrix(tmax)
      min <- min[i,]
      max <- max[i,]
      gdd.yr[i,] <- gdd(tmax = max, tmin = min, 
                         tbase = 5, type = "B")
    }
  
    gdd.yr <- as.data.frame(gdd.yr)
    doy <- 1:doymax
    doy <- as.character(doy)
    year.doy <- paste0(year,".",doy)
    first <- paste0(year, ".", 1)
    last <- paste0(year, ".", doymax)
    
    colnames(gdd.yr) <- year.doy
    gdd.yr <- cbind(sites[,c(3,2)], gdd.yr)
    gdd_long <- gather(gdd.yr, date, gdd, first:last, factor_key=TRUE)
    gdd_long <- as.data.frame(gdd_long)
    gdd_search <- cbind(paste0(gdd_long$lat,".", gdd_long$lon, ".", 
                               gdd_long$date), gdd_long$gdd)
    gdd_search <- as.data.frame(gdd_search)
    names(gdd_search) <- c("ID", "gdd")
    return(list(gdd_search=gdd_search))
}


c(gdd.2009) %<-% .gdd.calc(tmin.2009, tmax.2009, 2009, 365)
c(gdd.2010) %<-% .gdd.calc(tmin.2010, tmax.2010, 2010, 365)
c(gdd.2011) %<-% .gdd.calc(tmin.2011, tmax.2011, 2011, 365)
c(gdd.2012) %<-% .gdd.calc(tmin.2012, tmax.2012, 2012, 366)
c(gdd.2013) %<-% .gdd.calc(tmin.2013, tmax.2013, 2013, 365)
c(gdd.2014) %<-% .gdd.calc(tmin.2014, tmax.2014, 2014, 365)
c(gdd.2015) %<-% .gdd.calc(tmin.2015, tmax.2015, 2015, 365)
c(gdd.2016) %<-% .gdd.calc(tmin.2016, tmax.2016, 2016, 366)
c(gdd.2017) %<-% .gdd.calc(tmin.2017, tmax.2017, 2017, 365)
c(gdd.2018) %<-% .gdd.calc(tmin.2018, tmax.2018, 2018, 365)
c(gdd.2019) %<-% .gdd.calc(tmin.2019, tmax.2019, 2019, 365)
c(gdd.2020) %<-% .gdd.calc(tmin.2020, tmax.2020, 2020, 366)

all.gdd <- rbind(gdd.2009, gdd.2010, gdd.2011, gdd.2012, gdd.2013, gdd.2014, gdd.2015, gdd.2016,
      gdd.2017, gdd.2018, gdd.2019, gdd.2020)

saveRDS(all.gdd, "clean_data/gdd_for_merge.RDS")

###################################
### Test it out with maple data ###
###################################

maple_lo <- readRDS("clean_data/redmaple_lo.RDS")
maple_fl <- readRDS("clean_data/redmaple_fl.RDS")
all.gdd <- readRDS("clean_data/gdd_for_merge.RDS")

maple_lo$ID <- paste0(maple_lo$latitude,".",maple_lo$longitude,".",
                      maple_lo$first_yes_year,".",maple_lo$first_yes_doy)
maple_fl$ID <- paste0(maple_fl$latitude,".",maple_fl$longitude,".",
                      maple_fl$first_yes_year,".",maple_fl$first_yes_doy)

maple_IDs <- unique(maple_lo$ID)
all_IDs <- unique(all.gdd$ID)
all_IDs <- as.character(all_IDs)

maple_lo_gdd <- merge(maple_lo, all.gdd, by="ID")
maple_lo_gdd$gdd <- as.numeric(as.character(maple_lo_gdd$gdd))
maple_fl_gdd <- merge(maple_fl, all.gdd, by="ID")
maple_fl_gdd$gdd <- as.numeric(as.character(maple_fl_gdd$gdd))

plot(maple_lo_gdd$gdd, maple_lo_gdd$latitude)
plot(maple_fl_gdd$gdd, maple_fl_gdd$latitude)

################################### 
##### just keep first yes of yr ###
###################################

str(maple_lo_gdd)
# group by individual_id, first_yes_year
# sort by doy
# remove duplicates
library(dplyr)
library(gridExtra)

maple_LO_gdd <- maple_lo_gdd %>% 
  group_by(individual_id, first_yes_year) %>% 
  filter(first_yes_doy == min(first_yes_doy)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()

maple_LO_gdd <- as.data.frame(maple_LO_gdd)
plot(maple_LO_gdd$gdd, maple_LO_gdd$latitude, xlab="GDD", ylab="Latitude")
plot(maple_lo_gdd$gdd, maple_lo_gdd$latitude, xlab="GDD", ylab="Latitude")

######## flowering 

maple_FL_gdd <- maple_fl_gdd %>% 
  group_by(individual_id, first_yes_year) %>% 
  filter(first_yes_doy == min(first_yes_doy)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()

maple_FL_gdd <- as.data.frame(maple_FL_gdd)
plot(maple_FL_gdd$gdd, maple_FL_gdd$latitude, xlim=c(0,1000), xlab="GDD", ylab="Latitude")
plot(maple_fl_gdd$gdd, maple_fl_gdd$latitude, xlim=c(0,1000), xlab="GDD", ylab="Latitude")

############### 

annual_lo_plots <- list()
#lo_lat_plots <- list()
for (i in unique(maple_LO_gdd$first_yes_year)) {
  yr <- maple_LO_gdd[maple_LO_gdd$first_yes_year==i,]

t1 <- theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.line.x = element_line(size=.5),
        axis.line.y = element_line(size=.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12, margin=margin(0,0,0,0)),
        axis.text.x = element_text(size=12, angle = 70, hjust = 1),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12, margin=margin(5,0,0,0)),
        axis.title.y = element_text(size=12, margin=margin(0,5,0,0)))
annual_lo_plots[[i]] <-ggplot(yr, aes(x=gdd, y=latitude)) +
  geom_point(alpha=0.4) +
  labs(x="GDD", y = bquote('Latitude')) + 
  scale_x_continuous(limits = c(0, 2000)) +
  scale_y_continuous(limits = c(20,50)) +
  ggtitle(paste(as.character(i))) +
  t1
}

ggsave("LO_annual_gdd.pdf", marrangeGrob(grobs=annual_lo_plots, nrow=2, ncol=2))

