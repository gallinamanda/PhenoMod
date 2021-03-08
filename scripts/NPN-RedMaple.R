library(ggplot2)

data <- readRDS("clean_data/NPN-LO_FL-08_20.RDS")
str(data)

rm <- data[which(data$species_id==3),]
head(rm)
rm <- rm[,c(1:4,11:12,14:17)]
rm$first_yes_year <- as.factor(rm$first_yes_year)

fl <- rm[which(rm$phenophase_id==501),]
lo <- rm[which(rm$phenophase_id==371),]

# check DOYs and remove unusual flowering dates (for now)
hist(fl$first_yes_doy)
fl <- fl[which(fl$first_yes_doy<=200),]

# check DOYs and remove unusual leaf-out dates (for now)
hist(lo$first_yes_doy)
# looks like there is a second wave of leaf-out from day ~250-320... 
# let's remove them for simplicity, for now
lo <- lo[which(lo$first_yes_doy<=200),]

# what is the high latitude red maple?
odd <- lo[order(-lo$latitude),] # a single data point from 2012 in Juneau Alaska
# let's just remove it for now since it has so much control
lo <- lo[which(lo$latitude<=55),]

# explore the red maples with some figures
# leaf out DOY by latitude for different years...
ggplot(lo, aes(x=latitude, y=first_yes_doy, color=first_yes_year)) + 
  geom_point() + theme_bw() + geom_smooth(method=lm, aes(fill=first_yes_year))
# flower DOY by latitude for different years...
ggplot(fl, aes(x=latitude, y=first_yes_doy, color=first_yes_year)) + 
  geom_point() + theme_bw() + geom_smooth(method=lm, aes(fill=first_yes_year))

# can we rearrange the data so that for single sites/individuals we have columns of year DOY?


