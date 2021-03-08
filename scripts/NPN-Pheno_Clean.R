dat <- readRDS("raw_data/NPN-LO_FL-08_20.RDS")

# remove records with no recent prior observations
dat <- dat[which(dat$numdays_since_prior_no >=1),]
dat <- dat[which(dat$numdays_since_prior_no <=7),]

# other cleaning!
# map to check lat/lon are in range
# check for timing outliers (e.g. winter leaf-out)

saveRDS(dat, "clean_data/NPN-LO_FL-08_20.RDS")
