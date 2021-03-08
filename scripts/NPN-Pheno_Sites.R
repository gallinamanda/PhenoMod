dat <- readRDS("clean_data/NPN-LO_FL-08_20.RDS")

#isolate site name, lat, lon
npn_sites <- dat[,c(1:3)]
#remove duplicates
npn_sites <- unique(npn_sites)
#check it out
head(npn_sites)
rownames(npn_sites) <- NULL
write.csv(npn_sites, "npn_sites.csv")
