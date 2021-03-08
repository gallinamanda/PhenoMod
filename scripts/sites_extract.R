# command line: Rscript --vanilla sites_extract.R nrccfile.nc variable outname.RDS

args = commandArgs(trailingOnly=TRUE)

library(raster)

tmp_raster <- brick(args[1], varname=args[2])
tmp_raster; class(tmp_raster)

sites <- read.csv("npn_sites.csv")
sites <- sites[,c(2,4,3)]
names(sites) <- c("site", "lon", "lat")
coords <- data.frame(lon=sites[,2], lat=sites[,3])
coordinates(coords) <- c("lon", "lat")

# extract min temp from these lat/lon in order...

val <- raster:::extract(x=tmp_raster, y=coords, df=TRUE)

# this produces a vector of values for the year... which is something, but what
# should we do when we have many years in the same file? also, want to add something
# that pairs this back up with lat/lon values (though this is not a huge deal, because
# they'll be in order)

# add args[4] to name the column
saveRDS(val, args[3])

work <- readRDS("out.RDS")
