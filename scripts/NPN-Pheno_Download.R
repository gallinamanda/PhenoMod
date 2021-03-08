library(rnpn)

# download data from 2008-2020 for all species, leaf out and open flowers
all.dat <- npn_download_individual_phenometrics(request_source="Gallinat",
                                                years=c(2008:2020),
                                                #species_ids = 3, # red maple
                                                phenophase_ids=c(371, 501),
                                                climate_data = FALSE)

#convert to data frame
all.dat <- as.data.frame(all.dat)

#save it out for cleaning
saveRDS(all.dat, "raw_data/NPN-LO_FL-08_20.RDS")
