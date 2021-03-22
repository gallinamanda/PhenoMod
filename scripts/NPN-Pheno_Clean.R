library(tidyr)
library(tidyverse)
library(dplyr)
library(plyr)
library(gridExtra)
library(sf)
library(leaflet)
library(mapview)
library(usmap)


dat <- readRDS("raw_data/NPN-LO_FL-08_20.RDS")

# remove records with no recent prior observations
dat <- dat[which(dat$numdays_since_prior_no >=1),]
dat <- dat[which(dat$numdays_since_prior_no <=7),]

# other cleaning!
# map to check lat/lon are in range
# check for timing outliers (e.g. winter leaf-out)

saveRDS(dat, "clean_data/NPN-LO_FL-08_20.RDS")

# leaf-out data
lo <- dat[which(dat$phenophase_id==371),]
# flowering data
fl <- dat[which(dat$phenophase_id==501),]

#lo_wide <- spread(lo, first_yes_year, first_yes_doy)
lo$latin <- paste0(lo$genus, "_", lo$species)
lo <- lo[,c("latin", "latitude", "longitude", "site_id", "first_yes_year", "first_yes_doy")]
names(lo) <- c("species", "lat", "lon", "site", "year", "doy")
lo$species <- as.factor(lo$species)
lo$site <- as.factor(lo$site)
lo$year <- as.numeric(lo$year)
lo$doy <- as.numeric(lo$doy)
str(lo)

lo_summary <- lo %>%
  group_by(species) %>%
  dplyr::summarise(count = n())

lo_summary <- as.data.frame(lo_summary)
lo_summary <- lo_summary[order(-lo_summary$count),]
top_spp <- as.data.frame(lo_summary[c(1:30),1]); names(top_spp) <- "species"

top_lo <- semi_join(lo,top_spp)

lo_spp_plots <- list()
lo_spp_maps <- list()
for (i in unique(top_lo$species)) {
  spp <- top_lo[top_lo$species==i,]
  coords <- data.frame(spp[,c(3,2)])
  rownames(coords) <- NULL
  transform <- usmap_transform(coords)
  
  # histograms of observations by year
  t1<-theme_bw() +
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
  lo_spp_plots[[i]][[i]]<-ggplot(spp, aes(year)) +
    labs(x="Year", y = bquote('Observations')) + 
    geom_histogram(binwidth = 1,
                   col="dark green", 
                   fill="green", 
                   alpha = .2) +
    scale_x_continuous(breaks=seq(2009, 2020, by=1)) +
    ggtitle(paste(as.character(i))) +
    t1
  
  # make a map of observations
  lo_spp_maps[[i]][[i]]<-plot_usmap() +
    geom_point(data = transform, aes(x = lon.1, y = lat.1),
               color = "dark green", alpha = 0.4) +
    labs(title = paste(as.character(i)))
}

lo_combo <- mapply(c, lo_spp_plots, lo_spp_maps, SIMPLIFY = FALSE)
lo_combo <- unlist(lo_combo, recursive = FALSE, use.names = TRUE)

ggsave("LO_hist_maps.pdf", marrangeGrob(grobs=lo_combo, nrow=2, ncol=1))


###
#lo_summary <- lo %>%
#  group_by(year, species) %>%
#  summarise(count = n())
#lo_summary <- as.data.frame(lo_summary)
#lo_summary <- lo_summary[order(lo_summary$species, lo_summary$year),]


#fl_wide <- spread(fl, first_yes_year, first_yes_doy)
fl$latin <- paste0(fl$genus, "_", fl$species)
fl <- fl[,c("latin", "latitude", "longitude", "site_id", "first_yes_year", "first_yes_doy")]
names(fl) <- c("species", "lat", "lon", "site", "year", "doy")
fl$species <- as.factor(fl$species)
fl$site <- as.factor(fl$site)
fl$year <- as.numeric(fl$year)
fl$doy <- as.numeric(fl$doy)
str(fl)

fl_summary <- fl %>%
  group_by(species) %>%
  dplyr::summarise(count = n())

fl_summary <- as.data.frame(fl_summary)
fl_summary <- fl_summary[order(-fl_summary$count),]
top_spp <- as.data.frame(fl_summary[c(1:30),1]); names(top_spp) <- "species"

top_fl <- semi_join(fl,top_spp)

fl_spp_plots <- list()
fl_spp_maps <- list()
for (i in unique(top_fl$species)) {
  spp <- top_fl[top_fl$species==i,]
  coords <- data.frame(spp[,c(3,2)])
  rownames(coords) <- NULL
  transform <- usmap_transform(coords)
  
  # histograms of observations by year
  t1<-theme_bw() +
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
  fl_spp_plots[[i]][[i]]<-ggplot(spp, aes(year)) +
    labs(x="Year", y = bquote('Observations')) + 
    geom_histogram(binwidth = 1,
                   col="red", 
                   fill="red", 
                   alpha = .2) +
    scale_x_continuous(breaks=seq(2009, 2020, by=1)) +
    ggtitle(paste(as.character(i))) +
    t1
  
  # make a map of observations
  fl_spp_maps[[i]][[i]]<-plot_usmap() +
    geom_point(data = transform, aes(x = lon.1, y = lat.1),
               color = "red", alpha = 0.4) +
    labs(title = paste(as.character(i)))
}

fl_combo <- mapply(c, fl_spp_plots, fl_spp_maps, SIMPLIFY = FALSE)
fl_combo <- unlist(fl_combo, recursive = FALSE, use.names = TRUE)

ggsave("FL_hist_maps.pdf", marrangeGrob(grobs=fl_combo, nrow=2, ncol=1))
