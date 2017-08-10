setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa")

weather <- read.table("daily_current_sim_weather_full")

plant_silk <- read.table("current_sim_planting_silking")

planting <- plant_silk[,3]
silking <- plant_silk[,4]

####splitting pcp into sections####

###3 sections###

#the 3 sections are, planting to just before silking, silking, after silking until harvest#

median_gdd_accum <- 1389

mature <- NULL
mature_accum <- 2700-median_gdd_accum

Oct30 <- seq(303,nrow(weather),365)
for(i in 1:length(silking))
{accum_gdd_ag <- 0
for(j in silking[i]:Oct30[i])
{accum_gdd_ag<- weather$gdd_ag[j]+accum_gdd_ag
if(accum_gdd_ag>=mature_accum)
{mature[i] <- j
break}
else{mature[i]<- Oct30[i]}}
}

May1 <- seq(121,nrow(weather),365)
Aug30 <- seq(242,nrow(weather),365)

gdd_season <- NULL
for(i in 1:length(May1))
{gdd_season[i] <- sum(weather$gdd[May1[i]:Aug30[i]])}

hdd_season <- NULL
for(i in 1:length(May1))
{hdd_season[i] <- sum(weather$hdd[May1[i]:Aug30[i]])}


pcp_b_i <- NULL
for(i in 1:length(planting))
{pcp_b_i[i] <- sum(weather$pcp[planting[i]:(silking[i]-14)])}

pcp_pol_i <- NULL
for (i in 1:length(planting))
{pcp_pol_i[i] <- sum(weather$pcp[(silking[i]-13):(silking[i]+14)])}

pcp_e_i <- NULL
for (i in 1:length(planting))
{pcp_e_i[i] <- sum(weather$pcp[(silking[i]+15):mature[i]])}

vpd_b_i <- NULL
for(i in 1:length(planting))
{vpd_b_i[i] <- sum(weather$vpd[planting[i]:(silking[i]-14)])}

vpd_pol_i <- NULL
for (i in 1:length(planting))
{vpd_pol_i[i] <- sum(weather$vpd[(silking[i]-13):(silking[i]+14)])}

vpd_e_i <- NULL
for (i in 1:length(planting))
{vpd_e_i[i] <- sum(weather$vpd[(silking[i]+15):mature[i]])}

###d_iv###

#gdd, hdd, accum pcp and vpd for 3 stages#

year <- plant_silk$year
county <- plant_silk$county

#trend will be for year 2013 - the year following the last year of data - assumption is that climate has been stable from #2000 to now

trend <- rep(59,length(year))

Data_d_iv_iowa <- data.frame(year,county,trend,gdd_season,hdd_season,vpd_b_i,vpd_pol_i,vpd_e_i,pcp_b_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_d_iv_iowa,"Data_d_iv_iowa_sim_current")

