setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario")

weather <- read.table("daily_current_sim_weather_full")

plant_silk <- read.table("current_sim_planting_silking")

planting <- plant_silk[,3]
silking <- plant_silk[,4]

May1 <- seq(121,nrow(weather),365)
May31 <- seq(151,nrow(weather),365)
June1 <- seq(152,nrow(weather),365)
June30 <- seq(181,nrow(weather),365)
July1 <- seq(182,nrow(weather),365)
July31 <- seq(212,nrow(weather),365)
Aug1 <- seq(213,nrow(weather),365)
Aug31 <- seq(243,nrow(weather),365)
Sep1 <- seq(214,nrow(weather),365)
Sep30 <- seq(243,nrow(weather),365)

mean_pcp_may <- NULL
for(i in 1:length(planting))
{mean_pcp_may[i] <- sum(weather$pcp[May1[i]:May31[i]])}

mean_pcp_june <- NULL
for(i in 1:length(planting))
{mean_pcp_june[i] <- sum(weather$pcp[June1[i]:June30[i]])}

mean_pcp_july <- NULL
for(i in 1:length(planting))
{mean_pcp_july[i] <- sum(weather$pcp[July1[i]:July31[i]])}

mean_pcp_aug <- NULL
for(i in 1:length(planting))
{mean_pcp_aug[i] <- sum(weather$pcp[Aug1[i]:Aug31[i]])}

mean_vpd_may <- NULL
for(i in 1:length(planting))
{mean_vpd_may[i] <- sum(weather$vpd[May1[i]:May31[i]])}

mean_vpd_june <- NULL
for(i in 1:length(planting))
{mean_vpd_june[i] <- sum(weather$vpd[June1[i]:June30[i]])}

mean_vpd_july <- NULL
for(i in 1:length(planting))
{mean_vpd_july[i] <- sum(weather$vpd[July1[i]:July31[i]])}

mean_vpd_aug <- NULL
for(i in 1:length(planting))
{mean_vpd_aug[i] <- sum(weather$vpd[Aug1[i]:Aug31[i]])}

hdd_season <- NULL
for(i in 1:length(May1))
{hdd_season[i] <- sum(weather$hdd[May1[i]:Aug31[i]])}

gdd_season <- NULL
for(i in 1:length(May1))
{gdd_season[i] <- sum(weather$gdd[May1[i]:Aug31[i]])}

county <- plant_silk[,1]
year <- plant_silk[,2]

N=38
T=100

# year is 2014 - this is 65
time <- rep(65,N*T)

#trend is after 1999 - which is 49, so it is 16
trend <- rep(16,N*T)

  
#gdd, hdd, accum pcp and vpd for each month#

Data_d_iii_ont_sim_current <- data.frame(year,county,time,trend,gdd_season,hdd_season,mean_vpd_may,mean_vpd_june,mean_vpd_july,mean_vpd_aug,mean_pcp_may,mean_pcp_june,mean_pcp_july,mean_pcp_aug)
saveRDS(Data_d_iii_ont_sim_current,"Data_d_iii_ont_sim_current")

