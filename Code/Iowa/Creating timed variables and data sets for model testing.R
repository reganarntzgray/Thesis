setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis")

weather <- readRDS("Data needed to complete empirical work/Daily weather data/Iowa/daily_weather_Iowa")

#####Creating variables and data sets to test different models#####

###getting yield###

Yield <- readRDS("Data needed to complete empirical work/Yield Data/Iowa/Yield_Iowa")

yield <- Yield[,3]

year <- Yield[,1]

county <- Yield[,2]

###planting and silking date###

planting <- readRDS("Data needed to complete empirical work/Planting Date data/Iowa/planting_row_estimate")
planting <- planting[,3]

silking <- readRDS("Data needed to complete empirical work/Planting Date data/Iowa/silking_row_estimate")
silking <- silking[,3]

####0####
###a###

#mean temp and pcp over growing season#

May1 <- seq(121,nrow(weather),365)
Aug30 <- seq(242,nrow(weather),365)

mean_temp_daily <- (weather$min+weather$max)/2

mean_temp_season <- NULL
for(i in 1:length(May1))
{mean_temp_season[i] <- mean(mean_temp_daily[May1[i]:Aug30[i]])}

mean_pcp_season <- NULL
for(i in 1:length(May1))
{mean_pcp_season[i] <- mean(weather$pcp[May1[i]:Aug30[i]])}

trend <- c(1:58)
Trend <- matrix(0,99,length(trend))
for (i in 1:length(trend))
{Trend[,i] <- rep(trend[i],99)}
trend <- as.vector(Trend)


Data_a_0_iowa <- data.frame(year,county,yield,trend,mean_temp_season,mean_pcp_season)
saveRDS(Data_a_0_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_0_iowa")

###b###

#mean temp, pcp and vpd over growing season#

mean_vpd_season <- NULL
for(i in 1:length(May1))
{mean_vpd_season[i] <- mean(weather$vpd[May1[i]:Aug30[i]])}

Data_b_0_iowa <- data.frame(year,county,yield,trend,mean_temp_season,mean_vpd_season,mean_pcp_season)
saveRDS(Data_b_0_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_0_iowa")

###c###

#accumulated gdd mean pcp and vpd over growing season#

gdd_season <- NULL
for(i in 1:length(May1))
{gdd_season[i] <- sum(weather$gdd[May1[i]:Aug30[i]])}

Data_c_0_iowa <- data.frame(year,county,yield,trend,gdd_season,mean_vpd_season,mean_pcp_season)
saveRDS(Data_c_0_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_0_iowa")

###d###

#gdd, hdd, mean pcp and vpd over growing season#

hdd_season <- NULL
for(i in 1:length(May1))
{hdd_season[i] <- sum(weather$hdd[May1[i]:Aug30[i]])}

Data_d_0_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,mean_vpd_season,mean_pcp_season)
saveRDS(Data_d_0_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_0_iowa")

###e###

#d plus cdd#

cdd_season <- NULL
for(i in 1:length(May1))
{cdd_season[i] <- sum(weather$cdd[May1[i]:Aug30[i]])}

Data_e_0_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,mean_vpd_season,mean_pcp_season)
saveRDS(Data_e_0_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_0_iowa")

###f###

#e with fdd subbed for cdd#

fdd_season <- NULL
for(i in 1:length(May1))
{fdd_season[i] <- sum(weather$fdd[May1[i]:Aug30[i]])}

Data_f_0_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,fdd_season,mean_vpd_season,mean_pcp_season)
saveRDS(Data_f_0_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_0_iowa")

###g###

#f with cdd as well#

Data_g_0_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,fdd_season,mean_vpd_season,mean_pcp_season)
saveRDS(Data_g_0_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_0_iowa")

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


V10 <- NULL
Oct30 <- seq(303,nrow(weather),365)
for(i in 1:length(planting))
{accum_gdd_ag <- 0
for(j in planting[i]:Oct30[i])
{accum_gdd_ag<- weather$gdd_ag[j]+accum_gdd_ag
if(accum_gdd_ag>=740)
{V10[i] <- j
break}}
}


#accumulated pcp and vpd#

pcp_pre <- NULL
pcp_pre[1]<- sum(weather$pcp[1:planting[1]])
for(i in 2:length(planting))
{pcp_pre[i] <- sum(weather$pcp[Oct30[i-1]:(planting[i])])}

pcp_b1 <- NULL
for(i in 1:length(planting))
{pcp_b1[i] <- sum(weather$pcp[planting[i]:V10[i]])}
pcp_b2 <- NULL
for(i in 1:length(planting))
{pcp_b2[i] <- sum(weather$pcp[(V10[i]+1):(silking[i]-5)])}


pcp_b <- NULL
for(i in 1:length(planting))
{pcp_b[i] <- sum(weather$pcp[planting[i]:(silking[i]-5)])}

pcp_pol <- NULL
for (i in 1:length(planting))
{pcp_pol[i] <- sum(weather$pcp[(silking[i]-4):(silking[i]+14)])}

pcp_e <- NULL
for (i in 1:length(planting))
{pcp_e[i] <- sum(weather$pcp[(silking[i]+15):mature[i]])}

vpd_pre <- NULL
vpd_pre[1]<- sum(weather$vpd[1:planting[1]])
for(i in 2:length(planting))
{vpd_pre[i] <- sum(weather$vpd[Oct30[i-1]:(planting[i])])}


vpd_b1 <- NULL
for(i in 1:length(planting))
{vpd_b1[i] <- sum(weather$vpd[planting[i]:V10[i]])}
vpd_b2 <- NULL
for(i in 1:length(planting))
{vpd_b2[i] <- sum(weather$vpd[(V10[i]+1):(silking[i]-5)])}


vpd_b <- NULL
for(i in 1:length(planting))
{vpd_b[i] <- sum(weather$vpd[planting[i]:(silking[i]-5)])}

vpd_pol <- NULL
for (i in 1:length(planting))
{vpd_pol[i] <- sum(weather$vpd[(silking[i]-4):(silking[i]+14)])}

vpd_e <- NULL
for (i in 1:length(planting))
{vpd_e[i] <- sum(weather$vpd[(silking[i]+15):mature[i]])}

#averaged pcp and vpd#

mean_pcp_pre <- NULL
mean_pcp_pre[1]<- mean(weather$pcp[1:planting[1]])
for(i in 2:length(planting))
{mean_pcp_pre[i] <- mean(weather$pcp[Oct30[i-1]:(planting[i])])}

mean_pcp_b1 <- NULL
for(i in 1:length(planting))
{mean_pcp_b1[i] <- mean(weather$pcp[planting[i]:V10[i]])}

mean_pcp_b2 <- NULL
for(i in 1:length(planting))
{mean_pcp_b2[i] <- mean(weather$pcp[(V10[i]+1):(silking[i]-5)])}


mean_pcp_b <- NULL
for(i in 1:length(planting))
{mean_pcp_b[i] <- mean(weather$pcp[planting[i]:(silking[i]-5)])}

mean_pcp_pol <- NULL
for (i in 1:length(planting))
{mean_pcp_pol[i] <- mean(weather$pcp[(silking[i]-4):(silking[i]+14)])}

mean_pcp_e <- NULL
for (i in 1:length(planting))
{mean_pcp_e[i] <- mean(weather$pcp[(silking[i]+15):mature[i]])}

mean_vpd_pre <- NULL
mean_vpd_pre[1]<- mean(weather$vpd[1:planting[1]])
for(i in 2:length(planting))
{mean_vpd_pre[i] <- mean(weather$vpd[Oct30[i-1]:(planting[i])])}


mean_vpd_b1 <- NULL
for(i in 1:length(planting))
{mean_vpd_b1[i] <- mean(weather$vpd[planting[i]:V10[i]])}

mean_vpd_b2 <- NULL
for(i in 1:length(planting))
{mean_vpd_b2[i] <- mean(weather$vpd[(V10[i]+1):(silking[i]-5)])}


mean_vpd_b <- NULL
for(i in 1:length(planting))
{mean_vpd_b[i] <- mean(weather$vpd[planting[i]:(silking[i]-5)])}

mean_vpd_pol <- NULL
for (i in 1:length(planting))
{mean_vpd_pol[i] <- mean(weather$vpd[(silking[i]-4):(silking[i]+14)])}

mean_vpd_e <- NULL
for (i in 1:length(planting))
{mean_vpd_e[i] <- mean(weather$vpd[(silking[i]+15):mature[i]])}


mean_temp_b <- NULL
for(i in 1:length(planting))
{mean_temp_b[i] <- mean(mean_temp_daily[planting[i]:(silking[i]-5)])}

mean_temp_pol <- NULL
for (i in 1:length(planting))
{mean_temp_pol[i] <- mean(mean_temp_daily[(silking[i]-4):(silking[i]+14)])}

mean_temp_e <- NULL
for (i in 1:length(planting))
{mean_temp_e[i] <- mean(mean_temp_daily[(silking[i]+15):mature[i]])}




###monthly###

May1 <- seq(121,nrow(weather),365)
May31 <- seq(151,nrow(weather),365)
June1 <- seq(152,nrow(weather),365)
June30 <- seq(181,nrow(weather),365)
July1 <- seq(182,nrow(weather),365)
July31 <- seq(212,nrow(weather),365)
Aug1 <- seq(213,nrow(weather),365)
Aug31 <- seq(243,nrow(weather),365)

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

####i####
###a_i###

#mean temp and pcp accum for each stage#

Data_a_i_iowa <- data.frame(year,county,yield,trend,mean_temp_season,pcp_b,pcp_pol,pcp_e)
saveRDS(Data_a_i_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_i_iowa")

###b_i###

#mean temp, pcp and vpd accum for 3 stages#

Data_b_i_iowa <- data.frame(year,county,yield,trend,mean_temp_season,vpd_b,vpd_pol,vpd_e,pcp_b,pcp_pol,pcp_e)
saveRDS(Data_b_i_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_i_iowa")

###c_i###

#accumulated gdd and pcp and vpd for 3 stages#

Data_c_i_iowa <- data.frame(year,county,yield,trend,gdd_season,vpd_b,vpd_pol,vpd_e,pcp_b,pcp_pol,pcp_e)
saveRDS(Data_c_i_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_i_iowa")

###d_i###

#gdd, hdd, accum pcp and vpd for 3 stages#

Data_d_i_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,vpd_b,vpd_pol,vpd_e,pcp_b,pcp_pol,pcp_e)
saveRDS(Data_d_i_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_i_iowa")


Data_d_i2_iowa <- data.frame(year,county,yield,trend,mean_temp_b,mean_temp_pol,mean_temp_e,vpd_b,vpd_pol,vpd_e,pcp_b,pcp_pol,pcp_e)
saveRDS(Data_d_i_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_i2_iowa")

###e_i###

#d plus cdd#

Data_e_i_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,vpd_b,vpd_pol,vpd_e,pcp_b,pcp_pol,pcp_e)
saveRDS(Data_e_i_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_i_iowa")

###f_i###

#e with fdd subbed for cdd#

Data_f_i_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,fdd_season,vpd_b,vpd_pol,vpd_e,pcp_b,pcp_pol,pcp_e)
saveRDS(Data_f_i_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_i_iowa")

###g_i###

#f with cdd as well#

Data_g_i_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,fdd_season,vpd_b,vpd_pol,vpd_e,pcp_b,pcp_pol,pcp_e)
saveRDS(Data_g_i_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_i_iowa")

####ii####
###a_ii###

#mean temp and mean pcp for each stage#

Data_a_ii_iowa <- data.frame(year,county,yield,trend,mean_temp_season,mean_pcp_b,mean_pcp_pol,mean_pcp_e)
saveRDS(Data_a_ii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_ii_iowa")

###b_ii###

#mean temp, pcp and vpd mean for 3 stages#

Data_b_ii_iowa <- data.frame(year,county,yield,trend,mean_temp_season,mean_vpd_b,mean_vpd_pol,mean_vpd_e,mean_pcp_b,mean_pcp_pol,mean_pcp_e)
saveRDS(Data_b_ii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_ii_iowa")

###c_ii###

#accumulated gdd and mean pcp and vpd for 3 stages#

Data_c_ii_iowa <- data.frame(year,county,yield,trend,gdd_season,mean_vpd_b,mean_vpd_pol,mean_vpd_e,mean_pcp_b,mean_pcp_pol,mean_pcp_e)
saveRDS(Data_c_ii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_ii_iowa")

###d_ii###

#gdd, hdd,mean pcp and vpd for 3 stages#

Data_d_ii_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,mean_vpd_b,mean_vpd_pol,mean_vpd_e,mean_pcp_b,mean_pcp_pol,mean_pcp_e)
saveRDS(Data_d_ii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_ii_iowa")

###e_ii###

#d plus cdd#

Data_e_ii_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,mean_vpd_b,mean_vpd_pol,mean_vpd_e,mean_pcp_b,mean_pcp_pol,mean_pcp_e)
saveRDS(Data_e_ii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_ii_iowa")

###f_ii###

#e with fdd subbed for cdd#

Data_f_ii_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,fdd_season,mean_vpd_b,mean_vpd_pol,mean_vpd_e,mean_pcp_b,mean_pcp_pol,mean_pcp_e)
saveRDS(Data_f_ii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_ii_iowa")

###g_ii###

#f with cdd as well#

Data_g_ii_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,fdd_season,mean_vpd_b,mean_vpd_pol,mean_vpd_e,mean_pcp_b,mean_pcp_pol,mean_pcp_e)
saveRDS(Data_g_ii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_ii_iowa")

####iii####
###a_iii###

#mean temp and mean pcp accum for each month#

Data_a_iii_iowa <- data.frame(year,county,yield,trend,mean_temp_season,mean_pcp_may,mean_pcp_june,mean_pcp_july,mean_pcp_aug)
saveRDS(Data_a_iii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_iii_iowa")

###b_iii###

#mean temp, pcp and vpd accum for each month#

Data_b_iii_iowa <- data.frame(year,county,yield,trend,mean_temp_season,mean_vpd_may,mean_vpd_june,mean_vpd_july,mean_vpd_aug,mean_pcp_may,mean_pcp_june,mean_pcp_july,mean_pcp_aug)
saveRDS(Data_b_iii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_iii_iowa")

###c_iii###

#accumulated gdd and pcp and vpd for each month#

Data_c_iii_iowa <- data.frame(year,county,yield,trend,gdd_season,mean_vpd_may,mean_vpd_june,mean_vpd_july,mean_vpd_aug,mean_pcp_may,mean_pcp_june,mean_pcp_july,mean_pcp_aug)
saveRDS(Data_c_iii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_iii_iowa")

###d_iii###

#gdd, hdd, accum pcp and vpd for each month#

Data_d_iii_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,mean_vpd_may,mean_vpd_june,mean_vpd_july,mean_vpd_aug,mean_pcp_may,mean_pcp_june,mean_pcp_july,mean_pcp_aug)
saveRDS(Data_d_iii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_iii_iowa")

###e_iii###

#d plus cdd#

Data_e_iii_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,mean_vpd_may,mean_vpd_june,mean_vpd_july,mean_vpd_aug,mean_pcp_may,mean_pcp_june,mean_pcp_july,mean_pcp_aug)
saveRDS(Data_e_iii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_iii_iowa")

###f_iii###

#e with fdd subbed for cdd#

Data_f_iii_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,fdd_season,mean_vpd_may,mean_vpd_june,mean_vpd_july,mean_vpd_aug,mean_pcp_may,mean_pcp_june,mean_pcp_july,mean_pcp_aug)
saveRDS(Data_f_iii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_iii_iowa")

###g_iii###

#f with cdd as well#

Data_g_iii_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,fdd_season,mean_vpd_may,mean_vpd_june,mean_vpd_july,mean_vpd_aug,mean_pcp_may,mean_pcp_june,mean_pcp_july,mean_pcp_aug)
saveRDS(Data_g_iii_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_iii_iowa")

####preseason pcp and other changes####

Data_d_i4vp_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,vpd_b1,vpd_b2,vpd_pol,vpd_e,pcp_b1,pcp_b2,pcp_pol,pcp_e)
saveRDS(Data_d_i4vp_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_i4vp_iowa")


#from model testing we know that d is the best set of options across all 4 types so we will continue with just testing d#

Data_d_ii4vp_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,mean_vpd_b1,mean_vpd_b2,mean_vpd_pol,mean_vpd_e,mean_pcp_b1,mean_pcp_b2,mean_pcp_pol,mean_pcp_e)
saveRDS(Data_d_ii4vp_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_ii4vp_iowa")

Data_d_i5vp_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,vpd_pre,vpd_b1,vpd_b2,vpd_pol,vpd_e,pcp_pre,pcp_b1,pcp_b2,pcp_pol,pcp_e)
saveRDS(Data_d_i5vp_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_i5vp_iowa")

Data_d_ii5vp_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,mean_vpd_pre,mean_vpd_b1,mean_vpd_b2,mean_vpd_pol,mean_vpd_e,mean_pcp_pre,mean_pcp_b1,mean_pcp_b2,mean_pcp_pol,mean_pcp_e)
saveRDS(Data_d_ii5vp_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_ii5vp_iowa")



####differing the lengths of the breaks between pcp and vpd changes####

pcp_pre <- NULL
pcp_pre[1]<- sum(weather$pcp[1:planting[1]])
for(i in 2:length(planting))
{pcp_pre[i] <- sum(weather$pcp[Oct30[i-1]:(planting[i])])}

pcp_b1_i <- NULL
for(i in 1:length(planting))
{pcp_b1_i[i] <- sum(weather$pcp[planting[i]:(V10[i]-10)])}
pcp_b2_i <- NULL
for(i in 1:length(planting))
{pcp_b2_i[i] <- sum(weather$pcp[(V10[i]-9):(silking[i]-14)])}


pcp_b_i <- NULL
for(i in 1:length(planting))
{pcp_b_i[i] <- sum(weather$pcp[planting[i]:(silking[i]-14)])}

pcp_pol_i <- NULL
for (i in 1:length(planting))
{pcp_pol_i[i] <- sum(weather$pcp[(silking[i]-13):(silking[i]+14)])}

pcp_e_i <- NULL
for (i in 1:length(planting))
{pcp_e_i[i] <- sum(weather$pcp[(silking[i]+15):mature[i]])}

vpd_pre <- NULL
vpd_pre[1]<- sum(weather$vpd[1:planting[1]])
for(i in 2:length(planting))
{vpd_pre[i] <- sum(weather$vpd[Oct30[i-1]:(planting[i])])}


vpd_b1_i <- NULL
for(i in 1:length(planting))
{vpd_b1_i[i] <- sum(weather$vpd[planting[i]:(V10[i]-10)])}
vpd_b2_i <- NULL
for(i in 1:length(planting))
{vpd_b2_i[i] <- sum(weather$vpd[(V10[i]-9):(silking[i]-14)])}


vpd_b_i <- NULL
for(i in 1:length(planting))
{vpd_b_i[i] <- sum(weather$vpd[planting[i]:(silking[i]-14)])}

vpd_pol_i <- NULL
for (i in 1:length(planting))
{vpd_pol_i[i] <- sum(weather$vpd[(silking[i]-13):(silking[i]+14)])}

vpd_e_i <- NULL
for (i in 1:length(planting))
{vpd_e_i[i] <- sum(weather$vpd[(silking[i]+15):mature[i]])}

####iv####
###a_iv###

#mean temp and pcp accum for each stage#

Data_a_iv_iowa <- data.frame(year,county,yield,trend,mean_temp_season,pcp_b_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_a_iv_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_iv_iowa")

###b_iv###

#mean temp, pcp and vpd accum for 3 stages#

Data_b_iv_iowa <- data.frame(year,county,yield,trend,mean_temp_season,vpd_b_i,vpd_pol_i,vpd_e_i,pcp_b_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_b_iv_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_iv_iowa")

###c_iv###

#accumulated gdd and pcp and vpd for 3 stages#

Data_c_iv_iowa <- data.frame(year,county,yield,trend,gdd_season,vpd_b_i,vpd_pol_i,vpd_e_i,pcp_b_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_c_iv_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_iv_iowa")

###d_iv###

#gdd, hdd, accum pcp and vpd for 3 stages#

Data_d_iv_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,vpd_b_i,vpd_pol_i,vpd_e_i,pcp_b_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_d_iv_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_iv_iowa")

###e_iv###

#d plus cdd#

Data_e_iv_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,vpd_b_i,vpd_pol_i,vpd_e_i,pcp_b_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_e_iv_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_iv_iowa")

###f_iv###

#e with fdd subbed for cdd#

Data_f_iv_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,fdd_season,vpd_b_i,vpd_pol_i,vpd_e_i,pcp_b_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_f_iv_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_iv_iowa")

###g_iv###

#f with cdd as well#

Data_g_iv_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,fdd_season,vpd_b_i,vpd_pol_i,vpd_e_i,pcp_b_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_g_iv_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_iv_iowa")

####v####
###a_v###

#mean temp and pcp accum for each stage#

Data_a_v_iowa <- data.frame(year,county,yield,trend,mean_temp_season,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_a_v_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_v_iowa")

###b_v###

#mean temp, pcp and vpd accum for 3 stages#

Data_b_v_iowa <- data.frame(year,county,yield,trend,mean_temp_season,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_b_v_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_v_iowa")

###c_v###

#accumulated gdd and pcp and vpd for 3 stages#

Data_c_v_iowa <- data.frame(year,county,yield,trend,gdd_season,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_c_v_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_v_iowa")

###d_v###

#gdd, hdd, accum pcp and vpd for 3 stages#

Data_d_v_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_d_v_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_v_iowa")

###e_v###

#d plus cdd#

Data_e_v_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_e_v_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_v_iowa")

###f_v###

#e with fdd subbed for cdd#

Data_f_v_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,fdd_season,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_f_v_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_v_iowa")

###g_v###

#f with cdd as well#

Data_g_v_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,fdd_season,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_g_v_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_v_iowa")

####vi####
###a_vi###

#mean temp and pcp accum for each stage#

Data_a_vi_iowa <- data.frame(year,county,yield,trend,mean_temp_season,pcp_pre,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_a_vi_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_vi_iowa")

###b_vi###

#mean temp, pcp and vpd accum for 3 stages#

Data_b_vi_iowa <- data.frame(year,county,yield,trend,mean_temp_season,vpd_pre,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_pre,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_b_vi_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_vi_iowa")

###c_vi###

#accumulated gdd and pcp and vpd for 3 stages#

Data_c_vi_iowa <- data.frame(year,county,yield,trend,gdd_season,vpd_pre,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_pre,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_c_vi_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_vi_iowa")

###d_vi###

#gdd, hdd, accum pcp and vpd for 3 stages#

Data_d_vi_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,vpd_pre,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_pre,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_d_vi_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_vi_iowa")

###e_vi###

#d plus cdd#

Data_e_vi_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,vpd_pre,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_pre,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_e_vi_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_vi_iowa")

###f_vi###

#e with fdd subbed for cdd#

Data_f_vi_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,fdd_season,vpd_pre,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_pre,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_f_vi_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_vi_iowa")

###g_vi###

#f with cdd as well#

Data_g_vi_iowa <- data.frame(year,county,yield,trend,gdd_season,hdd_season,cdd_season,fdd_season,vpd_pre,vpd_b1_i,vpd_b2_i,vpd_pol_i,vpd_e_i,pcp_pre,pcp_b1_i,pcp_b2_i,pcp_pol_i,pcp_e_i)
saveRDS(Data_g_vi_iowa,"Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_vi_iowa")

