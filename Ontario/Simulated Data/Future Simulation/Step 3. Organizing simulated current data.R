setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario")

deseasonalized <- read.table("weather_base_deseasonalized_Ontario")
year1 <- read.table("weather_base_simulated_year1")

#Reseasonalizing#

mean_day <- read.table("base_mean_day")
mean_day <- mean_day[,3:ncol(mean_day)]
sd_day <- read.table("base_sd_day")
sd_day <- sd_day[,3:ncol(sd_day)]


year1_var <- year1*sd_day
year1_var_mean <- year1_var+mean_day

#future seasonalizing 
#ontario is in the ENA region#

DJF_temp <- 3.8
MAM_temp <- 3.5
JJA_temp <- 3.3
SON_temp <- 3.5

DJF_pcp <- 1.11
MAM_pcp <- 1.12
JJA_pcp <- 1.01
SON_pcp <- 1.07

Dec1 <- 335
Mar1 <- 60
Jun1 <- 152
Sep1 <- 244

min_col <- seq(1,ncol(year1),3)
max_col <- seq(2,ncol(year1),3)
pcp_col <- seq(3,ncol(year1),3)

year1_var_mean_f <- year1_var_mean-year1_var_mean

year1_var_mean_f[1:(Mar1-1),min_col] <- year1_var_mean[1:(Mar1-1),min_col]+DJF_temp
year1_var_mean_f[Mar1:(Jun1-1),min_col] <- year1_var_mean[Mar1:(Jun1-1),min_col]+MAM_temp
year1_var_mean_f[Jun1:(Sep1-1),min_col] <- year1_var_mean[Jun1:(Sep1-1),min_col]+JJA_temp
year1_var_mean_f[Sep1:(Dec1-1),min_col] <- year1_var_mean[Sep1:(Dec1-1),min_col]+SON_temp
year1_var_mean_f[Dec1:365,min_col] <- year1_var_mean[Dec1:365,min_col]+DJF_temp

year1_var_mean_f[1:(Mar1-1),max_col] <- year1_var_mean[1:(Mar1-1),max_col]+DJF_temp
year1_var_mean_f[Mar1:(Jun1-1),max_col] <- year1_var_mean[Mar1:(Jun1-1),max_col]+MAM_temp
year1_var_mean_f[Jun1:(Sep1-1),max_col] <- year1_var_mean[Jun1:(Sep1-1),max_col]+JJA_temp
year1_var_mean_f[Sep1:(Dec1-1),max_col] <- year1_var_mean[Sep1:(Dec1-1),max_col]+SON_temp
year1_var_mean_f[Dec1:365,max_col] <- year1_var_mean[Dec1:365,max_col]+DJF_temp

year1_var_mean_f[1:(Mar1-1),pcp_col] <- year1_var_mean[1:(Mar1-1),pcp_col]*DJF_pcp
year1_var_mean_f[Mar1:(Jun1-1),pcp_col] <- year1_var_mean[Mar1:(Jun1-1),pcp_col]*MAM_pcp
year1_var_mean_f[Jun1:(Sep1-1),pcp_col] <- year1_var_mean[Jun1:(Sep1-1),pcp_col]*JJA_pcp
year1_var_mean_f[Sep1:(Dec1-1),pcp_col] <- year1_var_mean[Sep1:(Dec1-1),pcp_col]*SON_pcp
year1_var_mean_f[Dec1:365,pcp_col] <- year1_var_mean[Dec1:365,pcp_col]*DJF_pcp


#organizing and naming columns correctly#

day <- deseasonalized[1:365,2]
year <- rep(1,365)

year_final1 <- data.frame(year,day,year1_var_mean)

colnames(year_final1) <- colnames(deseasonalized)

sim_future_data1 <- year_final1

for (i in 2:100)

{print(i)
  yeari <- read.table(paste("weather_base_simulated_year",i,sep=""))

  nam_year <- paste("year",i, sep = "")
  assign(nam_year, yeari)
  
  yeari_var <- yeari*sd_day
  yeari_var_mean <- yeari_var+mean_day
  
  min_col <- seq(1,ncol(yeari),3)
  max_col <- seq(2,ncol(yeari),3)
  pcp_col <- seq(3,ncol(yeari),3)
  
  yeari_var_mean_f <- yeari_var_mean-yeari_var_mean
  
  yeari_var_mean_f[1:(Mar1-1),min_col] <- yeari_var_mean[1:(Mar1-1),min_col]+DJF_temp
  yeari_var_mean_f[Mar1:(Jun1-1),min_col] <- yeari_var_mean[Mar1:(Jun1-1),min_col]+MAM_temp
  yeari_var_mean_f[Jun1:(Sep1-1),min_col] <- yeari_var_mean[Jun1:(Sep1-1),min_col]+JJA_temp
  yeari_var_mean_f[Sep1:(Dec1-1),min_col] <- yeari_var_mean[Sep1:(Dec1-1),min_col]+SON_temp
  yeari_var_mean_f[Dec1:365,min_col] <- yeari_var_mean[Dec1:365,min_col]+DJF_temp
  
  yeari_var_mean_f[1:(Mar1-1),max_col] <- yeari_var_mean[1:(Mar1-1),max_col]+DJF_temp
  yeari_var_mean_f[Mar1:(Jun1-1),max_col] <- yeari_var_mean[Mar1:(Jun1-1),max_col]+MAM_temp
  yeari_var_mean_f[Jun1:(Sep1-1),max_col] <- yeari_var_mean[Jun1:(Sep1-1),max_col]+JJA_temp
  yeari_var_mean_f[Sep1:(Dec1-1),max_col] <- yeari_var_mean[Sep1:(Dec1-1),max_col]+SON_temp
  yeari_var_mean_f[Dec1:365,max_col] <- yeari_var_mean[Dec1:365,max_col]+DJF_temp
  
  yeari_var_mean_f[1:(Mar1-1),pcp_col] <- yeari_var_mean[1:(Mar1-1),pcp_col]*DJF_pcp
  yeari_var_mean_f[Mar1:(Jun1-1),pcp_col] <- yeari_var_mean[Mar1:(Jun1-1),pcp_col]*MAM_pcp
  yeari_var_mean_f[Jun1:(Sep1-1),pcp_col] <- yeari_var_mean[Jun1:(Sep1-1),pcp_col]*JJA_pcp
  yeari_var_mean_f[Sep1:(Dec1-1),pcp_col] <- yeari_var_mean[Sep1:(Dec1-1),pcp_col]*SON_pcp
  yeari_var_mean_f[Dec1:365,pcp_col] <- yeari_var_mean[Dec1:365,pcp_col]*DJF_pcp

  nam_seasonalized <- paste("year_seasonalized_f_",i, sep = "")
  assign(nam_seasonalized, yeari_var_mean_f)

  year <- rep(i,365)
 
  year_finali <- data.frame(year,day,yeari_var_mean_f)

  colnames(year_finali) <- colnames(deseasonalized)
  
  sim_future_data <- rbind(sim_future_data1,year_finali)

  sim_future_data1 <- sim_future_data



  nam_final <- paste("year_final",i, sep = "")
  assign(nam_final, year_finali)
  
 
}


#creating county vector and stacking county observations to ease creation of weather variables#


weather <- readRDS("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Daily weather data/Ontario/daily_weather_ontario")

N <- 38
T <- 100

county_full <- weather[1:(365*N),3]
county_full <- rep(county_full,T)

county <- unique(county_full)

Year <- matrix(0,(365*N),T)
for (i in 1:T)
{yeari <- rep(i,(365*N))
 Year[,i]<-yeari}

year_full <- as.vector(Year)

day <- c(1:365)
day_full <- rep(day,N*T)

col_count <- seq(3,ncol(sim_future_data),3)

dataj <- matrix(1,1,3)

for(i in 1:T)
{print(i)
 mat <- sim_future_data[sim_future_data$year==i,]

 for(j in 1:length(col_count))
{
 matj <- as.matrix(mat[,col_count[j]:(col_count[j]+2)])
 data <- rbind(dataj,matj)
 dataj <- data
}
}

daily_sim_future_weather <- data.frame(year_full,county_full,day_full,data[2:nrow(data),])

colnames(daily_sim_future_weather) <- c("year","county","day","min","max","pcp")

write.table(daily_sim_future_weather,"future_sim_stacked_daily_weather")
