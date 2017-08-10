setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario")

deseasonalized <- read.table("weather_current_deseasonalized_Ontario")
year1 <- read.table("weather_current_simulated_year1")

#Reseasonalizing#

mean_day <- read.table("current_mean_day")
mean_day <- mean_day[,3:ncol(mean_day)]
sd_day <- read.table("current_sd_day")
sd_day <- sd_day[,3:ncol(sd_day)]

year1_var <- year1*sd_day
year1_var_mean <- year1_var+mean_day

#organizing and naming columns correctly#

day <- deseasonalized[1:365,2]
year <- rep(1,365)

year_final1 <- data.frame(year,day,year1_var_mean)

colnames(year_final1) <- colnames(deseasonalized)

sim_current_data1 <- year_final1

for (i in 2:100)

{yeari <- read.table(paste("weather_current_simulated_year",i,sep=""))

  nam_year <- paste("year",i, sep = "")
  assign(nam_year, yeari)
  
  yeari_var <- yeari*sd_day
  yeari_var_mean <- yeari_var+mean_day

  nam_seasonalized <- paste("year_seasonalized_",i, sep = "")
  assign(nam_seasonalized, yeari_var_mean)

  year <- rep(i,365)
 
  year_finali <- data.frame(year,day,yeari_var_mean)
  
  colnames(year_finali) <- colnames(deseasonalized)

  sim_current_data <- rbind(sim_current_data1,year_finali)

  sim_current_data1 <- sim_current_data

  nam_final <- paste("year_final",i, sep = "")
  assign(nam_final, year_finali)
  
 
}

colnames(sim_current_data) <- colnames(deseasonalized)

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

col_count <- seq(3,ncol(sim_current_data),3)

dataj <- matrix(1,1,3)

for(i in 1:T)
{
 mat <- sim_current_data[sim_current_data$year==i,]

 for(j in 1:length(col_count))
{
 matj <- as.matrix(mat[,col_count[j]:(col_count[j]+2)])
 data <- rbind(dataj,matj)
 dataj <- data
}
}

daily_sim_current_weather <- data.frame(year_full,county_full,day_full,data[2:nrow(data),])

colnames(daily_sim_current_weather) <- c("year","county","day","min","max","pcp")

write.table(daily_sim_current_weather,"current_sim_stacked_daily_weather")
